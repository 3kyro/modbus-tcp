{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language PackageImports #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}

module Network.Modbus.Common.Protocol
  ( -- Entity addresses
    Address(..)
  , ToAddress(..)
    -- Entity numbers
  , CoilNumber
  , DiscreteInputNumber
  , InputRegisterNumber
  , HoldingRegisterNumber
  , mkCoilNumber
  , mkDiscreteInputNumber
  , mkInputRegisterNumber
  , mkHoldingRegisterNumber

    -- Protocol sessions
  , Config(..)
  , RetryPredicate

    -- Protocol objects
  , PDU(..)
  , FunctionCode(..)
  , ExceptionCode(..)
  , ModbusException(..)

    -- Parsers
  , functionCodeParser
  , exceptionCodeParser

    -- Builders
  , addressBuilder
  , coilsBuilder
  , functionCodeBuilder
  , pduBuilder
  , rangeBuilder
  , exceptionCodeBuilder
  , builderToByteString

    -- Utils
  ,getW8s
  ,getW16s
  ,(<&>)
  ,anyWord16be
  ) where

import "attoparsec" Data.Attoparsec.ByteString ( anyWord8 )
import qualified "attoparsec" Data.Attoparsec.ByteString as AB
import "base" Control.Monad ( replicateM )
import "base" Data.Word ( Word8, Word16 )
import "base" Data.Typeable ( Typeable )
import qualified "bytestring" Data.ByteString as B
import qualified "bytestring" Data.ByteString.Lazy as BL
import qualified "bytestring" Data.ByteString.Builder as BB
import "exceptions" Control.Monad.Catch
  ( Exception )
import           "this" Data.Range ( Range )
import qualified "this" Data.Range as Range


--------------------------------------------------------------------------------

-- | An entity address.
--
-- Identifies an entity. Entities can be coils\/discrete inputs\/input
-- registers\/holding registers.
--
-- See: /MODBUS Application Protocol Specification V1.1b, section 4.4/
newtype Address
      = Address { unAddress :: Word16 }
        deriving (Bounded, Enum, Eq, Num, Ord, Read, Show)

-- | Things that can be converted to addresses.
class ToAddress a where
    toAddress :: a -> Address

instance ToAddress Address where
    toAddress = id

--------------------------------------------------------------------------------

-- | Coil numbers start with a __zero__ and then span from __0__0001 to __0__9999
newtype CoilNumber
      = CoilNumber Address
        deriving (Eq, Ord)

-- | Discrete input numbers start with a __one__ and then span from
-- __1__0001 to __1__9999
newtype DiscreteInputNumber
      = DiscreteInputNumber Address
        deriving (Eq, Ord)

-- | Input register numbers start with a __three__ and then span from
-- __3__0001 to __3__9999
newtype InputRegisterNumber
      = InputRegisterNumber Address
        deriving (Eq, Ord)

-- | Holding register numbers start with a __four__ and then span from
-- __4__0001 to __4__9999
newtype HoldingRegisterNumber
      = HoldingRegisterNumber Address
        deriving (Eq, Ord)

instance Bounded CoilNumber where
    minBound = CoilNumber 0
    maxBound = CoilNumber 9998

instance Bounded DiscreteInputNumber where
    minBound = DiscreteInputNumber 0
    maxBound = DiscreteInputNumber 9998

instance Bounded InputRegisterNumber where
    minBound = InputRegisterNumber 0
    maxBound = InputRegisterNumber 9998

instance Bounded HoldingRegisterNumber where
    minBound = HoldingRegisterNumber 0
    maxBound = HoldingRegisterNumber 9998

deriving instance Enum CoilNumber
deriving instance Enum DiscreteInputNumber
deriving instance Enum InputRegisterNumber
deriving instance Enum HoldingRegisterNumber

instance Show CoilNumber where
    show (CoilNumber (Address n)) =
        "CoilNumber " <> show (n + 1)

instance Show DiscreteInputNumber where
    show (DiscreteInputNumber (Address n)) =
        "DiscreteInputNumber " <> show (n + 10001)

instance Show InputRegisterNumber where
    show (InputRegisterNumber (Address n)) =
        "InputRegisterNumber " <> show (n + 30001)

instance Show HoldingRegisterNumber where
    show (HoldingRegisterNumber (Address n)) =
        "HoldingRegisterNumber " <> show (n + 40001)

instance ToAddress CoilNumber where
    toAddress (CoilNumber a) = a
instance ToAddress DiscreteInputNumber where
    toAddress (DiscreteInputNumber a) = a
instance ToAddress InputRegisterNumber where
    toAddress (InputRegisterNumber a) = a
instance ToAddress HoldingRegisterNumber where
    toAddress (HoldingRegisterNumber a) = a

mkEntityNumber :: (Address -> a) -> Int -> Int -> Maybe a
mkEntityNumber fromAddress group n
    | n >= lo && n <= hi =
        Just $ fromAddress $ Address $ fromIntegral $ n - lo
    | otherwise = Nothing
  where
    lo = 1 + group * 10000
    hi = lo + 9999

mkCoilNumber :: Int -> Maybe CoilNumber
mkCoilNumber = mkEntityNumber CoilNumber 0

mkDiscreteInputNumber :: Int -> Maybe DiscreteInputNumber
mkDiscreteInputNumber = mkEntityNumber DiscreteInputNumber 1

mkInputRegisterNumber :: Int -> Maybe InputRegisterNumber
mkInputRegisterNumber = mkEntityNumber InputRegisterNumber 3

mkHoldingRegisterNumber :: Int -> Maybe HoldingRegisterNumber
mkHoldingRegisterNumber = mkEntityNumber HoldingRegisterNumber 4

--------------------------------------------------------------------------------

data Config
   = Config
     { cfgWrite :: !(B.ByteString -> IO Int)
       -- ^ Action that writes bytes.
       --
       -- You can use Network.Socket.ByteString.send applied to some
       -- socket, or some custom function.
     , cfgRead :: !(IO B.ByteString)
       -- ^ Action that reads bytes.
       --
       -- You can use Network.Socket.ByteString.recv applied to some
       -- socket, or some custom function.
     , cfgCommandTimeout :: !Int
       -- ^ Time limit in microseconds for each command.
     , cfgRetryWhen :: !RetryPredicate
       -- ^ Predicate that determines whether a failed command should
       -- be retried.
     , cfgEnableBroadcasts :: !Bool
       -- ^ When broadcasts are enabled any command send to
       -- 'broadcast' is regarded as a broadcast. No response is
       -- expected after sending a broadcast. Attempts to broadcast a
       -- command that expects a response will throw an
       -- `BroadcastRequiresData` error.
     }

-- | Predicate applied to exceptions raised while executing a command
-- in order to determine if the command should be retried.
type RetryPredicate
   =  Int -- ^ Number of tries.
   -> ModbusException
      -- ^ Exception raised by the latest attempt to execute a command.
   -> Bool
      -- ^ If 'True' the command will be retried, if 'False' the
      -- aforementioned exception will be rethrown.


--------------------------------------------------------------------------------
-- Protocol objects
--------------------------------------------------------------------------------

-- | MODBUS Protocol Data Unit
--
-- See: /MODBUS Application Protocol Specification V1.1b, section 4.1/
data PDU
   = PDU
     { pduFunction :: !FunctionCode
     , pduData     :: !B.ByteString
       -- ^ The data field of messages sent from a client to server
       -- devices contains additional information that the server uses
       -- to take the action defined by the 'pduFunction'. This can
       -- include items like discrete and register addresses, the
       -- quantity of items to be handled, and the count of actual
       -- data bytes in the field. The data field may be nonexistent
       -- (of zero length) in certain kinds of requests, in this case
       -- the server does not require any additional information. The
       -- function code alone specifies the action.
     } deriving (Eq, Show)


-- | When a message is sent from a Client to a Server device the
-- function code field tells the server what kind of action to
-- perform.
--
-- Sub-function codes are added to some function codes to define
-- multiple actions.
--
-- See: /MODBUS Application Protocol Specification V1.1b, sections 4.1 and 5/
data FunctionCode
   = -- | See: /MODBUS Application Protocol Specification V1.1b, section 6.1/
     ReadCoils
     -- | See: /MODBUS Application Protocol Specification V1.1b, section 6.2/
   | ReadDiscreteInputs
     -- | See: /MODBUS Application Protocol Specification V1.1b, section 6.3/
   | ReadHoldingRegisters
     -- | See: /MODBUS Application Protocol Specification V1.1b, section 6.4/
   | ReadInputRegisters
     -- | See: /MODBUS Application Protocol Specification V1.1b, section 6.5/
   | WriteSingleCoil
     -- | See: /MODBUS Application Protocol Specification V1.1b, section 6.6/
   | WriteSingleRegister
     -- | See: /MODBUS Application Protocol Specification V1.1b, section 6.7/
   | ReadExceptionStatus
     -- | See: /MODBUS Application Protocol Specification V1.1b, section 6.8/
   | Diagnostics
     -- | See: /MODBUS Application Protocol Specification V1.1b, section 6.9/
   | GetCommEventCounter
     -- | See: /MODBUS Application Protocol Specification V1.1b, section 6.10/
   | GetCommEventLog
     -- | See: /MODBUS Application Protocol Specification V1.1b, section 6.11/
   | WriteMultipleCoils
     -- | See: /MODBUS Application Protocol Specification V1.1b, section 6.12/
   | WriteMultipleRegisters
     -- | See: /MODBUS Application Protocol Specification V1.1b, section 6.13/
   | ReportSlaveID
     -- | See: /MODBUS Application Protocol Specification V1.1b, section 6.14/
   | ReadFileRecord
     -- | See: /MODBUS Application Protocol Specification V1.1b, section 6.15/
   | WriteFileRecord
     -- | See: /MODBUS Application Protocol Specification V1.1b, section 6.16/
   | MaskWriteRegister
     -- | See: /MODBUS Application Protocol Specification V1.1b, section 6.17/
   | ReadWriteMultipleRegisters
     -- | See: /MODBUS Application Protocol Specification V1.1b, section 6.18/
   | ReadFIFOQueue
     -- | See: /MODBUS Application Protocol Specification V1.1b, section 6.19/
   | EncapsulatedInterfaceTransport
     -- | See: /MODBUS Application Protocol Specification V1.1b, section 5/
   | UserDefinedCode Word8
     -- | See: /MODBUS Application Protocol Specification V1.1b, section 5/
   | ReservedCode Word8
   | OtherCode Word8
   | ExceptionCode FunctionCode
     deriving (Eq, Ord, Show)

-- | See: /MODBUS Application Protocol Specification V1.1b, section 7/
data ExceptionCode
   = -- | The function code received in the query is not an allowable
     -- action for the server (or slave). This may be because the
     -- function code is only applicable to newer devices, and was not
     -- implemented in the unit selected. It could also indicate that
     -- the server (or slave) is in the wrong state to process a
     -- request of this type, for example because it is unconfigured
     -- and is being asked to return register values.
     IllegalFunction
     -- | The data address received in the query is not an allowable
     -- address for the server (or slave). More specifically, the
     -- combination of reference number and transfer length is
     -- invalid. For a controller with 100 registers, the PDU addresses
     -- the first register as 0, and the last one as 99. If a request
     -- is submitted with a starting register address of 96 and a
     -- quantity of registers of 4, then this request will successfully
     -- operate (address-wise at least) on registers 96, 97, 98, 99. If
     -- a request is submitted with a starting register address of 96
     -- and a quantity of registers of 5, then this request will fail
     -- with Exception Code 0x02 \"Illegal Data Address\" since it
     -- attempts to operate on registers 96, 97, 98, 99 and 100, and
     -- there is no register with address 100.
   | IllegalDataAddress
     -- | A value contained in the query data field is not an allowable
     -- value for server (or slave). This indicates a fault in the
     -- structure of the remainder of a complex request, such as that
     -- the implied length is incorrect. It specifically does NOT mean
     -- that a data item submitted for storage in a register has a
     -- value outside the expectation of the application program, since
     -- the MODBUS protocol is unaware of the significance of any
     -- particular value of any particular register.
   | IllegalDataValue
     -- | An unrecoverable error occurred while the server (or slave)
     -- was attempting to perform the requested action.
   | SlaveDeviceFailure
     -- | Specialized use in conjunction with programming commands. The
     -- server (or slave) has accepted the request and is processing
     -- it, but a long duration of time will be required to do so. This
     -- response is returned to prevent a timeout error from occurring
     -- in the client (or master). The client (or master) can next
     -- issue a Poll Program Complete message to determine if
     -- processing is completed.
   | Acknowledge
     -- | Specialized use in conjunction with programming commands. The
     -- server (or slave) is engaged in processing a long–duration
     -- program command. The client (or master) should retransmit the
     -- message later when the server (or slave) is free.
   | SlaveDeviceBusy
     -- | Specialized use in conjunction with function codes
     -- 'ReadFileRecord' and 'WriteFileRecord' and reference type 6, to
     -- indicate that the extended file area failed to pass a
     -- consistency check.
   | MemoryParityError
     -- | Specialized use in conjunction with gateways, indicates that
     -- the gateway was unable to allocate an internal communication
     -- path from the input port to the output port for processing the
     -- request. Usually means that the gateway is misconfigured or
     -- overloaded.
   | GatewayPathUnavailable
     -- | Specialized use in conjunction with gateways, indicates that
     -- no response was obtained from the target device. Usually means
     -- that the device is not present on the network.
   | GatewayTargetDeviceFailedToRespond
     deriving (Eq, Show)

data ModbusException
   = ExceptionResponse !FunctionCode !ExceptionCode
   | DecodeException !String
   | CommandTimeout
     -- ^ A command took longer than 'connCommandTimeout'
     -- microseconds.
   | BroadcastRequiresData
     -- ^ A command that must return some data was
     -- broadcast. Broadcasts can not return data.
   | RunBatchError !String
   | OtherException !String
     deriving (Eq, Show, Typeable)

instance Exception ModbusException

--------------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------------

functionCodeParser :: AB.Parser FunctionCode
functionCodeParser = dec <$> anyWord8
  where
    dec = \case
      0x01 -> ReadCoils
      0x02 -> ReadDiscreteInputs
      0x03 -> ReadHoldingRegisters
      0x04 -> ReadInputRegisters
      0x05 -> WriteSingleCoil
      0x06 -> WriteSingleRegister
      0x07 -> ReadExceptionStatus
      0x08 -> Diagnostics
      0x0b -> GetCommEventCounter
      0x0c -> GetCommEventLog
      0x0f -> WriteMultipleCoils
      0x10 -> WriteMultipleRegisters
      0x11 -> ReportSlaveID
      0x14 -> ReadFileRecord
      0x15 -> WriteFileRecord
      0x16 -> MaskWriteRegister
      0x17 -> ReadWriteMultipleRegisters
      0x18 -> ReadFIFOQueue
      0x2b -> EncapsulatedInterfaceTransport
      code |    (code >= 0x41 && code <= 0x48)
             || (code >= 0x64 && code <= 0x6e)
             -> UserDefinedCode code
           | code `elem` [0x09, 0x0a, 0x0d, 0x0e, 0x29, 0x2a, 0x5a, 0x5b, 0x7d, 0x7e, 0x7f]
             -> ReservedCode code
           | code >= 0x80 -> ExceptionCode $ dec $ code - 0x80
           | otherwise -> OtherCode code

exceptionCodeParser :: AB.Parser ExceptionCode
exceptionCodeParser = anyWord8 >>= \case
    0x01 -> pure IllegalFunction
    0x02 -> pure IllegalDataAddress
    0x03 -> pure IllegalDataValue
    0x04 -> pure SlaveDeviceFailure
    0x05 -> pure Acknowledge
    0x06 -> pure SlaveDeviceBusy
    0x08 -> pure MemoryParityError
    0x0A -> pure GatewayPathUnavailable
    0x0B -> pure GatewayTargetDeviceFailedToRespond
    other -> fail $ "invalid ExceptionCode " <> show other


anyWord16be :: AB.Parser Word16
anyWord16be = do
    h <- fromIntegral <$> anyWord8
    l <- fromIntegral <$> anyWord8
    pure $ h * 256 + l

--------------------------------------------------------------------------------
-- Builders
--------------------------------------------------------------------------------

pduBuilder :: PDU -> BB.Builder
pduBuilder (PDU fc bs) = functionCodeBuilder fc <> BB.byteString bs

functionCodeBuilder :: FunctionCode -> BB.Builder
functionCodeBuilder = BB.word8 . enc
  where
    enc = \case
      ReadCoils                      -> 0x01
      ReadDiscreteInputs             -> 0x02
      ReadHoldingRegisters           -> 0x03
      ReadInputRegisters             -> 0x04
      WriteSingleCoil                -> 0x05
      WriteSingleRegister            -> 0x06
      ReadExceptionStatus            -> 0x07
      Diagnostics                    -> 0x08
      GetCommEventCounter            -> 0x0B
      GetCommEventLog                -> 0x0C
      WriteMultipleCoils             -> 0x0F
      WriteMultipleRegisters         -> 0x10
      ReportSlaveID                  -> 0x11
      ReadFileRecord                 -> 0x14
      WriteFileRecord                -> 0x15
      MaskWriteRegister              -> 0x16
      ReadWriteMultipleRegisters     -> 0x17
      ReadFIFOQueue                  -> 0x18
      EncapsulatedInterfaceTransport -> 0x2B
      UserDefinedCode code           -> code
      ReservedCode    code           -> code
      OtherCode       code           -> code
      ExceptionCode fc               -> 0x80 + enc fc

exceptionCodeBuilder :: ExceptionCode -> BB.Builder
exceptionCodeBuilder = BB.word8 . enc
  where
    enc = \case
      IllegalFunction                    -> 0x01

      IllegalDataAddress                 -> 0x02
      IllegalDataValue                   -> 0x03
      SlaveDeviceFailure                 -> 0x04
      Acknowledge                        -> 0x05
      SlaveDeviceBusy                    -> 0x06
      MemoryParityError                  -> 0x08
      GatewayPathUnavailable             -> 0x0A
      GatewayTargetDeviceFailedToRespond -> 0x0B

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

addressBuilder :: Address -> BB.Builder
addressBuilder = BB.word16BE . unAddress

rangeBuilder :: Range Address -> BB.Builder
rangeBuilder r =
       addressBuilder (Range.begin r)
       -- The size of the address range is not technically an
       -- address. We use the addressBuilder for convenience.
    <> addressBuilder (Range.discreteSize r)

coilsBuilder :: [Bool] -> BB.Builder
coilsBuilder bs =
       BB.word16BE (fromIntegral $ length bs)
    <> BB.word8 (fromIntegral $ length packedCoils)
    <> mconcat (map BB.word8 packedCoils)
  where
    packedCoils :: [Word8]
    packedCoils = error "todo" bs

-- TODO (RvD): Get (V.Vector Word8)
getW8s :: Word8 -> AB.Parser [Word8]
getW8s  n = do
    bs <- AB.take (fromIntegral n)
    pure $ B.unpack bs

-- TODO (RvD): Get (V.Vector Word16)
getW16s :: Word8 -> AB.Parser [Word16]
getW16s n = replicateM (fromIntegral $ n `div` 2) anyWord16be

builderToByteString :: BB.Builder -> B.ByteString
builderToByteString = BL.toStrict . BB.toLazyByteString

-- | Infix flipped 'fmap'.
--
-- @
-- ('<&>') = 'flip' 'fmap'
-- @
(<&>) :: (Functor f) => f a -> (a -> b) -> f b
as <&> f = f <$> as
{-# INLINE (<&>) #-}
infixl 1 <&>
