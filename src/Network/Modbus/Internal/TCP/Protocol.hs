{-# language BangPatterns #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language PackageImports #-}
{-# language ScopedTypeVariables #-}


{-# OPTIONS_HADDOCK not-home #-}

module Network.Modbus.Internal.TCP.Protocol
  ( TransactionId(..)
  , ProtocolId(..)
  , UnitId(..)
  , broadcast
    -- * Entity addresses
  , Address(..)
  , ToAddress(..)
    -- * Entity numbers
  , CoilNumber
  , DiscreteInputNumber
  , InputRegisterNumber
  , HoldingRegisterNumber
  , mkCoilNumber
  , mkDiscreteInputNumber
  , mkInputRegisterNumber
  , mkHoldingRegisterNumber

    -- * Protocol sessions
  , Config(..)
  , RetryPredicate

    -- * Protocol objects
  , Response(..)
  , ADU(..)
  , PDU(..)
  , Header(..)
  , TPU(..)
  , FunctionCode(..)
  , ExceptionCode(..)
  , ModbusException(..)

    -- ** Parsers
  , aduParser
  , pduParser
  , headerParser
  , functionCodeParser
  , exceptionCodeParser
  , protocolIdParser

    -- ** Builders
  , aduBuilder
  , pduBuilder
  , headerBuilder
  , functionCodeBuilder
  , exceptionCodeBuilder
  , protocolIdBuilder

    -- * Executing commands
  , command_
  , command'
  , readCoils_
  , readDiscreteInputs_
  , readHoldingRegisters_
  , readInputRegisters_
  , writeSingleCoil_
  , writeMultipleCoils_
  , writeSingleRegister_
  , writeMultipleRegisters_
  ) where

import "attoparsec" Data.Attoparsec.ByteString ( anyWord8 )
import qualified "attoparsec" Data.Attoparsec.ByteString as AB
import "base" Control.Monad ( replicateM )
import "base" Control.Monad.IO.Class ( MonadIO, liftIO )
import "base" Data.Bool ( bool )
import "base" Data.Functor ( ($>), void )
import "base" Data.Word ( Word8, Word16 )
import "base" System.Timeout ( timeout )
import qualified "bytestring" Data.ByteString as B
import qualified "bytestring" Data.ByteString.Builder as BB
import "exceptions" Control.Monad.Catch
  ( MonadThrow, throwM, MonadCatch, catch )
import "mtl" Control.Monad.Reader ( ask )
import           "this" Data.Range ( Range )
import "transformers" Control.Monad.Trans.Class ( lift )
import "transformers" Control.Monad.Trans.Reader ( ReaderT )
import "this" Network.Modbus.Protocol
--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------

-- | Server address (255 if not used).
--
-- Unit identifier is used with Modbus/TCP devices that are composites
-- of several Modbus devices, e.g. on Modbus/TCP to Modbus RTU
-- gateways. In such case, the unit identifier tells the Server Address
-- of the device behind the gateway. Natively Modbus/TCP-capable
-- devices usually ignore the Unit Identifier.
newtype UnitId
      = UnitId { unUnitId :: Word8 }
        deriving (Bounded, Enum, Eq, Num, Ord, Read, Show)

-- | The broadcast unit identifier.
--
-- __Note__: Not all devices consider this to be a broadcast! Some
-- devices consider 0 to be a distinct addressable unit. Others will
-- respond to 0 as if if where 1.
--
-- In order to send broadcast messages you have to ensure at least the following:
-- * enable broadcasts in your device (if applicable)
-- * @'connEnableBroadcasts' == True@
-- * unit identifier == 'broadcast'
broadcast :: UnitId
broadcast = UnitId 0


--------------------------------------------------------------------------------
-- Protocol objects
--------------------------------------------------------------------------------

data Response
   = Response !ADU
   | BroadcastResponse
     deriving (Eq, Show)

-- | MODBUS TCP/IP Application Data Unit
--
-- The MODBUS application data unit is built by the client that
-- initiates a MODBUS transaction. The `pduFunction` indicates to the
-- server what kind of action to perform.
--
-- __Note:__ The error check is missing. Data integrity is handled by
-- the TCP/IP layer.
--
-- See: /MODBUS Application Protocol Specification V1.1b, section 4.1/
data ADU
   = ADU
     { aduHeader :: !Header
     , aduPdu    :: !PDU
     } deriving (Eq, Show)

-- | MODBUS Application Protocol Header
--
-- See: /MODBUS Application Protocol Specification V1.1b, section 4.1/
data Header
   = Header
     { hdrTpu :: !TPU
     , hdrLength :: !Word16
       -- ^ Number of remaining bytes in this Modbus TCP frame.
     } deriving (Eq, Show)

-- | Transaction -, Protocol - and Unit identifier
data TPU
   = TPU
     { tpuTransactionId :: !TransactionId
     , tpuProtocolId    :: !ProtocolId
     , tpuUnitId        :: !UnitId
     } deriving (Eq, Ord, Show)

-- | For synchronization between messages of server & client.
--
-- The transaction identifier of a client request is mirrored in the
-- server response. This potentially allows for out-of-order messages.
newtype TransactionId
      = TransactionId { unTransactionId :: Word16 }
        deriving (Eq, Num, Ord, Read, Show)

-- | Protocol identifier. Should be 'ModbusTcp'.
data ProtocolId
   = ModbusTcp
   | OtherProtocolId !Word16
     deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------------

aduParser :: AB.Parser ADU
aduParser = do
    header <- headerParser
    pdu    <- pduParser header
    pure ADU
         { aduHeader = header
         , aduPdu    = pdu
         }

pduParser :: Header -> AB.Parser PDU
pduParser header = do
    fc <- functionCodeParser
    bs <- AB.take $ fromIntegral (hdrLength header) - 2
    pure PDU
         { pduFunction = fc
         , pduData     = bs
         }

headerParser :: AB.Parser Header
headerParser = do
    tid <- anyWord16be
    pid <- protocolIdParser
    len <- lengthParser
    uid <- anyWord8
    pure Header
         { hdrTpu =
             TPU
             { tpuTransactionId = TransactionId tid
             , tpuProtocolId    = pid
             , tpuUnitId        = UnitId uid
             }
         , hdrLength = len
         }
  where
    lengthParser :: AB.Parser Word16
    lengthParser = do
      l <- anyWord16be
      if l > 252
        then fail "length > 252"
        else pure l

protocolIdParser :: AB.Parser ProtocolId
protocolIdParser = anyWord16be <&> \case
    0 -> ModbusTcp
    n -> OtherProtocolId n

--------------------------------------------------------------------------------
-- Builders
--------------------------------------------------------------------------------

aduBuilder :: ADU -> BB.Builder
aduBuilder (ADU header pdu) = headerBuilder header <> pduBuilder pdu



headerBuilder :: Header -> BB.Builder
headerBuilder (Header (TPU (TransactionId tid) pid (UnitId uid)) len) =
    BB.word16BE tid <> protocolIdBuilder pid <> BB.word16BE len <> BB.word8 uid


protocolIdBuilder :: ProtocolId -> BB.Builder
protocolIdBuilder = BB.word16BE . enc
  where
    enc = \case
      ModbusTcp         -> 0
      OtherProtocolId n -> n

--------------------------------------------------------------------------------
-- Executing commands
--------------------------------------------------------------------------------

-- | Sends a raw MODBUS command.
command_
    :: forall m
     . (MonadIO m, MonadCatch m)
    => TPU
    -> FunctionCode -- ^ PDU function code.
    -> BB.Builder   -- ^ PDU data.
    -> ReaderT Config m Response
command_ tpu fc dataBytes = do
    conn <- ask
    lift $ withConn conn
  where
    withConn :: Config -> m Response
    withConn cfg = go 1
      where
        go :: Int -> m Response
        go !tries =
            catch
              (command' cfg tpu fc dataBytes)
              (\modbusErr ->
                bool (throwM modbusErr)
                     (go $ tries + 1)
                     (cfgRetryWhen cfg tries modbusErr)
              )

command'
    :: (MonadIO m, MonadThrow m)
    => Config
    -> TPU
    -> FunctionCode -- ^ PDU function code.
    -> BB.Builder   -- ^ PDU data.
    -> m Response
command' cfg tpu fc dataBytes = do
    mbMbResult <- liftIO $ timeout (cfgCommandTimeout cfg) $ do
      writeAllBytes cmdBytes

      if cfgEnableBroadcasts cfg && tpuUnitId tpu == broadcast
      then pure Nothing
      else do
        bs <- cfgRead cfg
        result <- AB.parseWith (cfgRead cfg) aduParser bs
        pure $ Just result

    mbResult <- maybe (throwM CommandTimeout) pure mbMbResult
    case mbResult of
      Nothing -> pure BroadcastResponse
      Just result -> do
        adu <- either (throwM . DecodeException)
                      pure
                      (AB.eitherResult result)
        let pdu = aduPdu adu
        case pduFunction pdu of
          ExceptionCode rc ->
              throwM
                $ either DecodeException (ExceptionResponse rc)
                $ AB.parseOnly (exceptionCodeParser <* AB.endOfInput)
                               (pduData pdu)
          _ -> pure $ Response adu
  where
    writeAllBytes bytes
        | B.null bytes = pure ()
        | otherwise = do
            bytesWritten <- cfgWrite cfg bytes
            writeAllBytes $ B.drop bytesWritten bytes

    cmdBytes :: B.ByteString
    cmdBytes = builderToByteString $ aduBuilder cmd

    cmd :: ADU
    cmd = ADU (Header tpu (fromIntegral $ 2 + B.length pduDataBytes))
              (PDU fc pduDataBytes)

    pduDataBytes :: B.ByteString
    pduDataBytes = builderToByteString dataBytes

-- TODO (RvD): should return [Bool]
readCoils_
    :: (MonadIO m, MonadCatch m)
    => TPU
    -> Range Address
    -> ReaderT Config m [Word8]
readCoils_ tpu range =
    withAduData tpu ReadCoils (rangeBuilder range) getW8s

-- TODO (RvD): should return [Bool]
readDiscreteInputs_
    :: (MonadIO m, MonadCatch m)
    => TPU
    -> Range Address
    -> ReaderT Config m [Word8]
readDiscreteInputs_ tpu range =
    withAduData tpu ReadDiscreteInputs (rangeBuilder range) getW8s

readHoldingRegisters_
    :: (MonadIO m, MonadCatch m)
    => TPU
    -> Range Address
    -> ReaderT Config m [Word16]
readHoldingRegisters_ tpu range =
    withAduData tpu ReadHoldingRegisters (rangeBuilder range) getW16s

readInputRegisters_
    :: (MonadIO m, MonadCatch m)
    => TPU
    -> Range Address
    -> ReaderT Config m [Word16]
readInputRegisters_ tpu range =
    withAduData tpu ReadInputRegisters (rangeBuilder range) getW16s

writeSingleCoil_
    :: (MonadIO m, MonadCatch m)
    => TPU
    -> Address
    -> Bool
    -> ReaderT Config m ()
writeSingleCoil_ tpu addr value =
    void $ command_ tpu WriteSingleCoil
                    (addressBuilder addr <> BB.word16BE value')
  where
    value' | value     = 0xff00
           | otherwise = 0x0000

writeMultipleCoils_
    :: (MonadIO m, MonadCatch m)
    => TPU
    -> Address
    -> [Bool]
    -> ReaderT Config m ()
writeMultipleCoils_ tpu startAddr values =
    void $ command_ tpu WriteMultipleCoils
                    (  addressBuilder startAddr
                    <> coilsBuilder values
                    )

writeSingleRegister_
    :: (MonadIO m, MonadCatch m)
    => TPU
    -> Address -- ^ Register address.
    -> Word16 -- ^ Register value.
    -> ReaderT Config m ()
writeSingleRegister_ tpu addr value =
    void $ command_ tpu WriteSingleRegister
                    (addressBuilder addr <> BB.word16BE value)

writeMultipleRegisters_
    :: (MonadIO m, MonadCatch m)
    => TPU
    -> Address -- ^ Register starting address
    -> [Word16] -- ^ Register values to be written
    -> ReaderT Config m ()
writeMultipleRegisters_ tpu startAddr values =
    withAduData tpu WriteMultipleRegisters
                (  addressBuilder startAddr
                <> BB.word16BE (fromIntegral numRegs)
                <> BB.word8 (fromIntegral numRegs)
                <> mconcat (map BB.word16BE values)
                )
                (anyWord16be *> anyWord16be $> ())
  where
    numRegs :: Int
    numRegs = length values

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

withAduData
    :: (MonadIO m, MonadCatch m)
    => TPU
    -> FunctionCode
    -> BB.Builder -- ^ PDU data
    -> AB.Parser a -- ^ Parser of resulting 'aduData'
    -> ReaderT Config m a
withAduData tpu fc dataBytes parser = do
    response <- command_ tpu fc dataBytes
    case response of
      BroadcastResponse -> throwM BroadcastRequiresData
      Response adu ->
        either (throwM . DecodeException)
               pure
               (AB.parseOnly (parser <* AB.endOfInput) $ pduData $ aduPdu adu)

-- TODO (RvD): Get (V.Vector Word8)
getW8s :: AB.Parser [Word8]
getW8s = do
    n <- anyWord8
    bs <- AB.take (fromIntegral n)
    pure $ B.unpack bs

-- TODO (RvD): Get (V.Vector Word16)
getW16s :: AB.Parser [Word16]
getW16s = do
    n <- anyWord8
    replicateM (fromIntegral $ n `div` 2) anyWord16be

