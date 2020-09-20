{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# language ScopedTypeVariables #-}

module Network.Modbus.Internal.RTU.Protocol
    ( -- Modbus Protocol
      ADU (..)
    , Config (..)
    , Response (..)
    , UnitId (..)
    , broadcast

    -- Parsers
    , pduParser

    -- Builders
    , aduBuilder

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
import "base" Data.Functor ( void )
import "this" Data.Range ( Range )
import "base" Data.Word ( Word8, Word16, byteSwap16 )
import "base" System.Timeout ( timeout )
import qualified "bytestring" Data.ByteString as B
import qualified "bytestring" Data.ByteString.Builder as BB
import "exceptions" Control.Monad.Catch
  ( MonadThrow, throwM, MonadCatch, catch )
import "mtl" Control.Monad.Reader ( ask )
import "transformers" Control.Monad.Trans.Class ( lift )
import "transformers" Control.Monad.Trans.Reader ( ReaderT )

import "this" Network.Modbus.Protocol
import "this" Network.Modbus.Internal.RTU.CRC16 (digest16)


-- | On MODBUS Serial Line, the Address field only contains the server address.
-- Valid  server  nodes  addresses are  in  the  range  of  0  –  247  decimal.
-- The  individual server devices are assigned addresses in the range of 1 – 247.
-- A client addresses a server by placing the server address in the address field of the message.
-- When the server returns its response, it places its own address in the response address field to let the client
-- know which server is responding
-- See: /MODBUS over serial line specification and implementation guide V1.02, section 2.3/
newtype UnitId
      = UnitId { unUnitId :: Word8 }
        deriving (Enum, Eq, Num, Ord, Read, Show)

instance Bounded UnitId where
    minBound = UnitId 0
    maxBound = UnitId 247

broadcast :: UnitId
broadcast = UnitId 0

-- See: /MODBUS over serial line specification and implementation guide V1.02, section 2.3/

data ADU
   = ADU
     { aduUnitId :: !UnitId
     , aduPdu    :: !PDU
     , aduCRC    :: !Word16
     } deriving (Eq, Show)

data RADU
   = RADU
     { raduUnitId :: !UnitId
     , raduLength :: !Word8
     , raduPdu    :: !PDU
     , raduCRC    :: !Word16
     } deriving (Eq, Show)




--------------------------------------------------------------------------------
-- Protocol objects
--------------------------------------------------------------------------------

data Response
   = Response !RADU
   | BroadcastResponse
     deriving (Eq, Show)




--------------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------------

raduParser :: AB.Parser RADU
raduParser = do
    uid    <- anyWord8
    fc     <- functionCodeParser
    len    <- anyWord8
    bs <- AB.take $ fromIntegral len
    crc <- anyWord16be
    pure RADU
         { raduUnitId = UnitId uid -- TODO force max check
         , raduLength = len
         , raduPdu    = PDU fc bs
         , raduCRC    = crc
         }

pduParser :: Word8 -> AB.Parser PDU
pduParser ln = do
    fc <- functionCodeParser
    bs <- AB.take $ fromIntegral ln
    pure PDU
         { pduFunction = fc
         , pduData     = bs
         }

--------------------------------------------------------------------------------
-- Builders
--------------------------------------------------------------------------------

aduBuilder :: ADU -> BB.Builder
aduBuilder (ADU uid pdu _) = BB.word8 (unUnitId uid) <> pduBuilder pdu <> crcBuilder uid pdu

-- CRC
-- Only the 8-bits of data in each word are used for generating the CRC.
-- First preload a register (Word16) to all 1's
-- Each 8-bit is XORed with the register and the reult is
-- shifted in the direction of the LSB, with zero filled into the MSB
-- The LSB is extracted and tested
-- If the LSB is 1 then the register is XORed with a preset value
-- if the LSB is 0 no XOR happens
-- Repeat until 8 shifts have happened
-- After the last (eighth) shift, the next 8-bit character is XORed with the
-- register's new value. The process is repeated until all the 8)bit
-- characters pass the eigth shift process.

crcBuilder :: UnitId -> PDU -> BB.Builder
crcBuilder uid pdu =
    BB.word16LE $ digest16 $ builderToByteString $ bUid <> bfn <> bPdu
  where
    bUid = BB.word8 $ unUnitId uid
    bfn = functionCodeBuilder $ pduFunction pdu
    bPdu = BB.byteString $ pduData pdu

--------------------------------------------------------------------------------
-- Executing commands
--------------------------------------------------------------------------------

-- | Sends a raw MODBUS command.
command_
    :: forall m
     . (MonadIO m, MonadCatch m)
    => UnitId
    -> FunctionCode -- ^ PDU function code.
    -> BB.Builder   -- ^ PDU data.
    -> ReaderT Config m Response
command_ uid fc dataBytes = do
    conn <- ask
    lift $ withConn conn
  where
    withConn :: Config -> m Response
    withConn cfg = go 1
      where
        go :: Int -> m Response
        go !tries =
            catch
              (command' cfg uid fc dataBytes)
              (\modbusErr ->
                bool (throwM modbusErr)
                     (go $ tries + 1)
                     (cfgRetryWhen cfg tries modbusErr)
              )

command'
    :: (MonadIO m, MonadThrow m)
    => Config
    -> UnitId
    -> FunctionCode -- ^ PDU function code.
    -> BB.Builder   -- ^ PDU data.
    -> m Response
command' cfg uid fc dataBytes = do
    mbMbResult <- liftIO $ timeout (cfgCommandTimeout cfg) $ do
      writeAllBytes cmdBytes

      if cfgEnableBroadcasts cfg && uid == broadcast
      then pure Nothing
      else do
        bs <- cfgRead cfg
        let result = AB.parse raduParser bs
        pure $ Just result

    mbResult <- maybe (throwM CommandTimeout) pure mbMbResult
    case mbResult of
      Nothing -> pure BroadcastResponse
      Just result -> do
        adu <- either (throwM . DecodeException)
                      pure
                      (AB.eitherResult result)
        if verifyCRC adu == raduCRC adu
        then do
            let pdu = raduPdu adu
            case pduFunction pdu of
                ExceptionCode rc ->
                        throwM
                            $ either DecodeException (ExceptionResponse rc)
                            $ AB.parseOnly (exceptionCodeParser <* AB.endOfInput)
                                        (pduData pdu)
                _ -> pure $ Response adu
        else throwM $ DecodeException "WrongCRC"
  where
    writeAllBytes bytes
        | B.null bytes = pure ()
        | otherwise = do
            bytesWritten <- cfgWrite cfg bytes
            writeAllBytes $ B.drop bytesWritten bytes

    cmdBytes :: B.ByteString
    cmdBytes = builderToByteString $ aduBuilder cmd

    cmd :: ADU
    cmd = ADU uid (PDU fc pduDataBytes) 0XFFFF

    pduDataBytes :: B.ByteString
    pduDataBytes = builderToByteString dataBytes

-- TODO (RvD): should return [Bool]
readCoils_
    :: (MonadIO m, MonadCatch m)
    => UnitId
    -> Range Address
    -> ReaderT Config m [Word8]
readCoils_ uid range =
    withAduData uid ReadCoils (rangeBuilder range) getW8s

-- TODO (RvD): should return [Bool]
readDiscreteInputs_
    :: (MonadIO m, MonadCatch m)
    => UnitId
    -> Range Address
    -> ReaderT Config m [Word8]
readDiscreteInputs_ uid range =
    withAduData uid ReadDiscreteInputs (rangeBuilder range) getW8s

readHoldingRegisters_
    :: (MonadIO m, MonadCatch m)
    => UnitId
    -> Range Address
    -> ReaderT Config m [Word16]
readHoldingRegisters_ uid range =
    withAduData uid ReadHoldingRegisters (rangeBuilder range) getW16s

readInputRegisters_
    :: (MonadIO m, MonadCatch m)
    => UnitId
    -> Range Address
    -> ReaderT Config m [Word16]
readInputRegisters_ uid range =
    withAduData uid ReadInputRegisters (rangeBuilder range) getW16s

writeSingleCoil_
    :: (MonadIO m, MonadCatch m)
    => UnitId
    -> Address
    -> Bool
    -> ReaderT Config m ()
writeSingleCoil_ uid addr value =
    void $ command_ uid WriteSingleCoil
                    (addressBuilder addr <> BB.word16BE value')
  where
    value' | value     = 0xff00
           | otherwise = 0x0000

writeMultipleCoils_
    :: (MonadIO m, MonadCatch m)
    => UnitId
    -> Address
    -> [Bool]
    -> ReaderT Config m ()
writeMultipleCoils_ uid startAddr values =
    void $ command_ uid WriteMultipleCoils
                    (  addressBuilder startAddr
                    <> coilsBuilder values
                    )

writeSingleRegister_
    :: (MonadIO m, MonadCatch m)
    => UnitId
    -> Address -- ^ Register address.
    -> Word16 -- ^ Register value.
    -> ReaderT Config m ()
writeSingleRegister_ uid addr value =
    void $ command_ uid WriteSingleRegister
                    (addressBuilder addr <> BB.word16BE value)

writeMultipleRegisters_
    :: (MonadIO m, MonadCatch m)
    => UnitId
    -> Address -- ^ Register starting address
    -> [Word16] -- ^ Register values to be written
    -> ReaderT Config m ()
writeMultipleRegisters_ uid startAddr values =
    withAduData uid WriteMultipleRegisters
                (  addressBuilder startAddr
                <> BB.word16BE (fromIntegral numRegs)
                <> BB.word8 (fromIntegral numRegs)
                <> mconcat (map BB.word16BE values)
                )
                ( const $ pure ())
  where
    numRegs :: Int
    numRegs = length values


-- ---------------------------------------------------------
-- -- Utils
-- --------------------------------------------------------------------------------

withAduData
    :: (MonadIO m, MonadCatch m)
    => UnitId
    -> FunctionCode
    -> BB.Builder -- ^ PDU data
    -> (Word8 -> AB.Parser a) -- ^ Parser of resulting 'aduData'
    -> ReaderT Config m a
withAduData uid fc dataBytes parser = do
    response <- command_ uid fc dataBytes
    case response of
      BroadcastResponse -> throwM BroadcastRequiresData
      Response (RADU _ len pdu _ ) ->
        either (throwM . DecodeException)
               pure
               (AB.parseOnly (parser len <* AB.endOfInput) $ pduData pdu)

verifyCRC :: RADU -> Word16
verifyCRC (RADU uid len pdu _) =
    byteSwap16 $ digest16 bs
  where
    bs = bsUid <> bsFun <> bsLen <> bsData
    bsUid = B.singleton $ unUnitId uid
    bsFun = builderToByteString $ functionCodeBuilder $ pduFunction pdu
    bsLen = B.singleton len
    bsData = pduData pdu

-- TODO (RvD): Get (V.Vector Word8)
getW8s :: Word8 -> AB.Parser [Word8]
getW8s  n = do
    bs <- AB.take (fromIntegral n)
    pure $ B.unpack bs

-- TODO (RvD): Get (V.Vector Word16)
getW16s :: Word8 -> AB.Parser [Word16]
getW16s n = replicateM (fromIntegral $ n `div` 2) anyWord16be