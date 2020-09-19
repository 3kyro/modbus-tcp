{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | An implementation of the Modbus RTU protocol.
--
-- This implementation is based on the @MODBUS Application Protocol
-- Specification V1.1b@
-- (<http://www.modbus.org/docs/Modbus_Application_Protocol_V1_1b.pdf>).
-- and the @MODBUS over Serial Line Specification and implementation Guide V1.02
-- (<https://www.modbus.org/docs/Modbus_over_serial_line_V1_02.pdf>).
module Network.Modbus.RTU


    where


import Network.Modbus.TCP.Internal.Protocol (RetryPredicate, PDU (..))

import Network.Modbus.TCP.Internal.Batch
import Data.Word (Word8)
import qualified Data.ByteString as B



