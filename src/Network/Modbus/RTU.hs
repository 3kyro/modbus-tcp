-- | An implementation of the Modbus RTU protocol.
--
-- This implementation is based on the @MODBUS Application Protocol
-- Specification V1.1b@
-- (<http://www.modbus.org/docs/Modbus_Application_Protocol_V1_1b.pdf>).
-- and the @MODBUS over Serial Line Specification and implementation Guide V1.02
-- (<https://www.modbus.org/docs/Modbus_over_serial_line_V1_02.pdf>).
module Network.Modbus.RTU
  ( -- * Sessions
    Session
  , runSession

  , Config(..)
  , RetryPredicate

    -- ** Workers
  , Worker
  , directWorker
  , batchWorker
  , BatchConfig(..)
  , BatchReadConfig(..)
  , defaultBatchConfig
  , defaultBatchReadConfig

    -- * Modbus Protocol
  , ExceptionCode(..)
  , ModbusException(..)
  , UnitId(..)
  , broadcast

    -- ** Entity addresses
  , Address(..)
  , ToAddress(..)

    -- ** Entity numbers
  , CoilNumber
  , DiscreteInputNumber
  , InputRegisterNumber
  , HoldingRegisterNumber
  , mkCoilNumber
  , mkDiscreteInputNumber
  , mkInputRegisterNumber
  , mkHoldingRegisterNumber

    -- * Commands
  , command
  , readCoils
  , readDiscreteInputs
  , readHoldingRegisters
  , readInputRegisters
  , writeSingleCoil
  , writeSingleRegister
  , writeMultipleRegisters
  ) where

import Network.Modbus.Protocol
import Network.Modbus.Internal.RTU.Protocol
import Network.Modbus.Internal.RTU.Batch




