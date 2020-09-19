{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language PackageImports #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}

{-# OPTIONS_HADDOCK not-home #-}

module Network.Modbus.RTU.Internal.Batch
  ( -- * Sessions
    Session
  , runSession

    -- ** Workers
  , Worker
  , directWorker
  , batchWorker

  , BatchConfig(..)
  , BatchReadConfig(..)
  , defaultBatchConfig
  , defaultBatchReadConfig

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

import "base" Control.Monad ( when )
import "base" Control.Monad.IO.Class ( MonadIO, liftIO )
import "base" Data.Foldable ( foldl', traverse_, for_ )
import "base" Data.List ( groupBy, sortBy, intercalate )
import "base" Data.Word ( Word8, Word16 )
import qualified "bytestring" Data.ByteString.Builder as BB
import "exceptions" Control.Monad.Catch
  ( Exception, SomeException, MonadThrow, throwM, MonadCatch, catch, try )
import qualified "monad-batcher" Control.Monad.Batcher as Batcher
import           "this" Data.Range ( Range )
import qualified "this" Data.Range as Range
import "this" Network.Modbus.RTU.Internal.Protocol
import "this" Network.Modbus.Common.Protocol
import "transformers" Control.Monad.Trans.Reader ( ReaderT, runReaderT )


--------------------------------------------------------------------------------
-- Batching
--------------------------------------------------------------------------------

data Command (m :: * -> *) (r :: *) where
    CmdPerformIO :: IO a -> Command m a

    CmdThrow :: (Exception e) => e -> Command m a

    CmdCommand
        :: UnitId
        -> FunctionCode
        -> BB.Builder
        -> Command m Response

    CmdReadCoils
        :: UnitId
        -> Range Address
        -> Command m [Word8]

    CmdReadDiscreteInputs
        :: UnitId
        -> Range Address
        -> Command m [Word8]

    CmdReadHoldingRegisters
        :: UnitId
        -> Range Address
        -> Command m [Word16]

    CmdReadInputRegisters
        :: UnitId
        -> Range Address
        -> Command m [Word16]

    CmdWriteSingleCoil
        :: UnitId
        -> Address
        -> Bool
        -> Command m ()

    CmdWriteMultipleCoils
        :: UnitId
        -> Address
        -> [Bool]
        -> Command m ()

    CmdWriteSingleRegister
        :: UnitId
        -> Address
        -> Word16
        -> Command m ()

    CmdWriteMultipleRegisters
        :: UnitId
        -> Address
        -> [Word16]
        -> Command m ()

type ScheduledCommand m = Batcher.Scheduled (Command m) m

-- | Sessions combine multiple commands before they are executed.
--
-- Users are encouraged to build sessions using 'Applicative' methods
-- instead of 'Monad' methods wherever possible.
--
-- Sessions build using Applicative methods can be efficiently
-- executed by the 'batchWorker'. This worker can reason about which
-- commands are independent of each other and rewrite them in a more
-- efficient form. For instance, multiple reads of holding registers
-- can be combined into a single larger read.
--
-- With GHC >= 8.0.1 you can enable the
-- <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#applicative-do-notation ApplicativeDo>
-- language extension in order to write 'Applicative' code using
-- do-notation.
newtype Session m a
      = Session {unSession :: Batcher.Batcher (Command (ReaderT Config m)) (ReaderT Config m) a}
        deriving ( Functor
                 , Applicative
                 , Monad
                 )

instance (MonadIO m, MonadThrow m) => MonadIO (Session m) where
    liftIO = Session . Batcher.schedule . CmdPerformIO

instance (MonadIO m, MonadThrow m) => MonadThrow (Session m) where
    throwM = Session . Batcher.schedule . CmdThrow

instance (MonadIO m, MonadCatch m) => MonadCatch (Session m) where
    catch (Session b) f = Session $ Batcher.catchBatcher b (unSession . f)

type BatcherWorker m = Batcher.Worker (Command (ReaderT Config m)) (ReaderT Config m)

-- | Workers execute 'Session's.
newtype Worker m = Worker (BatcherWorker m)

-- | A batch of commands.
data Batch m
   = NotBatched !(ScheduledCommand m)
     -- ^ Trivial wrapper for commands that can not or should not be batched.
   | BatchedReadCoils            !(BatchedReads m Word8)
   | BatchedReadDiscreteInputs   !(BatchedReads m Word8)
   | BatchedReadHoldingRegisters !(BatchedReads m Word16)
   | BatchedReadInputRegisters   !(BatchedReads m Word16)
   | BatchedWriteRegisters       !(BatchedWriteRegisters m)

data BatchedReads m a
   = BatchedReads
     { batchedReadUnitId :: !UnitId
     , batchedReadRange :: !(Range Address)
     , batchedReadConsumers :: ![(Range Address, Either SomeException [a] -> m ())]
     }

-- | Batched version of 'CmdWriteSingleRegister' and 'CmdWriteMultipleRegisters'.
data BatchedWriteRegisters m
   = MkBatchedWriteRegisters
     { bwrUnitId :: !UnitId
     , bwrRange :: !(Range Address)
     , bwrValues :: ![Word16]
     , bwrOriginalWrites :: ![Either SomeException () -> m ()]
     }

-- | Configuration for the 'batchWorker'.
data BatchConfig
   = BatchConfig
     { bCfgReadCoils                :: !BatchReadConfig
     , bCfgReadDiscreteInputs       :: !BatchReadConfig
     , bCfgReadHoldingRegisters     :: !BatchReadConfig
     , bCfgReadInputRegisters       :: !BatchReadConfig
     , bCfgReorderReads             :: !Bool
     , bCfgReorderWrites            :: !Bool
     , bCfgBatchWriteSingleRegister :: !Bool
     , bCfgDebug                    :: !Bool
     }

-- | Configuration options related to the batching of read commands.
data BatchReadConfig
   = BatchReadConfig
     { brCfgEnableBatching :: !Bool
       -- ^ Whether a command should be batched at all.
     , brCfgMaxSkip :: !Word16
       -- ^ Maximum amount of addresses that may be skipped in order
       -- to batch two read commands.
       --
       -- Skipping improves performance when reading a few extra
       -- addresses is cheaper than executing a separate read command.
       --
       -- Set to 0 to disable skipping.
     }

-- | Default configuration for the 'batchWorker'.
--
-- By default only reads are reordered and batched. No addresses are
-- skipped (spurious reads).
defaultBatchConfig :: BatchConfig
defaultBatchConfig =
    BatchConfig
     { bCfgReadCoils                = defaultBatchReadConfig
     , bCfgReadDiscreteInputs       = defaultBatchReadConfig
     , bCfgReadHoldingRegisters     = defaultBatchReadConfig
     , bCfgReadInputRegisters       = defaultBatchReadConfig
     , bCfgReorderReads             = True
     , bCfgReorderWrites            = False
     , bCfgBatchWriteSingleRegister = False
     , bCfgDebug                    = False
     }

-- | Default configuration for batching of read commands.
--
-- By default batching of read commands is enabled, but no addresses
-- are skipped.
defaultBatchReadConfig :: BatchReadConfig
defaultBatchReadConfig =
    BatchReadConfig
     { brCfgEnableBatching = True
     , brCfgMaxSkip  = 0
     }

-- | Command batching state. Used as the accumulator in a fold.
data BatchState m
   = BatchState
     { bStCompletedBatches :: ![Batch m]
     , bStCurrentBatch     :: !(Maybe (Batch m))
     }

--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------

-- | Runs a 'Session' with the help of a 'Worker'.
runSession
    :: forall m a
     . (MonadIO m)
    => Worker m
    -> Config
    -> Session m a
    -> m a
runSession (Worker worker) conn (Session b) =
    runReaderT (Batcher.runBatcher worker b) conn

-- throwSessionError :: (MonadIO m, MonadThrow m) => e -> Session e m a
-- throwSessionError = Session . Batcher.schedule . CmdThrow

execCommand
    :: (MonadIO m, MonadCatch m)
    => Command (ReaderT Config m) a
    -> ReaderT Config m a
execCommand = \case
    CmdPerformIO io -> liftIO io
    CmdThrow err -> throwM err
    CmdCommand                   uid fc fdata
      -> command_                uid fc fdata
    CmdReadCoils                 uid addressRange
      -> readCoils_              uid addressRange
    CmdReadDiscreteInputs        uid addressRange
      -> readDiscreteInputs_     uid addressRange
    CmdReadHoldingRegisters      uid addressRange
      -> readHoldingRegisters_   uid addressRange
    CmdReadInputRegisters        uid addressRange
      -> readInputRegisters_     uid addressRange
    CmdWriteSingleCoil           uid addr value
      -> writeSingleCoil_        uid addr value
    CmdWriteMultipleCoils        uid addr values
      -> writeMultipleCoils_     uid addr values
    CmdWriteSingleRegister       uid addr value
      -> writeSingleRegister_    uid addr value
    CmdWriteMultipleRegisters    uid addr values
      -> writeMultipleRegisters_ uid addr values

execScheduledCommand
    :: (MonadIO m, MonadCatch m)
    => ScheduledCommand (ReaderT Config m)
    -> ReaderT Config m ()
execScheduledCommand (Batcher.Scheduled cmd write) =
    try (execCommand cmd) >>= write

-- | Directly executes all commands in the order in which they where given.
directWorker :: (MonadIO m, MonadCatch m) => Worker m
directWorker = Worker $ Batcher.simpleWorker execCommand

-- | Batches commands before execution. Can yield dramatic performance
-- improvements, but can also change the meaning of a program
-- specified as a 'Session'.
--
-- In case of doubt compare behaviour with the 'directWorker'.
--
-- Adapt the 'BatchConfig' to the semantics of the Modbus device(s) on
-- which commands are to be executed.
batchWorker :: (MonadIO m, MonadCatch m) => BatchConfig -> Worker m
batchWorker = Worker . batchWorker'

batchWorker'
    :: forall m. (MonadIO m, MonadCatch m)
    => BatchConfig
    -> BatcherWorker m
batchWorker' cfg xs = do
    when (bCfgDebug cfg) $ liftIO $ do
      putStrLn "\noriginal command block"
      for_ xs $ \(Batcher.Scheduled cmd _) -> putStrLn $ "  " <> prettyCommand cmd
      putStrLn $ "\n" <> show (length sortedGroups) <> " sorted groups"
      for_ (zip sortedGroups [1..]) $ \(grp, grpNum :: Int) -> do
        putStrLn $ "  group " <> show grpNum
        for_ grp $ \(Batcher.Scheduled cmd _) ->
          putStrLn $ "    " <> prettyCommand cmd
      putStrLn "\nbatched"
      for_ batched $ \batch ->
        putStrLn $ "  " <> prettyBatch batch
    traverse_ runBatch batched
  where
    batched :: [Batch (ReaderT Config m)]
    batched = concatMap (batchIndependentCommands cfg) sortedGroups

    sortedGroups :: [[ScheduledCommand (ReaderT Config m)]]
    sortedGroups = groupCommands cfg xs

groupCommands :: forall m. BatchConfig -> [ScheduledCommand m] -> [[ScheduledCommand m]]
groupCommands cfg xs = map (sortBy batchOrder) independentGroups
  where
    -- Group commands into independent groups. Each group may be reordered.
    independentGroups :: [[ScheduledCommand m]]
    independentGroups = groupBy independent xs

    -- | If two commands are independent their results may not depend
    -- on the execution order. This allows reordering.
    independent :: ScheduledCommand m -> ScheduledCommand m -> Bool
    independent a b =
           (bCfgReorderReads  cfg && isReadCommand  a && isReadCommand  b)
        || (bCfgReorderWrites cfg && isWriteCommand a && isWriteCommand b)

isReadCommand :: ScheduledCommand m -> Bool
isReadCommand (Batcher.Scheduled cmd _) =
    case cmd of
      CmdReadCoils            {} -> True
      CmdReadDiscreteInputs   {} -> True
      CmdReadHoldingRegisters {} -> True
      CmdReadInputRegisters   {} -> True
      _                          -> False

isWriteCommand :: ScheduledCommand m -> Bool
isWriteCommand (Batcher.Scheduled cmd _) =
    case cmd of
      CmdWriteSingleCoil        {} -> True
      CmdWriteSingleRegister    {} -> True
      CmdWriteMultipleCoils     {} -> True
      CmdWriteMultipleRegisters {} -> True
      _                            -> False

batchOrder :: ScheduledCommand m -> ScheduledCommand m -> Ordering
batchOrder (Batcher.Scheduled a _) (Batcher.Scheduled b _) =
    case (a, b) of
      (CmdPerformIO {}, CmdPerformIO {}) -> EQ
      (CmdPerformIO {}, _              ) -> LT

      (CmdThrow {}, CmdPerformIO {}) -> LT
      (CmdThrow {}, CmdThrow     {}) -> EQ
      (CmdThrow {}, _              ) -> GT

      (CmdCommand {}, CmdPerformIO {}) -> GT
      (CmdCommand {}, CmdThrow     {}) -> GT
      (CmdCommand tpuA fA _, CmdCommand tpuB fB _) ->
          compare tpuA tpuB <> compare fA fB
      (CmdCommand {}, _) -> LT

      (CmdReadCoils {}, CmdPerformIO {}) -> GT
      (CmdReadCoils {}, CmdThrow     {}) -> GT
      (CmdReadCoils {}, CmdCommand   {}) -> GT
      (CmdReadCoils tpuA rA, CmdReadCoils tpuB rB) ->
          compare tpuA tpuB <> compare rA rB
      (CmdReadCoils {}, _) -> LT

      (CmdReadDiscreteInputs {}, CmdPerformIO {}) -> GT
      (CmdReadDiscreteInputs {}, CmdThrow     {}) -> GT
      (CmdReadDiscreteInputs {}, CmdCommand   {}) -> GT
      (CmdReadDiscreteInputs {}, CmdReadCoils {}) -> GT
      (CmdReadDiscreteInputs tpuA rA, CmdReadDiscreteInputs tpuB rB) ->
          compare tpuA tpuB <> compare rA rB
      (CmdReadDiscreteInputs {}, _) -> LT

      (CmdReadHoldingRegisters {}, CmdPerformIO          {}) -> GT
      (CmdReadHoldingRegisters {}, CmdThrow              {}) -> GT
      (CmdReadHoldingRegisters {}, CmdCommand            {}) -> GT
      (CmdReadHoldingRegisters {}, CmdReadCoils          {}) -> GT
      (CmdReadHoldingRegisters {}, CmdReadDiscreteInputs {}) -> GT
      (CmdReadHoldingRegisters tpuA rA, CmdReadHoldingRegisters tpuB rB) ->
          compare tpuA tpuB <> compare rA rB
      (CmdReadHoldingRegisters {}, _) -> LT

      (CmdReadInputRegisters {}, CmdPerformIO            {}) -> GT
      (CmdReadInputRegisters {}, CmdThrow                {}) -> GT
      (CmdReadInputRegisters {}, CmdCommand              {}) -> GT
      (CmdReadInputRegisters {}, CmdReadCoils            {}) -> GT
      (CmdReadInputRegisters {}, CmdReadDiscreteInputs   {}) -> GT
      (CmdReadInputRegisters {}, CmdReadHoldingRegisters {}) -> GT
      (CmdReadInputRegisters tpuA rA, CmdReadInputRegisters tpuB rB) ->
          compare tpuA tpuB <> compare rA rB
      (CmdReadInputRegisters {}, _) -> LT

      (CmdWriteSingleCoil {}, CmdPerformIO            {}) -> GT
      (CmdWriteSingleCoil {}, CmdThrow                {}) -> GT
      (CmdWriteSingleCoil {}, CmdCommand              {}) -> GT
      (CmdWriteSingleCoil {}, CmdReadCoils            {}) -> GT
      (CmdWriteSingleCoil {}, CmdReadDiscreteInputs   {}) -> GT
      (CmdWriteSingleCoil {}, CmdReadHoldingRegisters {}) -> GT
      (CmdWriteSingleCoil {}, CmdReadInputRegisters   {}) -> GT
      (CmdWriteSingleCoil tpuA rA _, CmdWriteSingleCoil tpuB rB _) ->
          compare tpuA tpuB <> compare rA rB
      (CmdWriteSingleCoil {}, _) -> LT

      (CmdWriteMultipleCoils {}, CmdPerformIO            {}) -> GT
      (CmdWriteMultipleCoils {}, CmdThrow                {}) -> GT
      (CmdWriteMultipleCoils {}, CmdCommand              {}) -> GT
      (CmdWriteMultipleCoils {}, CmdReadCoils            {}) -> GT
      (CmdWriteMultipleCoils {}, CmdReadDiscreteInputs   {}) -> GT
      (CmdWriteMultipleCoils {}, CmdReadHoldingRegisters {}) -> GT
      (CmdWriteMultipleCoils {}, CmdReadInputRegisters   {}) -> GT
      (CmdWriteMultipleCoils {}, CmdWriteSingleCoil      {}) -> GT
      (CmdWriteMultipleCoils tpuA rA _, CmdWriteMultipleCoils tpuB rB _) ->
          compare tpuA tpuB <> compare rA rB
      (CmdWriteMultipleCoils {}, _) -> LT

      (CmdWriteSingleRegister {}, CmdPerformIO            {}) -> GT
      (CmdWriteSingleRegister {}, CmdThrow                {}) -> GT
      (CmdWriteSingleRegister {}, CmdCommand              {}) -> GT
      (CmdWriteSingleRegister {}, CmdReadCoils            {}) -> GT
      (CmdWriteSingleRegister {}, CmdReadDiscreteInputs   {}) -> GT
      (CmdWriteSingleRegister {}, CmdReadHoldingRegisters {}) -> GT
      (CmdWriteSingleRegister {}, CmdReadInputRegisters   {}) -> GT
      (CmdWriteSingleRegister {}, CmdWriteSingleCoil      {}) -> GT
      (CmdWriteSingleRegister {}, CmdWriteMultipleCoils   {}) -> GT
      (CmdWriteSingleRegister tpuA rA _, CmdWriteSingleRegister tpuB rB _) ->
          compare tpuA tpuB <> compare rA rB
      (CmdWriteSingleRegister {}, _) -> LT

      (CmdWriteMultipleRegisters {}, CmdPerformIO            {}) -> GT
      (CmdWriteMultipleRegisters {}, CmdThrow                {}) -> GT
      (CmdWriteMultipleRegisters {}, CmdCommand              {}) -> GT
      (CmdWriteMultipleRegisters {}, CmdReadCoils            {}) -> GT
      (CmdWriteMultipleRegisters {}, CmdReadDiscreteInputs   {}) -> GT
      (CmdWriteMultipleRegisters {}, CmdReadHoldingRegisters {}) -> GT
      (CmdWriteMultipleRegisters {}, CmdReadInputRegisters   {}) -> GT
      (CmdWriteMultipleRegisters {}, CmdWriteSingleCoil      {}) -> GT
      (CmdWriteMultipleRegisters {}, CmdWriteMultipleCoils   {}) -> GT
      (CmdWriteMultipleRegisters {}, CmdWriteSingleRegister  {}) -> GT
      (CmdWriteMultipleRegisters tpuA rA _, CmdWriteMultipleRegisters tpuB rB _) ->
          compare tpuA tpuB <> compare rA rB

runBatch
    :: forall m. (MonadIO m, MonadCatch m)
    => Batch (ReaderT Config m)
    -> ReaderT Config m ()
runBatch = \case
    NotBatched scheduledCmd -> execScheduledCommand scheduledCmd

    BatchedReadCoils            br -> runBatchedReads readCoils_            br
    BatchedReadDiscreteInputs   br -> runBatchedReads readDiscreteInputs_   br
    BatchedReadHoldingRegisters br -> runBatchedReads readHoldingRegisters_ br
    BatchedReadInputRegisters   br -> runBatchedReads readInputRegisters_   br

    BatchedWriteRegisters bwr -> do
        eResult <- try $ writeMultipleRegisters_ uid (Range.begin range) values
        case eResult of
          Left exc -> traverse_ ($ Left exc) originalWrites
          Right () -> traverse_ ($ Right ()) originalWrites
      where
        MkBatchedWriteRegisters uid range values originalWrites = bwr

runBatchedReads
    :: forall m a. (MonadIO m, MonadCatch m)
    => (UnitId -> Range Address -> ReaderT Config m [a])
    -> BatchedReads (ReaderT Config m) a
    -> ReaderT Config m ()
runBatchedReads readCommand br = do
    eResult <- try $ readCommand uid batchedRange
    case eResult of
      Left exc ->
        traverse_ (\(_, write) -> write $ Left exc) consumers
      Right batchedData ->
        traverse_ (processConsumer batchedData) consumers
  where
    BatchedReads uid batchedRange consumers = br

    processConsumer
        :: [a]
        -> (Range Address, Either SomeException [a] -> ReaderT Config m ())
        -> ReaderT Config m ()
    processConsumer batchedData (orgRange, write) = do
        when (Range.gap batchedRange orgRange /= 0) $
          throwM $ RunBatchError errMsg
        write $ Right
              $ take (fromIntegral $ unAddress $ Range.discreteSize orgRange)
              $ drop (fromIntegral offset) batchedData
      where
        offset = unAddress (Range.begin orgRange) - unAddress (Range.begin batchedRange)
        errMsg =
               "BatchedReads: original request "
            <> prettyAddressRange orgRange
            <> " not fully contained in batched request "
            <> prettyAddressRange batchedRange


initBatchState :: BatchState m
initBatchState =
    BatchState
    { bStCompletedBatches = []
    , bStCurrentBatch     = Nothing
    }

extractBatches :: BatchState m -> [Batch m]
extractBatches bSt =
    reverse $ maybe id (:) (bStCurrentBatch bSt) $ bStCompletedBatches bSt

batchIndependentCommands
    :: forall m
     . BatchConfig
    -> [ScheduledCommand m]
    -> [Batch m]
batchIndependentCommands cfg = extractBatches . foldl' processCmd initBatchState
  where
    processCmd :: BatchState m -> ScheduledCommand m -> BatchState m
    processCmd bSt scheduledCmd =
        case bStCurrentBatch bSt of
          -- No batch in progress. Convert the scheduled command to a batch.
          Nothing -> bSt{bStCurrentBatch = Just (initBatch cfg scheduledCmd)}
          -- Try to grow the current batch using the scheduled command.
          Just curBatch ->
            case growBatch cfg curBatch scheduledCmd of
              -- The scheduled command couldn't be added to the current
              -- batch. Move the current batch to the list of completed batches
              -- and proceed with a fresh batch constructed from the scheduled
              -- command.
              Nothing ->
                  bSt{ bStCompletedBatches = curBatch : bStCompletedBatches bSt
                     , bStCurrentBatch = Just (initBatch cfg scheduledCmd)
                     }
              -- Success! Proceed with the new batch and updated context.
              Just newBatch -> bSt{bStCurrentBatch = Just newBatch}

-- Combine the next scheduled command with the current batched command, if
-- possible.
growBatch
    :: forall m
     . BatchConfig
    -> Batch m
    -> ScheduledCommand m
    -> Maybe (Batch m)
growBatch _cfg NotBatched {} _ = Nothing
growBatch cfg (BatchedReadCoils br) (Batcher.Scheduled cmd write) =
    case cmd of
      CmdReadCoils uid range ->
        BatchedReadCoils <$>
          growReadBatch (bCfgReadCoils cfg) br uid range write
      _ -> Nothing
growBatch cfg (BatchedReadDiscreteInputs br) (Batcher.Scheduled cmd write) =
    case cmd of
      CmdReadDiscreteInputs uid range ->
        BatchedReadDiscreteInputs <$>
          growReadBatch (bCfgReadDiscreteInputs cfg) br uid range write
      _ -> Nothing
growBatch cfg (BatchedReadHoldingRegisters br) (Batcher.Scheduled cmd write) =
    case cmd of
      CmdReadHoldingRegisters uid range ->
        BatchedReadHoldingRegisters <$>
          growReadBatch (bCfgReadHoldingRegisters cfg) br uid range write
      _ -> Nothing
growBatch cfg (BatchedReadInputRegisters br) (Batcher.Scheduled cmd write) =
    case cmd of
      CmdReadInputRegisters uid range ->
        BatchedReadInputRegisters <$>
          growReadBatch (bCfgReadInputRegisters cfg) br uid range write
      _ -> Nothing
growBatch _cfg (BatchedWriteRegisters bwr) (Batcher.Scheduled cmd write) =
    case cmd of
      CmdWriteSingleRegister uid address value  ->
          batchWrite bwr write uid address [value]
      CmdWriteMultipleRegisters uid address values ->
          batchWrite bwr write uid address values
      _ -> Nothing

growReadBatch
    :: BatchReadConfig
    -> BatchedReads m a
    -> UnitId
    -> Range Address
    -> (Either SomeException [a] -> m ())
    -> Maybe (BatchedReads m a)
growReadBatch brCfg br uid range write =
    if canBatch
    then Just batched
    else Nothing
  where
    canBatch =
           uid == batchedReadUnitId br
        && Range.begin range >= Range.begin batchedRange
        && skipped <= brCfgMaxSkip brCfg

    batched =
        br{ batchedReadRange = batchedRange <> range
          , batchedReadConsumers = (range, write) : batchedReadConsumers br
          }

    skipped = unAddress $ Range.gap batchedRange range
    batchedRange = batchedReadRange br

batchWrite
    :: BatchedWriteRegisters m
    -> (Either SomeException () -> m ())
    -> UnitId
    -> Address
    -> [Word16]
    -> Maybe (Batch m)
batchWrite bwr write uid address values
    | canBatch  = Just batched
    | otherwise = Nothing
  where
    batched = BatchedWriteRegisters $
              bwr{ bwrRange = batchedRange <> Range.singleton address
                   -- TODO (RvD): list append anti pattern (use difference list?)
                 , bwrValues = bwrValues bwr <> values
                 , bwrOriginalWrites = write : bwrOriginalWrites bwr
                 }

    canBatch =
           uid == bwrUnitId bwr
        && address >= Range.begin batchedRange
        && Range.gap batchedRange writeRange == 0

    writeRange = Range.fromSize address (Address . fromIntegral $ length values)
    batchedRange = bwrRange bwr

initBatch :: BatchConfig -> ScheduledCommand m -> Batch m
initBatch cfg scheduled@(Batcher.Scheduled cmd write) =
    case cmd of
      CmdReadCoils uid range | brCfgEnableBatching (bCfgReadCoils cfg) ->
        BatchedReadCoils $
          initReadBatch uid range write
      CmdReadDiscreteInputs uid range | brCfgEnableBatching (bCfgReadDiscreteInputs cfg) ->
        BatchedReadDiscreteInputs $
          initReadBatch uid range write
      CmdReadHoldingRegisters uid range | brCfgEnableBatching (bCfgReadHoldingRegisters cfg) ->
        BatchedReadHoldingRegisters $
          initReadBatch uid range write
      CmdReadInputRegisters uid range | brCfgEnableBatching (bCfgReadInputRegisters cfg) ->
        BatchedReadInputRegisters $
          initReadBatch uid range write
      CmdWriteSingleRegister uid address value | bCfgBatchWriteSingleRegister cfg ->
        BatchedWriteRegisters
        MkBatchedWriteRegisters
        { bwrUnitId = uid
        , bwrRange = Range.singleton address
        , bwrValues = [value]
        , bwrOriginalWrites = [write]
        }
      CmdWriteMultipleRegisters uid address values ->
        BatchedWriteRegisters
        MkBatchedWriteRegisters
        { bwrUnitId = uid
        , bwrRange = Range.fromSize address (Address . fromIntegral $ length values)
        , bwrValues = values
        , bwrOriginalWrites = [write]
        }
      _ -> NotBatched scheduled

initReadBatch
    :: UnitId
    -> Range Address
    -> (Either SomeException [a] -> m ())
    -> BatchedReads m a
initReadBatch uid range write =
    BatchedReads
    { batchedReadUnitId = uid
    , batchedReadRange = range
    , batchedReadConsumers = [(range, write)]
    }


-- | Sends a raw MODBUS command.
command
    :: (MonadIO m, MonadThrow m)
    => UnitId
    -> FunctionCode -- ^ PDU function code.
    -> BB.Builder -- ^ PDU data.
    -> Session m Response
command uid fc fdata =
    Session $ Batcher.schedule $ CmdCommand uid fc fdata

readCoils
    :: (MonadIO m, MonadThrow m)
    => UnitId
    -> Range Address
    -> Session m [Word8]
readCoils uid range =
    Session $ Batcher.schedule $ CmdReadCoils uid range

readDiscreteInputs
    :: (MonadIO m, MonadThrow m)
    => UnitId
    -> Range Address
    -> Session m [Word8]
readDiscreteInputs uid range =
    Session $ Batcher.schedule $ CmdReadDiscreteInputs uid range

readHoldingRegisters
    :: (MonadIO m, MonadThrow m)
    => UnitId
    -> Range Address
    -> Session m [Word16]
readHoldingRegisters uid range =
    Session $ Batcher.schedule $ CmdReadHoldingRegisters uid range

readInputRegisters
    :: (MonadIO m, MonadThrow m)
    => UnitId
    -> Range Address
    -> Session m [Word16]
readInputRegisters uid range =
    Session $ Batcher.schedule $ CmdReadInputRegisters uid range

writeSingleCoil
    :: (MonadIO m, MonadThrow m)
    => UnitId
    -> Address
    -> Bool
    -> Session m ()
writeSingleCoil uid addr value =
    Session $ Batcher.schedule $ CmdWriteSingleCoil uid addr value

writeSingleRegister
    :: (MonadIO m, MonadThrow m)
    => UnitId
    -> Address
    -> Word16 -- ^ Register value.
    -> Session m ()
writeSingleRegister uid addr value =
    Session $ Batcher.schedule $ CmdWriteSingleRegister uid addr value

writeMultipleRegisters
    :: (MonadIO m, MonadThrow m)
    => UnitId
    -> Address
    -> [Word16] -- ^ Register values to be written
    -> Session m ()
writeMultipleRegisters uid addr values =
    Session $ Batcher.schedule $ CmdWriteMultipleRegisters uid addr values

--------------------------------------------------------------------------------
-- Pretty printing (for debugging and error messages)
--------------------------------------------------------------------------------

prettyAddressRange :: Range Address -> String
prettyAddressRange range =
       "["
    <> show (unAddress $ Range.begin range)
    <> ".."
    <> show (unAddress $ Range.end range)
    <> "]"

prettyCommand :: Command m r -> String
prettyCommand = \case
    CmdPerformIO _io -> "CmdPerformIO <<io>>"
    CmdThrow err -> "CmdThrow " <> show err
    CmdCommand uid fc bs ->
        "CmdCommand " <> unwords [prettyUnitId uid, show fc, show (BB.toLazyByteString bs)]
    CmdReadCoils uid range ->
        "CmdReadCoils " <> unwords [prettyUnitId uid, prettyAddressRange range]
    CmdReadDiscreteInputs uid range ->
        "CmdReadDiscreteInputs " <> unwords [prettyUnitId uid, prettyAddressRange range]
    CmdReadHoldingRegisters uid range ->
        "CmdReadHoldingRegisters " <> unwords [prettyUnitId uid, prettyAddressRange range]
    CmdReadInputRegisters uid range ->
        "CmdReadInputRegisters " <> unwords [prettyUnitId uid, prettyAddressRange range]
    CmdWriteSingleCoil uid addr value ->
        "CmdWriteSingleCoil " <> unwords [prettyUnitId uid, prettyAddress addr, show value]
    CmdWriteMultipleCoils uid addr values ->
        "CmdWriteMultipleCoils " <> unwords [prettyUnitId uid, prettyAddress addr, show values]
    CmdWriteSingleRegister uid addr value ->
        "CmdWriteSingleRegister " <> unwords [prettyUnitId uid, prettyAddress addr, show value]
    CmdWriteMultipleRegisters uid addr values ->
        "CmdWriteMultipleRegisters " <> unwords [prettyUnitId uid, prettyAddress addr, show values]



prettyUnitId :: UnitId -> String
prettyUnitId (UnitId n) = "#" <> show n

prettyAddress :: Address -> String
prettyAddress (Address n) = show n

prettyBatch :: Batch m -> String
prettyBatch = \case
    NotBatched (Batcher.Scheduled cmd _) -> "NotBatched (" <> prettyCommand cmd <> ")"
    BatchedReadCoils br ->
        "BatchedReadCoils" <> " " <> prettyBatchedReads br
    BatchedReadDiscreteInputs br ->
        "BatchedReadDiscreteInputs" <> " " <> prettyBatchedReads br
    BatchedReadHoldingRegisters br ->
        "BatchedReadHoldingRegisters" <> " " <> prettyBatchedReads br
    BatchedReadInputRegisters br ->
        "BatchedReadInputRegisters" <> " " <> prettyBatchedReads br
    BatchedWriteRegisters bwr ->
        "BatchedWriteRegisters"
        <> " " <> prettyUnitId (bwrUnitId bwr)
        <> " " <> prettyAddressRange (bwrRange bwr)
        <> " " <> show (bwrValues bwr)

prettyBatchedReads :: BatchedReads m a -> String
prettyBatchedReads br =
    prettyUnitId (batchedReadUnitId br)
    <> " " <> prettyAddressRange (batchedReadRange br)
    <> " [" <> intercalate ", " (map (prettyAddressRange . fst) $ batchedReadConsumers br) <> "]"

_unusedPrettyCommand :: Command m r -> String
_unusedPrettyCommand = prettyCommand

_unusedPrettyBatch :: Batch m -> String
_unusedPrettyBatch = prettyBatch
