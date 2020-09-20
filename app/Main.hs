module Main where



import qualified Network.Socket as S
import Control.Exception.Safe (bracket)
import Data.IP (toHostAddress, IPv4)
import qualified System.Hardware.Serialport as SP
import Network.Modbus.RTU
import Data.Range (fromBounds)
import Data.Word (Word16)

main :: IO ()
main = do
    cnf <- connectRTU
    rtn <- runSession directWorker cnf simpleSession
    print rtn



batch :: Worker IO
batch = batchWorker $ defaultBatchConfig { bCfgBatchWriteSingleRegister = True, bCfgReorderWrites = False }

simpleSession :: Session IO [Word16]
simpleSession =
    readInputRegisters 23 (fromBounds 0 1)

withSocket :: S.SockAddr -> (S.Socket -> IO a) -> IO a
withSocket addr = bracket (connect addr) close
  where close s = S.gracefulClose s 1000

connect :: S.SockAddr -> IO S.Socket
connect addr = do
    putStrLn ("Connecting to " ++ show addr ++ "...")
    s <- S.socket S.AF_INET S.Stream S.defaultProtocol
    S.connect s addr
    putStrLn "connected"
    return s

connectRTU :: IO Config
connectRTU = do
    putStrLn "Connecting ..."
    s <- SP.openSerial "/dev/pts/1" SP.defaultSerialSettings { SP.stopb = SP.Two}
    putStrLn "connected"
    return $ configRTU s

getAddr :: IPv4 -> Int -> S.SockAddr
getAddr ip portNum = S.SockAddrInet (fromIntegral portNum) (toHostAddress ip)






configRTU :: SP.SerialPort -> Config
configRTU  s = Config
 {
       cfgWrite = SP.send s
     , cfgRead = SP.recv s 4096
     , cfgCommandTimeout = 1000000
     , cfgRetryWhen = const . const False
     , cfgEnableBroadcasts = True
 }




-- configuration
-- open a terminal and write
-- socat -d -d pty,raw,echo=0 pty,raw,echo=0
-- open a second terminal and write
-- sudo ./diagslave -b 9600 -p none -m rtu /dev/pts/2
-- now writing to "/dev/pts/1" with 9600 baudrate should work