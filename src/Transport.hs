module Transport
  ( Transport (..),
    readNBytes,
    readLine,
    writeBytes,
    writeString,
    setup,
  )
where

import qualified Data.ByteString.Lazy as BSL
import System.IO (BufferMode (..), hSetBinaryMode, hSetBuffering, stderr, stdin, stdout)

data Transport = Stdio

setup :: Transport -> IO ()
setup Stdio = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetBinaryMode stderr True
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

readNBytes :: Transport -> Int -> IO BSL.ByteString
readNBytes Stdio = BSL.hGet stdin

readLine :: Transport -> IO String
readLine Stdio = getLine

writeBytes :: Transport -> BSL.ByteString -> IO ()
writeBytes Stdio = BSL.putStr

writeString :: Transport -> String -> IO ()
writeString Stdio = putStr
