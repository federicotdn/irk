module Infix
  ( isInfixOfC,
    cpuHasAVX2,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Foreign.C.Types (CBool (CBool), CLLong (CLLong), CUChar (..))
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)

-- UChar = unsigned char -> uint8
-- LLong = long long     -> int64
-- CBool = bool          -> bool
foreign import capi "infix.h has_avx2" has_avx2 :: CBool

foreign import capi "infix.h is_infix_of"
  c_is_infix_of :: Ptr CUChar -> CLLong -> Ptr CUChar -> CLLong -> IO CBool

cpuHasAVX2 :: Bool
cpuHasAVX2 = has_avx2 /= 0

isInfixOfC :: BS.ByteString -> BS.ByteString -> Bool
isInfixOfC needle haystack = do
  if cpuHasAVX2
    -- Docs regarding 'unsafePerformIO':
    -- "For this to be safe, the IO computation should
    --  be free of side effects and independent of its environment."
    then unsafePerformIO $
      BSU.unsafeUseAsCStringLen needle $ \(needlePtr, needleLen) ->
        BSU.unsafeUseAsCStringLen haystack $ \(haystackPtr, haystackLen) ->
          (/= 0)
            <$> c_is_infix_of
              (castPtr needlePtr)
              (fromIntegral needleLen)
              (castPtr haystackPtr)
              (fromIntegral haystackLen)
    else
      needle `BS.isInfixOf` haystack
