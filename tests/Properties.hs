import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Data.ByteString.Char8 as B
import qualified Data.Double.Conversion.ByteString as B
import qualified Data.Double.Conversion.Text as T
import qualified Data.Text as T
import qualified Regressions
import Unsafe.Coerce
import Data.Word


import Debug.Trace

shortest :: (Double -> String) -> Double -> Double -> Bool
shortest f a b = traceShow (a, b, a == 0.0, b == 0.0) $ case (let x = read (f ab) in traceShow (f ab, x, isNaN x, isInfinite x) x) of
                   ba | isNaN ba      -> isNaN ab
                      | isInfinite ba -> traceShow ("ii", ab, isInfinite ab, signum ba, signum ab) $ isInfinite ab && signum ba == signum ab
                      | otherwise     -> ba == ab
  where ab = a / b

tests :: Test
tests = testGroup "Properties" [
    testProperty "b_shortest" $ shortest (B.unpack . B.toShortest)
  , testProperty "t_shortest" $ shortest (T.unpack . T.toShortest)
  ]

main :: IO ()
main = do
  putStrLn ""
  print 0.0
  print $ B.toShortest 0.0
  print $ B.unpack $ B.toShortest (0.0 / 0.0)
  putStrLn $ "realToFrac = " ++ show (realToFrac (0.0 / 0.0) :: Double)

  print (unsafeCoerce (0.0 / 0.0) :: Word64)

-- 18444492273895866368
-- 18444492273895866368

-- -O0
-- 0.0
-- "0"
-- "-Infinity"

-- -O
-- 0.0
-- "0"
-- "NaN"


  -- defaultMain [tests, Regressions.tests]
--


-- -O0
-- (0.0,0.0,True,True)
-- ("-Infinity",-Infinity,False,True)
-- ("ii",NaN,False,-1.0,NaN)



-- -O
-- (0.0,0.0,True,True)
-- ("NaN",NaN,True,False)
