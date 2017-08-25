{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Main (tests, main) where

import Control.Monad (forM_)
import System.Exit
import Distribution.TestSuite
--import JavaScript.JSON.Types.Instances
--import JavaScript.JSON.Types.Internal as JSON
--import Numeric.DataFrame
--import Commons
--import Model.GeoJSON.Point
--import Model.GeoJSON.Coordinates

--fromJSONPoint1 :: IO Progress
--fromJSONPoint1 = testFromJSON
--    "{ \"type\": \"Point\", \"coordinates\": [15,12,1,6]}"
--    (Point (vec3 15 12 1), PaddedZeros False)
--
--fromJSONPoint2 :: IO Progress
--fromJSONPoint2 = testFromJSON
--    "{ \"type\": \"Point\", \"coordinates\": [6, 3]}"
--    (Point (vec3 6 3 0), PaddedZeros True)
--
--fromJSONVec4f1 :: IO Progress
--fromJSONVec4f1 = testFromJSON
--    "[6, 3]"
--    (vec4 6 3 0 1 :: Vec4f, PaddedZeros True)
--
--fromJSONVec4f2 :: IO Progress
--fromJSONVec4f2 = testFromJSON
--    "[6, 3, 11]"
--    (vec4 6 3 11 1 :: Vec4f, PaddedZeros False)
--
--fromJSONVec4f3 :: IO Progress
--fromJSONVec4f3 = testFromJSON
--    "[6, 3, 11, 23]"
--    (vec4 6 3 11 1 :: Vec4f, PaddedZeros False)
--
--fromJSONPointSeq1 :: IO Progress
--fromJSONPointSeq1 = testFromJSON
--    "[[2, 1, 0], [6, 3, 11, 23], [12, 22, 2], [10, 0, 4]]"
--    (PointSeq . SomeDataFrame $
--            vec4  2 1  0 1
--       <::> vec4  6 3 11 1
--       <+:> vec4 12 22 2 1
--       <+:> vec4 10  0 4 1 :: PointSeq
--    , PaddedZeros False)
--
--fromJSONPointSeq2 :: IO Progress
--fromJSONPointSeq2 = testFromJSON
--    "[[2, 1, 0], [6, 3], [12, 22], [10, 0, 4]]"
--    (PointSeq . SomeDataFrame $
--            vec4  2 1  0 1
--       <::> vec4  6 3  0 1
--       <+:> vec4 12 22 0 1
--       <+:> vec4 10  0 4 1 :: PointSeq
--    , PaddedZeros True)


-- | Collection of tests in detailed-0.9 format
tests :: IO [Test]
tests = return
  [ --test "GeoJSON point 1"    fromJSONPoint1
--  , test "GeoJSON point 2"    fromJSONPoint2
--  , test "GeoJSON Vec4f 1"    fromJSONVec4f1
--  , test "GeoJSON Vec4f 2"    fromJSONVec4f2
--  , test "GeoJSON Vec4f 3"    fromJSONVec4f3
--  , test "GeoJSON PointSeq 1" fromJSONPointSeq1
--  , test "GeoJSON PointSeq 2" fromJSONPointSeq2
  ]

-- | Run tests as exitcode-stdio-1.0
main :: IO ()
main = do
    trs <- tests >>= mapM (\(Test ti) -> (,) (name ti) <$> run ti)
    forM_ trs $ \(n, rez) -> putStrLn $ case rez of
        Progress _ _ -> n ++ " is still in progress"
        Finished ans -> n ++ ": " ++ show ans
    case filter (not . isGood) trs of
       [] -> exitSuccess
       xs -> die $ "Failed tests: " ++ unwords (fmap fst xs)
  where
    isGood (_, Finished Pass) = True
    isGood _ = False

--
---- | Convert bool tests into Cabal tests
--test :: String -> IO Progress -> Test
--test tName propOp = Test testI
--  where
--    testI = TestInstance
--        { run = propOp
--        , name = tName
--        , tags = []
--        , options = []
--        , setOption = \_ _ -> Right testI
--        }
--
--testFromJSON :: (FromJSON a, Eq a) => JSString -> a -> IO Progress
--testFromJSON j val = do
--    ejsonval <- parseJSONValue j
--    return $ case ejsonval of
--      Left err -> Finished .  Fail $ "JSON.parse: " ++ show err
--      Right v  -> case fromJSON v of
--        JSON.Error er -> Finished .  Fail $ "fromJSON: " ++ er
--        JSON.Success a -> if a == val
--                          then Finished Pass
--                          else Finished . Fail $ "Result mismatch"
