
module Main where

import Data.ASN1.Parse
import Data.ASN1.Types

import System.Exit (exitFailure)

instance ASN1Object Integer where
    toASN1 = undefined
    fromASN1 (IntVal i : rest) = Right (i, rest)
    fromASN1 s = Left $ "looking for integer, got " ++ show (take 1 s)

shouldBeParsedAs :: (Eq a, Show a) => ParseASN1 a -> [ASN1] -> a -> IO ()
shouldBeParsedAs p stream expected = case runParseASN1 p stream of
    Left err -> print ("FAILED", stream, "parse failed with", err)
    Right y -> if y == expected
        then return ()
        else do
            print ("FAILED", stream, "parsed as", y, "but expected", expected)
            exitFailure

main :: IO ()
main = do
    shouldBeParsedAs getObject [IntVal 1] (1 :: Integer)
    return ()

