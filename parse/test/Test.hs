
module Main where

import Data.ASN1.Parse
import Data.ASN1.Types
import Control.Applicative

import System.Exit (exitFailure)

instance ASN1Object Integer where
    toASN1 = undefined
    fromASN1 (IntVal i : rest) = Right (i, rest)
    fromASN1 s = Left $ "looking for integer, got " ++ show (take 1 s)

instance ASN1Object Bool where
    toASN1 = undefined
    fromASN1 (Boolean b : rest) = Right (b, rest)
    fromASN1 s = Left $ "looking for bool, got " ++ show (take 1 s)

shouldBeParsedAs :: (Eq a, Show a) => ParseASN1 a -> [ASN1] -> a -> IO ()
shouldBeParsedAs p stream expected = case runParseASN1 p stream of
    Left err -> do
        print ("FAILED", stream, "parse failed with", err)
        exitFailure
    Right y -> if y == expected
        then print ("SUCCESS")
        else do
            print ("FAILED", stream, "parsed as", y, "but expected", expected)
            exitFailure

shouldFail :: Show a => ParseASN1 a -> [ASN1] -> IO ()
shouldFail p stream = case runParseASN1 p stream of
    Left err -> print ("SUCCESS", "parse failed with", err)
    Right y -> do
        print ("FAILED", stream, "parsed as", y, "but expected to fail")
        exitFailure

getBool :: ParseASN1 Bool
getBool = getObject
getInteger :: ParseASN1 Integer
getInteger = getObject

main :: IO ()
main = do
    shouldBeParsedAs getInteger [IntVal 1] 1
    shouldBeParsedAs (empty <|> getObject) [Boolean True] True
    shouldFail (empty :: ParseASN1 Bool) [Boolean True]
    shouldFail ((empty :: ParseASN1 Bool) <|> empty) [Boolean True]
    shouldBeParsedAs (empty <|> getBool) [Boolean True] True
    shouldBeParsedAs (fmap show getInteger <|> fmap show getBool) [Boolean True] "True"
    shouldBeParsedAs (fmap show getInteger <|> empty <|> fmap show getBool <|> empty) [Boolean True] "True"
    shouldBeParsedAs (empty <|> pure True) [] True
