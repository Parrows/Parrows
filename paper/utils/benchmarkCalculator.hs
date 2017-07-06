module Main where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number

import Text.Regex.PCRE

import Data.String
import Data.CSV

import Data.Maybe
import Data.Either

import Debug.Trace

import System.Environment

data BenchResult = BenchResult {
    name :: String,
    nCores :: Int,
    mean :: Double,
    meanLB :: Double,
    meanUB :: Double,
    stdDev :: Double,
    stdDevLB :: Double,
    stdDevUB :: Double
} deriving (Show)

-- this could probably be done prettier (with Parsec, but works)
convDoubles :: [String] -> Maybe [Double]
convDoubles strs =
    let
        doublesOrErrors = map (\str -> parse floating "" str) strs
        origCnt = length strs
        doubles = rights doublesOrErrors
        doublesCount = length $ doubles
    in
    if doublesCount == origCnt
        then Just $ doubles
        else Nothing

convToBenchResults :: [[String]] -> [BenchResult]
convToBenchResults lines = catMaybes $ map convToBenchResult lines
    where
        convToBenchResult :: [String] -> Maybe BenchResult
        convToBenchResult (nameStr:rest) = go nameStr $ convDoubles rest
            where go nameStr (Just ([mean,meanLB,meanUB,stdDev,stdDevLB,stdDevUB])) =
                    let (name, nCores) = parseName nameStr
                    in
                        Just $ BenchResult {
                            name = name,
                            nCores = nCores,
                            mean = mean,
                            meanLB = meanLB,
                            meanUB = meanUB,
                            stdDev = stdDev,
                            stdDevLB = stdDevLB,
                            stdDevUB = stdDevUB
                        }
                  go name Nothing = Nothing

                  parseName :: String -> (String, Int)
                  parseName str = let (_, _, _, [name, nCores]) = str =~ "(.*) \\+RTS -N([0-9]*).*" :: (String,String,String,[String])
                                  in traceShowId $ (name, read nCores)

main :: IO ()
main = do
    file:rest <- getArgs
    linesOrError <- parseFromFile csvFile file
    handleParse linesOrError
    where
        handleParse :: Either ParseError [[String]] -> IO ()
        handleParse (Right lines) = do
            putStrLn $ "parsing" ++ (show lines)
            putStrLn $ show $ convToBenchResults lines
        handleParse _ = putStrLn "parse Error!"