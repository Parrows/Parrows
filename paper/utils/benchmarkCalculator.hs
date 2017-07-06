module Main where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import Data.String
import Data.CSV

import Data.Maybe
import Data.Either

import Debug.Trace

import System.Environment

data BenchResult = BenchResult {
    name :: String,
    mean :: Double,
    meanLB :: Double,
    meanUB :: Double,
    stdDev :: Double,
    stdDevLB :: Double,
    stdDevUB :: Double
} deriving (Show)

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
        convToBenchResult (name:rest) = go name $ convDoubles rest
            where go name (Just ([mean,meanLB,meanUB,stdDev,stdDevLB,stdDevUB])) =
                    Just $ BenchResult {
                        name = name,
                        mean = mean,
                        meanLB = meanLB,
                        meanUB = meanUB,
                        stdDev = stdDev,
                        stdDevLB = stdDevLB,
                        stdDevUB = stdDevUB
                    }
                  go name Nothing = Nothing

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