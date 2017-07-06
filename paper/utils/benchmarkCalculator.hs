module Main where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number

import Text.Regex.PCRE

import Data.String
import Data.CSV

import Data.Maybe
import Data.Either
import qualified Data.Map.Strict as M

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
                  -- little hacky with the two Regexes, but who cares?
                  parseName str = let (_, _, _, nameWithRTS) = str =~ "(.*) \\+RTS -N[0-9]*.*" :: (String,String,String,[String])
                                      (_, _, _, nCores) = str =~ ".* \\+RTS -N([0-9]*).*" :: (String,String,String,[String])
                                  in
                                    (if length nameWithRTS > 0 then head nameWithRTS else str,
                                        if length nCores > 0 then read (head nCores) else 1)

toMap :: [BenchResult] -> M.Map String [BenchResult]
toMap benchRes =
    foldl (\m bRes -> let name_ = name bRes in M.insert name_ (bRes:(lookup' name_ m)) m) M.empty benchRes
        where
            lookup' :: String -> M.Map String [BenchResult] -> [BenchResult]
            lookup' key map = go $ M.lookup key map
                where
                    go (Just a) = a
                    go Nothing = []



main :: IO ()
main = do
    file:rest <- getArgs
    putStrLn $ "parsing from file: " ++ file
    linesOrError <- parseFromFile csvFile file
    handleParse linesOrError
    where
        handleParse :: Either ParseError [[String]] -> IO ()
        handleParse (Right lines) = do
            let benchResultsPerProgram = toMap $ convToBenchResults lines
            putStrLn $ "parsed " ++ show (M.size $ benchResultsPerProgram) ++ " different programs (with different number of cores)"
        handleParse _ = putStrLn "parse Error!"