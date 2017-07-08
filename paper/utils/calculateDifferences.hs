module Main where

import Util hiding (Speedup, nCores, name)

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Drawing

import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart.Backend.Cairo
import Control.Lens

import Text.Regex.PCRE

import Data.String.Utils

import Data.String
import Data.CSV
import Data.List

import Data.Maybe
import Data.Either
import qualified Data.Map.Strict as M

import Debug.Trace

import System.Environment

data Speedup = Speedup {
    num :: String,
    name :: String,
    time :: Double,
    nCores :: String,
    speedup :: Double
} deriving (Show)

main :: IO ()
main = do
    args <- getArgs
    if (length args < 3)
    then
        do
            putStrLn $ "usage: <program> file1 file2 output"
    else
        do
            let (file1:file2:output:rest) = args

                lineToSpeedup :: [String] -> Maybe Speedup
                lineToSpeedup (num:name:numStrings) | (length numStrings) == 3 =
                                                        let [timeString, nCores, speedupString] = numStrings
                                                            maybeDoubles = convDoubles [timeString, speedupString]
                                                        in
                                                            if (isNothing maybeDoubles)
                                                            then
                                                                Nothing
                                                            else
                                                                let (Just [time, speedup]) = maybeDoubles
                                                                in
                                                                    Just $ Speedup {
                                                                        num = num,
                                                                        name = name,
                                                                        time = time,
                                                                        nCores = nCores,
                                                                        speedup = speedup
                                                                    }
                                                   | otherwise = Nothing
                lineToSpeedup _ = Nothing

                diff :: Speedup -> Speedup -> Maybe Speedup
                diff x y
                    | nCores x == nCores y && num x == num y = Just $ Speedup {
                            num = num x,
                            name = (name x) ++ "-" ++ (name y),
                            time = (time x) - (time y),
                            nCores = nCores x,
                            speedup = (speedup x) - (speedup y)
                        }
                    | otherwise = Nothing

                legend :: String
                legend = "\"\",\"name\",\"time\",\"nCores\",\"speedup\"" ++ "\n"

                str :: String -> String
                str st = "\"" ++ st ++ "\""

                toString :: Speedup -> String
                toString x = (str (num x)) ++ "," ++ (str $ name x) ++ "," ++  (show $ time x) ++ "," ++
                                    (str (nCores x)) ++ "," ++ (show $ speedup x) ++ "\n"

                handleParse :: Either ParseError [[String]] -> [Speedup]
                handleParse (Right lines) = catMaybes $ traceShowId $ map lineToSpeedup lines
                handleParse _  = []

            linesOrError1 <- parseFromFile csvFile file1
            linesOrError2 <- parseFromFile csvFile file2

            let speedups1 = handleParse $ linesOrError1
                speedups2 = handleParse $ linesOrError2
            if (length speedups1 == 0 || length speedups2 == 0 || length speedups1 /= length speedups2)
            then
                putStrLn "parse Error!"
            else
                do
                    let maybeDiffs = zipWith diff speedups1 speedups2
                        diffs = catMaybes maybeDiffs
                    if (length diffs /= length maybeDiffs)
                    then
                        putStrLn $ "parse Error!"
                    else
                        do writeFile output $ legend ++ (concat $ map toString diffs)
