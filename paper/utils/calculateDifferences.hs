module Main where

import Util

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number

import Data.String.Utils

import Data.String
import Data.CSV
import Data.List

import Data.Ord

import Statistics.Sample as S hiding(stdDev)
import Data.Vector(fromList)

import Numeric

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
    speedup :: Double,
    stdDev :: Double,
    factor :: Double,
    factorStdDev :: Double,
    overhead :: Double,
    stdDevForOverhead :: Double,
    runtimeX :: Double,
    runtimeY :: Double
} deriving (Show)


textBFLaTeX :: String -> String
textBFLaTeX str = "\"\\textbf{" ++ str ++ "}\""

textITLaTeX :: String -> String
textITLaTeX str = "\"\\textit{" ++ str ++ "}\""

formatOverheadForLaTeX :: Double -> String
formatOverheadForLaTeX overhead
   | overhead < 0 = textBFLaTeX (showFFloat Nothing overhead "")
   | overhead > 0 = textITLaTeX (showFFloat Nothing overhead "")

main :: IO ()
main = do
    args <- getArgs
    if (length args < 3)
    then
        do
            putStrLn $ "usage: <program> file1 file2 output [calculateWorst]"
    else
        do
            let (file1:file2:output:rest) = args

                lineToSpeedup :: [String] -> Maybe Speedup
                lineToSpeedup (num:name:numStrings) | (length numStrings) == 4 =
                                                        let [timeString, nCores, speedupString, stdDevString] = numStrings
                                                            maybeDoubles = convDoubles [timeString, speedupString, stdDevString]
                                                        in
                                                            if (isNothing maybeDoubles)
                                                            then
                                                                Nothing
                                                            else
                                                                let (Just [time, speedup, stdDev]) = maybeDoubles
                                                                in
                                                                    Just $ Speedup {
                                                                        num = num,
                                                                        name = name,
                                                                        time = time,
                                                                        nCores = nCores,
                                                                        speedup = speedup,
                                                                        stdDev = stdDev,
                                                                        factor = 1,
                                                                        factorStdDev = 0,
                                                                        overhead = 0,
                                                                        stdDevForOverhead = 0,
                                                                        runtimeX = 0,
                                                                        runtimeY = 0
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
                            speedup = (speedup x) - (speedup y),
                            stdDev = max (stdDev x) (stdDev y),
                            -- care: different order than time. cause reasons
                            factor = (time y) / (time x),
                            factorStdDev = (max (stdDev x) (stdDev y)) / (time x),
                            overhead = (time y - time x) / (time y),
                            stdDevForOverhead = (max (stdDev x) (stdDev y)) / (time y),
                            runtimeX = time x,
                            runtimeY = time y
                        }
                    | otherwise = Nothing

                legend :: String
                legend = "\"\",\"name\",\"time\",\"nCores\",\"speedup\",\"max stddev\",\"factor\",\"factorStdDev\",\"overhead\",\"stdDevForOverhead\",\"runtimeX\",\"runtimeY\"" ++ "\n"

                str :: String -> String
                str st = "\"" ++ st ++ "\""

                toStringLn :: Speedup -> String
                toStringLn x = (toString x) ++ "\n"

                toString :: Speedup -> String
                toString x = (str (num x)) ++ "," ++ (str $ name x) ++ "," ++  (show $ time x) ++ "," ++
                                    (nCores x) ++ "," ++ (show $ speedup x) ++ "," ++ (show $ stdDev x) ++ "," ++
                                    (show $ factor x) ++ "," ++ (show $ factorStdDev x) ++ "," ++ (show $ overhead x) ++
                                    "," ++ (show $ stdDevForOverhead x) ++ "," ++ (show $ runtimeX x) ++  "," ++ (show $ runtimeY x)

                handleParse :: Either ParseError [[String]] -> [Speedup]
                handleParse (Right lines) = catMaybes $ map lineToSpeedup lines
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
                        diffs = traceShowId $ catMaybes maybeDiffs
                    if (length diffs /= length maybeDiffs)
                    then
                        putStrLn $ "parse Error!"
                    else
                        if (length args >= 4)
                        then
                            let
                                -- hacky, but nvm
                                (_:restOfRest) = rest

                                readBool :: String -> Bool
                                readBool = read

                                calcOutput :: Bool -> Speedup
                                calcOutput True = maximumBy (comparing factor) diffs
                                calcOutput False = minimumBy (comparing factor) diffs

                                rnd :: Speedup -> Speedup
                                rnd x = Speedup {
                                                    num = num x,
                                                    name = name x,
                                                    time = time x,
                                                    nCores = nCores x,
                                                    speedup = speedup x,
                                                    stdDev = stdDev x,
                                                    factor = rndVal (factor x),
                                                    factorStdDev = factorStdDev x,
                                                    overhead = rndVal $ overhead x,
                                                    stdDevForOverhead = stdDevForOverhead x,
                                                    runtimeX = runtimeX x,
                                                    runtimeY = runtimeY x
                                                }

                                rndVal x = (fromIntegral $ round (x * 1000)) / 1000

                                roundedGeometricMean = rndVal . S.geometricMean . fromList

                                roundedMean = rndVal . S.mean . fromList
                            in
                                do appendFile output $ (toString $ rnd $ calcOutput True)
                                    ++ ","
                                    ++ (toString $ rnd $ calcOutput False)
                                    ++ ","
                                    -- geometric mean of factor
                                    ++ (showFFloat Nothing $ roundedMean $ map (factor) diffs) ""
                                    ++ ","
                                    --
                                    ++ (showFFloat Nothing $ rndVal $ factorStdDev $ maximumBy (comparing factorStdDev) diffs) ""
                                    ++ ","
                                    ++ (formatOverheadForLaTeX $ roundedMean $ map (overhead) diffs)
                                    ++ ","
                                    ++ (showFFloat Nothing $ rndVal $ stdDevForOverhead $ maximumBy (comparing stdDevForOverhead) diffs) ""
                                    ++ ","
                                    ++ (showFFloat Nothing $ rndVal $ runtimeX $ maximumBy (comparing nCores) diffs) ""
                                    ++ "\n"

                        else
                            do writeFile output $ legend ++ (concat $ map toStringLn diffs)
