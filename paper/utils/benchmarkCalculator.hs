module Main where

import Util

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

main :: IO ()
main = do
    args <- getArgs
    if(length args < 2)
    then
        do  putStrLn "usage: <program> file output maxCores plotName dimX dimY (for pdf output)"
            putStrLn "    or <program> file output ignoreSeq (for tex output)"
    else if (length args < 6)
    then
        -- output to TeX compatible csv file
        do
           let (file:output:ignoreSeqStr:rest) = args
               handleParse :: Either ParseError [[String]] -> IO ()
               handleParse (Right lines) = do
                   let
                       ignoreSeq :: Bool
                       ignoreSeq = read ignoreSeqStr

                       benchResultsPerProgram = toMap $ convToBenchResults lines
                       speedUpsPerPrograms = calculateSpeedUpsForMap benchResultsPerProgram
                       plottableValues = toPlottableValues speedUpsPerPrograms

                       speedups :: [Speedup]
                       speedups = concat $ map snd speedUpsPerPrograms

                       -- "","name","time","nCores","speedup"
                       legend :: String
                       legend = "\"\",\"name\",\"time\",\"nCores\",\"speedup\"" ++ "\n"

                       str :: String -> String
                       str st = "\"" ++ st ++ "\""

                       writeToFile :: String -> [Speedup] -> IO ()
                       writeToFile output speedups =
                            writeFile output $
                                legend ++
                                (concat $ zipWith ($) (zipWith ($) (repeat speedUpToString) ([1..]::([Int]))) speedups)

                       speedUpToString :: Int -> Speedup -> String
                       speedUpToString num (Speedup (Just speedupVal) benchRes) =
                            if (nCores benchRes) == 1 && ignoreSeq
                            then ""
                            else (str (show num)) ++ "," ++ (str $ name benchRes) ++ "," ++  (show $ mean benchRes) ++ "," ++
                                (show $ nCores benchRes) ++ "," ++ (show speedupVal) ++ "\n"

                       sanitizeFileName :: String -> String
                       sanitizeFileName str = replace "_" "-" $ replace " " "_" $ replace "/" "" str

                   putStrLn $ "parsed " ++ show (M.size $ benchResultsPerProgram) ++ " different programs (with different number of cores)"
                   putStrLn $ "speedUps: " ++ show (speedUpsPerPrograms)
                   mapM_ (\(name, speedups) -> writeToFile (sanitizeFileName $ output ++ "." ++ name ++ ".csv") speedups) speedUpsPerPrograms
                   putStrLn $ "finished."
               handleParse _ = putStrLn "parse Error!"

           putStrLn $ "parsing from file: " ++ file
           linesOrError <- parseFromFile csvFile file
           handleParse linesOrError
    else
        -- directly plot to PDF file
        do
            let (file:output:maxCoresStr:plotName:dimX:dimY:rest) = args

                maxCores = read maxCoresStr

                outProps = fo_size .~ (read dimX,read dimY)
                        $ fo_format .~ PDF
                        $ def

                handleParse :: Either ParseError [[String]] -> IO ()
                handleParse (Right lines) = do
                    let benchResultsPerProgram = toMap $ convToBenchResults lines
                        speedUpsPerPrograms = calculateSpeedUpsForMap benchResultsPerProgram
                        plottableValues = toPlottableValues speedUpsPerPrograms
                        pointSize = 5
                        thickness = 2
                        pointStyles = cycle [filledCircles pointSize $ opaque black,
                                                filledPolygon pointSize 3 True $ opaque black,
                                                plusses pointSize thickness $ opaque black,
                                                exes pointSize thickness $ opaque black,
                                                stars pointSize thickness $ opaque black,
                                                filledPolygon pointSize 3 False $ opaque black,
                                                hollowPolygon pointSize thickness 3 True $ opaque black,
                                                hollowCircles pointSize thickness $ opaque black,
                                                hollowPolygon pointSize thickness 3 False $ opaque black
                                                ]
                    putStrLn $ "parsed " ++ show (M.size $ benchResultsPerProgram) ++ " different programs (with different number of cores)"
                    putStrLn $ "speedUps: " ++ show (speedUpsPerPrograms)
                    renderableToFile outProps output $ chart maxCores plotName (repeat $ (def :: LineStyle)) pointStyles plottableValues
                    putStrLn $ "finished."
                handleParse _ = putStrLn "parse Error!"

            putStrLn $ "parsing from file: " ++ file
            linesOrError <- parseFromFile csvFile file
            handleParse linesOrError