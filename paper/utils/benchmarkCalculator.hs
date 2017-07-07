module Main where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number

import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart.Backend.Cairo
import Control.Lens

import Text.Regex.PCRE

import Data.String
import Data.CSV

import Data.Maybe
import Data.Either
import qualified Data.Map.Strict as M

import Debug.Trace

import System.Environment

data Speedup = Speedup (Maybe SpeedupVal) BenchResult deriving (Show)

type SpeedupsPerProgram = (String, [Speedup])
type NCores = Int
type SpeedupVal = Double

data BenchResult = BenchResult {
    name :: String,
    nCores :: NCores,
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

findSeqRun :: [BenchResult] -> Maybe BenchResult
findSeqRun results = go $ filter (\res -> nCores res == 1) results
    where
        go (res:rest) = Just res
        go _ = Nothing

calculateSpeedUps :: [BenchResult] -> Maybe [Double]
calculateSpeedUps benchResults = let maybeSeqRun = findSeqRun benchResults
    in
        fmap (\seqRun -> speedUps seqRun benchResults) maybeSeqRun
        where
            speedUps :: BenchResult -> [BenchResult] -> [Double]
            speedUps seqRun benchResults = map (speedUp seqRun) benchResults

            speedUp :: BenchResult -> BenchResult -> Double
            speedUp seqRun benchResult = (mean seqRun) / (mean benchResult)

calculateSpeedUpsForMap :: M.Map String [BenchResult] -> [SpeedupsPerProgram]
calculateSpeedUpsForMap m = foldr (:) [] $ M.mapWithKey (\key benchResults ->
                                                let
                                                    maybeSpeedUps = calculateSpeedUps benchResults

                                                    zipToSpeedUp (Just speedUps) = zipWith (Speedup) (map Just speedUps) benchResults
                                                    zipToSpeedUp Nothing = zipWith Speedup (repeat Nothing) benchResults
                                                in
                                                    (key, zipToSpeedUp maybeSpeedUps))
                            m




toPlottableValues :: [SpeedupsPerProgram] -> [(String, [(NCores, SpeedupVal)])]
toPlottableValues speedUpsPerPrograms =
    map (\(name, speedupList) -> (name, catMaybes $ map speedupVal speedupList)) speedUpsPerPrograms

speedupVal :: Speedup -> Maybe (NCores, SpeedupVal)
speedupVal (Speedup (Just speedup) benchRes) = Just $ (nCores benchRes, speedup)
speedupVal _ = Nothing

--oFile def (name ++ ".png") $ do
         --layout_title .= name
         --
       --setShapes [PointShapeCircle, PointShapePlus, PointShapeStar]
       --setColors [opaque blue, opaque red]

setLinesBlue :: PlotLines a b -> PlotLines a b
setLinesBlue = plot_lines_style  . line_color .~ opaque blue

chart :: Renderable ()
chart = toRenderable layout
  where
    am :: Double -> Double
    am x = (sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))

    sinusoid1 :: PlotLines Double Double
    sinusoid1 = plot_lines_values .~ [[ (x,(am x)) | x <- [0,(0.5)..400]]]
              $ plot_lines_style  . line_color .~ opaque blue
              $ plot_lines_title .~ "am"
              $ def

    sinusoid2 :: PlotPoints Double Double
    sinusoid2 = plot_points_style .~ filledCircles 6 (opaque red)
              $ plot_points_values .~ [ (x,(am x)) | x <- [0,7..400]]
              $ plot_points_title .~ "am points"
              $ def

    layout :: Layout Double Double
    layout = layout_title .~ "Amplitude Modulation"
           $ layout_plots .~ [toPlot sinusoid1,
                              toPlot sinusoid2]
           $ def

--plotAll :: [(String, [(NCores, SpeedupVal)])] -> EC (Layout NCores SpeedupVal) ()
--plotAll = mapM_ plotOne

--plotOne :: (String, [(NCores, SpeedupVal)]) -> EC (Layout NCores SpeedupVal) ()
--plotOne (name, plottableValues) = do
--       plot (line "" $ [plottableValues])
--       plot (points name $ plottableValues)


main :: IO ()
main = do
    (file:output:rest) <- getArgs
    putStrLn $ "parsing from file: " ++ file
    linesOrError <- parseFromFile csvFile file
    handleParse output linesOrError
    where
        outProps = fo_size .~ (1024,768)
                $ fo_format .~ PDF
                $ def

        handleParse :: FilePath -> Either ParseError [[String]] -> IO ()
        handleParse output (Right lines) = do
            let benchResultsPerProgram = toMap $ convToBenchResults lines
            let speedUpsPerPrograms = calculateSpeedUpsForMap benchResultsPerProgram
            let plottableValues = toPlottableValues speedUpsPerPrograms
            putStrLn $ "parsed " ++ show (M.size $ benchResultsPerProgram) ++ " different programs (with different number of cores)"
            putStrLn $ "speedUps: " ++ show (speedUpsPerPrograms)
            renderableToFile outProps output chart
            putStrLn $ "finished."
        handleParse _ _ = putStrLn "parse Error!"