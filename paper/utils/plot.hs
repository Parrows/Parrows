module Main where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import Statistics.Distribution.Normal
import Statistics.Distribution

-- pOak = class1
-- pBeech = class2

stdDevOak :: NormalDistribution
stdDevOak = normalDistr (- 2.25) 1

stdDevBeech :: NormalDistribution
stdDevBeech = normalDistr 1.596 1

type Priori = Double

pOak :: Priori
pOak = 0.25

pBeech :: Priori
pBeech = 0.75

oak :: [Double] -> [(Double,Double)]
oak xs = [ (x,(density stdDevOak x) * pOak) | x <- xs ]

beech :: [Double] -> [(Double, Double)]
beech xs = [ (x,(density stdDevBeech x) * pBeech) | x <- xs ]

identit :: [Double] -> [(Double, Double)]
identit xs = [(x, x) | x <- xs]

increments :: Double -> Double -> Double -> [Double]
increments from to inc
    | from > to = []
    | otherwise = (from):(increments (from + inc) to inc)

--main = print $ (increments (-5) 5 0.1)

main = toFile def "output.png" $ do
    layout_title .= "KMeans"
    setShapes [PointShapeCircle, PointShapePlus, PointShapeStar]
    setColors [opaque blue, opaque red]
    plot (points "class2" $ beech (increments (-5) 5 0.1))
    plot (line "class1" [oak (increments (-5) 5 0.1)])