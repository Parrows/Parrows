module Util where


import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number

import Data.String.Utils

import Data.String
import Data.CSV
import Data.List

import Data.Maybe
import Data.Either
import qualified Data.Map.Strict as M

import Debug.Trace

import System.Environment



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