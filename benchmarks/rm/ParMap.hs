module ParMap(parMapF) where
import Parallel.Eden


parMapF :: (Trans a, Trans b) => (a->b) -> [a] -> [b]
parMapF f xs = length result `seq` result  where result = [(pf f) # x | x <- xs]

pf :: (Trans a, Trans b) => (a->b) -> Process a b
pf f = process (\x -> f x)


-- tl__ = createProcessTL pid (1::Int)
