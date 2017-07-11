%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                            %
%  Eden Porting Project, Philipps-Universität Marburg        %
%                                                            %
%                                                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The following Haskell module defines implementation skeletons for 
the parallel functional language Eden, as they are described in
a number of research papers.

\begin{code}

{-# OPTIONS -cpp #-}

module Parallel.Skel.EdenSkel (-- process schemes :: <varying types>
		 parMap, farm, dm, workpool, workpoolD,
		 -- map skeletons :: (a -> b) -> [a] -> [b]
		 map_par, map_farm, map_dm, map_wp,
		 map_par', map_farm', map_dm', map_wp'',
		 -- stateful replicated workers skeleton:
		 strw,
		 -- divide&conquer skeletons:
		 divCon, divConD, 
-- To Do: insert stable tree divide&conquer
		 -- helper functions to be used externally:
		 -- Int -> [a] -> [[a]] / [[a]] -> [a]
		 -- 
		 unshuffleN,             shuffle, shuffleN,
                 splitIntoN,             unSplit,
		 chunk,                  unchunk,
		 -- simple sequential list strategies
		 spine, whnfspine
	  ) where

import Parallel.Eden hiding (using)
import Data.List

import Parallel.Strategies(Strategy, seqList, r0, rwhnf, using)
 -- We restrict the import to avoid all GpH strategies which use "par".

import Debug.Trace
\end{code}

\section{Eden Map Skeletons}
This section provides essential Eden Skeletons for \verb!map!, as 
described in research papers: ParMap, Farm, Direct Mapping, Workpool.

\subsection{Basic Process Schemes for map skeletons}
\begin{code}
-- basic: parMap
parMap :: (Trans a, Trans b) => 
		Process a b -> [a] -> [b]
parMap p xs =  -- [ p # x | x <- xs ] `using` whnfspine
	eagerInstList (repeat p) xs

-- process farm
farm :: (Trans a, Trans b) => 
                Int -> (Int -> [a] -> [[a]]) -- n, distribute
		    -> ([[b]] -> [b])        -- combine
		    -> Process [a] [b]       -- worker process
		    -> [a] -> [b]            -- what to do
farm np distr combine p inputs = 
        combine (parMap p (distr np inputs))

-- direct mapping
dm :: (Trans a, Trans b) => 
                 Int -> (Int -> [a] -> [[a]]) -> ([[b]] -> [b])
		     -> ([a] -> Process () [b])
		     -> [a] -> [b]
dm np split combine proc inputs = combine results
       where 
         results  = eagerInstList proclist (repeat void)
	 proclist = [ proc (extract i np inputs)| i <- [0..(np-1)] ]
         extract i n inputs = (split n inputs)!!i
	 void = ()

-- workpool: non-determinated order
workpool :: (Trans t, Trans r) =>
	    Int ->			-- #workers
	    Int	->			-- prefetch
	    Process [t] [r] -> 		-- worker process abstraction
	    [t] -> [r]
workpool np pf proc tasks = results
	where results = map snd fromWorkers
	
	      workerlist = eagerInstList (repeat proc) toWorkers
	
	      fromWorkers = merge (tagWithPids workerlist)
	      toWorkers = distribute np requests tasks
	
	      requests =  initialReqs ++ newReqs
	      initialReqs = concat (replicate pf [0..np-1])
	      newReqs = map fst fromWorkers
	      
	      tagWithPids :: [[a]] -> [[(Int, a)]]
	      tagWithPids input = zipWith f input [0..(length input)-1]
	          where f :: [a] -> Int -> [(Int, a)]
	                f [] _ = []
	                f (x:xs) n = (n, x):f xs n
	      
	      distribute :: Int -> [Int] -> [a] -> [[a]]
	      distribute np requests tasks =  map (taskList requests tasks) [0..np-1] 
	      	where taskList :: [Int] -> [a] -> Int -> [a]
	      	      taskList (r:rs) (t:ts) pe
	                  | pe == r = t:(taskList rs ts pe)
	                  | otherwise = taskList rs ts pe
	              taskList _ _ _ = []

-- Workpool: determinated result order
workpoolD :: (Trans a, Trans b) => Int -> Int -> (a -> b) -> [a] -> [b]
workpoolD np prefetch function tasks = map snd results 
	where results = mergeRunsBy lookAtFirst workerList
		         where lookAtFirst :: (Int,b) -> (Int,b) -> Bool
			       lookAtFirst (a,_) (b,_) = a < b
	      fromWorkers = merge (tagWithPids workerList)
	      proc   = process ( map (\(n,x) -> (n,function x)))
	      input  = zip numbers tasks
	      numbers = [1..]
      -- Rest as above
	      workerList = eagerInstList (repeat proc) toWorkers
	      toWorkers = distribute np requests input

	      requests =  initialReqs ++ newReqs
	      initialReqs = concat (replicate prefetch [0..np-1])
	      newReqs = map fst fromWorkers
	      
	      tagWithPids :: [[a]] -> [[(Int, a)]]
	      tagWithPids input = zipWith f input [0..(length input)-1]
	          where f :: [a] -> Int -> [(Int, a)]
	                f [] _ = []
	                f (x:xs) n = (n, x):f xs n
	      distribute :: Int -> [Int] -> [a] -> [[a]]
	      distribute np requests tasks =  map (taskList requests tasks) [0..np-1] 
	      	where taskList :: [Int] -> [a] -> Int -> [a]
	      	      taskList (r:rs) (t:ts) pe
	                  | pe == r = t:(taskList rs ts pe)
	                  | otherwise = taskList rs ts pe
	              taskList _ _ _ = []

{- 
mergeRunsBy: join in some way "sorted" lists into one sorted list
  uses a binary combination scheme to merge several runs into one,
  this ensures that the smallest runs are merged first.
  The comparison function cmp implies the sort criterion.
-}
mergeRunsBy :: (a -> a -> Bool) -> [[a]] -> [a]
mergeRunsBy _ [] = []
mergeRunsBy _ [xs] = xs
mergeRunsBy cmp [xs,ys] = mergeRunsBy2 cmp xs ys
mergeRunsBy cmp xxs = mergeRunsBy2 cmp odds evens
 		    where odds =  mergeHalf xxs
			  evens = mergeHalf (drop 1 xxs)
			  mergeHalf = (mergeRunsBy cmp) . (takeEach 2)
{- this version does not work when lists do not have equal length (due to zipWith):
mergeRunsBy cmp xxs = mergeRunsBy cmp 
			(zipWith (mergeRunsBy2 cmp) (odds) (evens))
 		    where odds = takeEach 2 xxs
			  evens = takeEach 2 (drop 1 xxs) -}

mergeRunsBy2 cmp [] xs = xs
mergeRunsBy2 cmp xs [] = xs
mergeRunsBy2 cmp (x:xs) (y:ys) 
	| cmp y x = y : (mergeRunsBy2 cmp (x:xs) ys)
	| otherwise = x : (mergeRunsBy2 cmp xs (y:ys))

{-
takeEach can be used to separate the neighbour elements when splitting a list
-}
takeEach :: Int -> [a] -> [a]
takeEach n [] = []
takeEach n (x:xs) = x : (takeEach n (drop (n-1) xs))
\end{code}

\subsection{Resulting Map Skeletons}
The Process Schemes presented up to now can be used to define 
implementation skeletons which implement a parallel map with the
same type as the sequential one:

\begin{code}
map_par, map_farm, map_dm, map_wp :: (Trans a , Trans b) => 
				       (a -> b) -> [a] -> [b] 
map_par f = parMap (process f)
map_farm = (farm noPe unshuffleN shuffleN ).mapProc
    where mapProc f = process (map f)
map_dm f xs = dm noPe unshuffleN shuffleN procf xs
    where procf xs = process (\() -> map f xs) 
map_wp f xs = workpoolD noPe 2 f xs

map_par', map_farm', map_dm', map_wp'' :: (Trans a , Trans b) => 
				         (a -> b) -> [a] -> [b] 
maxPe = max (noPe - 1) 1
map_par' f = parMap (process f)
map_farm' = (farm maxPe unshuffleN shuffleN ).mapProc
    where mapProc f = process (map f)
map_dm' f xs = dm maxPe unshuffleN shuffleN procf xs
    where procf xs = process (\() -> map f xs) 
map_wp'' f xs = workpoolD maxPe 2 f xs

\end{code}

\section{Stateful Replicated Workers}
The stateful replicated workers skeleton can handle a type of list 
processing where the worker's jobs depend on a global state which is 
updated by the manager process from time to time. The manager holds 
this global state and updates it when a result from one of the 
workers implies a new global state. A simple example would be a 
parallel search for a minimum.

This is a very common extension to the basic workpool skeleton. Whereas
the basic workpool distributes a fixed number of tasks on demand, the 
number of tasks is not fixed here. The manager may as well generate new 
subproblems from previous results and submit them to the workers.

\begin{code}
strw  :: (Trans tsk, Trans act, 
         Trans res, Trans wl, NFData res) => 
      Int ->                                      -- no. of PE
      Int ->                                      -- buffer size
      (inp -> Int -> ([wl],[tsk],ml)) ->          -- split function
      (wl -> tsk -> [act] -> (res,wl)) ->         -- worker function
      (ml -> res -> Int -> ([[act]],[tsk],ml)) -> -- combine function
      (ml -> result) ->                           -- result function
      inp ->                                      -- input
      result                                      -- result

\end{code}

The meaning of the parameters for \verb!strw! are:
\begin{itemize}
  \item[\small \sc no. of PE] number of workers to include in process scheme;
  \item[\small \sc buffer size] no. of tasks to give to every worker at startup;
  \item[\small \sc split function] constructs initial worker states, initial
    tasks and initial manager state from input and no. of workers;
  \item[\small \sc worker function] evaluates a pair \verb!(result, local state)!
    from a local state, an input and a list of state
    updates;\footnote{The update list is used as a {\tt Maybe} type:
    it is either empty or contains exactly one update.}
  \item[\small \sc combine function] builds a new update list for all workers,
    new tasks and a new manager state from an old manager state, one
    worker result and the no. of workers;
  \item[\small \sc result function] computes the final result from the manager
    state (called when the manager has no more tasks to distribute).
\end{itemize}
\begin{code}

{-
        (trace ("Tarea para : "++ show i ++ "\t" ++ show nrec ++ "\t" ++
	show as ++ "\tDistribuidas:" ++ show (ndis+1) ) (insert i (as,t))
	(distribute np (ndis+1) (ngen+n) cs' ts asss' is ns))
-}
strw np prefetch split wf combine rf inp =  result
  where
    (iniwls,iniTasks,iniml)  = split inp np
    outss                    = [process{-maple-} (worker i wf) #  (wl,actsks) | 
                               (i,wl,actsks) <- zip3 [0..np-1] iniwls actskss] `using` spine
    unorderedResults         = mergeProc outss  
    (moreReqs,results)       = unzip unorderedResults
    (moreTasks,acsss,moreGens,result) = manager np combine rf iniml results
    iniReqs                  = concat (replicate prefetch [0..np-1]) 
    iniGens		     = concat (replicate prefetch (replicate np 0))
    tasks                    = iniTasks ++ moreTasks
    actskss                  = distribute np  0 (length iniTasks) (replicate np 0)
                               tasks acsss (zip iniReqs (repeat 0) ++ zip moreReqs [1..])
			       (iniGens ++ moreGens)
-- spine strategy: unfold a list structure, leaving elements alone
spine, whnfspine :: Strategy [a]
spine [] = ()
spine (x:xs) = spine xs

--whnfspine [] = ()
--whnfspine (x:xs) = x `seq` whnfspine xs
whnfspine = seqList rwhnf

-- Manager with an internal state / con estado interno
--
manager :: Int -> (ml -> res -> Int -> ([[act]],[tsk],ml)) -> 
           (ml -> result) -> ml -> [res] -> ([tsk],[[[act]]],[Int],result)

manager np comb rf ml []     = ([], replicate np [], [], rf ml)
manager np comb rf ml (r:rs) = (ts1 ++ ts2,zipWith' (:) ass asss,n:ns,res)
  where (ts2,asss,ns,res) = manager np comb rf ml' rs
        (ass,ts1,ml')  = comb ml r np
	zipWith' f (x:xs) ~(y:ys) = f x y : zipWith' f xs ys
	n = length ts1

--
-- Worker with an internal state and list of state updates
-- Trabajador con estado interno y lista de actualizaciones
--
worker :: Int -> (wl -> tsk -> [act] -> (res,wl)) -> (wl,[([act],tsk)]) -> 
          [(Int,res)]

worker i wf (wl,[])      = []
--                         unsafePerformIO $
--                      do mapleTermPE 
--                         return []

worker i wf (local,(as,t):actss) = (i,r) : worker i wf (newlocal,actss)
  where (r,newlocal) = wf local t as
-- Esta claro: r es [] o [h]
-- newlocal es el G actualizado.
-- worker solo se llama una vez desde el control, y lego por perezosa 
-- va tirando de eso.
  
distribute :: Int -> Int -> Int -> [Int] -> [tsk] -> [[[act]]] -> [(Int,Int)] ->
              [Int] -> [[([act],tsk)]]

distribute np ndis ngen cs ts asss ((i,nrec):is) (n:ns)
	     | ndis == ngen' && ndis == nrec = replicate np []
	     | ndis == ngen' = distribute np ndis ngen' cs ts  asss is ns
	     where ngen' = ngen + n
distribute np ndis ngen cs (t:ts) asss ((i,nrec):is) (n:ns) =
--        (trace ("Tarea para : "++ show i ++ "\t" ++ show nrec ++ "\t" ++
--	show as ++ "\tDistribuidas:" ++ show (ndis+1) ) (insert i (as,t))
	   (distribute np (ndis+1) (ngen+n) cs' ts asss' is ns) --)

  where ndif 		  = nrec - cs !! i
        (ass1,ass2)       = splitAt ndif (asss !! i)
        as                = concat ass1
        cs'               = replace i nrec cs
        asss'             = replace i ass2 asss


replace 0     e ~(x:xs) = e : xs
replace (n+1) e ~(x:xs) = x : replace n e xs

--insert 0 e ~(x:xs)     = (e:x) : xs
--insert (n+1) e ~(x:xs) = x : insert n e xs

\end{code}

\jbcomment{ToDo: give explicit (easy) example}

The skeleton is used e.g. in the way described in (Pena et
al.,IFL2003) where a Gr\"obner-Basis is computed using the Buchberger
algorithm (an iterative calculation). 


\subsection{Divide and Conquer Skeletons}

\subsubsection{Straight-Forward Implementations} 
The straight-forward method to parallelise Divide and Conquer
algorithms is to unfold the call tree onto different
processors. Process schemes for this implementation are as follows:

\begin{code}
-- divide and conquer skeleton
divCon :: (Trans a, Trans b) => 
           (a -> Bool) -> (a -> b) -> (a -> [a]) -> ([b] -> b) -> Process a b
divCon trivial solve split combine = 
        process (\x -> if trivial x then solve x
		                    else combine (parMap myself (split x)))
	    where myself = divCon trivial solve split combine

-- with additional depth control parameter:
divConD :: (Trans a, Trans b) => 
           Int -> (a -> Bool) -> (a -> b) -> (a -> [a]) -> ([b] -> b) -> Process a b
divConD depth trivial solve split combine = 
        process (\x -> if depth == 0 then seqDC x
                       else if trivial x then solve x 
                       else combine (parMap myself (split x)))
	    where myself = divConD (depth - 1) trivial solve split combine
	          seqDC x = if trivial x then solve x else combine (map seqDC (split x))
\end{code}

The first process scheme proceeds exactly as described, executing
every recursive call as a new process. The second variant allows to
give an additional depth parameter for the recursion, proceeding in a
sequential manner when \verb!depth!$=0$.
\jbcomment{ToDo: BUG! {\tt solve} may be too weak to solve the
problem, if it is not {\tt trivial}}

Both process schemes unfold the call tree on processors chosen by the
runtime environment. Round-Robin distribution is unfavourable for these
skeletons, better use runtime option \verb!-Ernd! when using them.


\subsubsection{Static Tree Implementation} 
To Do: copy code, describe.



\section{Helpers}
The helper functions in this section are needed by different skeletons 


\subsection{Eager Process Instantiation}

These are the functions to create eagerly instantiated processes in programs
(a very common task in Eden programs). Lists are eagerly instantiated by 
evaluating each element to WHNF.
This function uses the internals createProcess and Lift to avoid waiting
for a result after process creation.
%------------------------------
\begin{code}
cpTL :: (Trans a, Trans b) => Process a b -> a -> Lift b
cpTL = createProcess

-- Front End Function:
----------------------
{- eagerInstList zips a list of processes and their input. 
   All processes are instantiated on the way.-}
eagerInstList :: (Trans a, Trans b) =>
		 [Process a b ] -> [a] -> [b]
eagerInstList ps xs = let insts = zipWith (cpTL) ps xs
		      in tlList insts

-- Helpers:
-----------
-- tlList: force the instantiation of the element processes and return the list of results
tlList :: [Lift a] -> [a]
-- tlList insts = foldr1 (seq) insts
tlList insts = forceWHNFSpine insts
	       `seq` (map deLift insts)

-- forceWHNFSpine: evaluate all elements of the list to WHNF
forceWHNFSpine :: [a] -> ()
forceWHNFSpine [] = ()
forceWHNFSpine (x:xs) = x `seq` forceWHNFSpine xs

-- tlLList: the same for an list of lists
tlLList :: [[Lift a]] -> [[a]]
-- tlLList insts = foldr1 (seq) (foldr1 (seq) insts)
tlLList insts = forceWHNFSpine (map forceWHNFSpine insts)
	       `seq` (map (map deLift) insts)
\end{code}
Another way to create early demand is the core transformation for Eden
processes, switched on with the \verb!-feager!  flag during
compilation.  This transformation applies to all let-blocks in the
program and uses \verb!createProcess!, \verb!deLift! and
\verb!case!-expressions to instantiate a process immediately when its
result is bound directly inside a let expression. The process will be
started, even if the bound variable is not needed at all in the
overall result.

\subsection{Distribution and Combination functions}
These are basic helpers needed for the defined skeletons.
All functionality serves to split and combine lists/streams 
(as process input and output), used in the map skeletons above. 

Each pair of functions (in the order presented here) is mutually 
inverse:\\
\begin{xcode}
(shuffle  . (unshuffleN n)) xs == xs
(shuffleN . (unshuffleN n)) xs == xs
( unchunk . (chunk n))      xs == xs
( unSplit . (splitInto n))  xs == xs
\end{xcode}

Note that \verb!unshuffleN! and \verb!splitIntoN! distribute the input list 
round-robin to \verb!n! sublists, whereas the \verb!chunk! function 
chops off sublists of {\it length} \verb!n! from the beginning of its input.
Thus the usage of these distribution functions is different.

\begin{code}
{- unshuffleN splits a list into n lists
    [takeEach n (drop i xs) | i <- [0..(n-1)]] -}
unshuffleN :: Int -> [a] -> [[a]]
unshuffleN n xs = unshuffle xs
		where  unshuffle xs = map (f xs) [0..n-1]
				where f xs i = g (drop i xs)
				      g [] = []
				      g xs = head xs : (g (drop n xs))

-- simple shuffling (not incremental!)
shuffle :: [[a]] -> [a]
shuffle = concat . transpose

{- 
shuffleN joins n lists which had been splitted with unshuffle
-}
shuffleN :: [[b]] -> [b]
-- shuffleN = concat . transpose 
-- this impl. sequential evaluation on input list after	the other
-- for Eden we need a version, that produces the first outputs as fast
--  as possible, i. e. evaluates all input lists concurrently:
shuffleN xxs 
	| and (map null xxs) = []
	| otherwise = (mymaphead xxs) ++ ( shuffleN (map mytail xxs))
		 where mymaphead [] = []
		       mymaphead ([]:xxs) = mymaphead xxs
		       mymaphead ((x:xs):xxs) = x : mymaphead xxs
		       mytail [] = []
		       mytail xs = tail xs


{- bresenham computes [i1, ..., ip] such that i1 + ... + ip = n
		and | ij - ik | <= 1, for all 1 <= j,k <= n  
    (from computer graphics for printing smooth lines)
-}
bresenham :: Int -> Int -> [Int]
bresenham n p = take p (bresenham1 n)
              where bresenham1 m = (m `div` p) : bresenham1 ((m `mod` p)+ n)


{- Parameterized list splitting: 
splitIntoN distributes one list on n lists with Bresenham distribution
             (equal distribution without precondition on length)
-}
splitIntoN :: Int -> [a] -> [[a]]
splitIntoN n xs = f bh xs
		where bh = bresenham (length xs) n
		      f [] [] = []
		      f [] _  = error "some elements left over"
		      f (t:ts) xs = hs : (f ts rest)
				  where (hs,rest) = splitAt t xs 

unSplit :: [[a]] -> [a]
unSplit = concat

{-
chunk is the simple variant, filling the last list with less elements
             (works best on lists of length k*n)
-}
chunk      :: Int -> [a] -> [[a]]
chunk _ [] =  []
chunk n xs =  ys : chunk n zs
    where (ys,zs) = splitAt n xs

unchunk :: [[a]] -> [a]
unchunk = concat

\end{code}
