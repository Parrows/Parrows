\section{Eden Implementation of Remote Data}\label{rd}
%\mdcomment{Diese Datei kann nach Aenderung des Namens
%von remdata.tex in
%  remdata.lhs mit GHC uebersetzt werden. Benoetigt Module ParPrim.hs
%  und Eden.hs.}
%
% ignore this in latex:
\newcommand{\ign}[1]{}
\ign{
\begin{code}
{-# OPTIONS_GHC -cpp -fno-cse -fglasgow-exts #-}
module RemoteData
 -- TODO export list
where
import Parallel.Eden
import System.IO.Unsafe
import Control.Monad
import Data.List (transpose)
import Debug.Trace
import Control.Parallel.Strategies (seqList, rwhnf)

a = 1 :: Int

--transform a list of remote data into a corresponding local data list
--blocks on partial defined list structure
fetchAll :: Trans a => [RD a] -> [a]
fetchAll das = -- let (cs,as) = createChans (length das)
               -- in multifill das cs as
               let fetchAllL = [fetch' da | da <- das]
               in map fromL (fetchAllL `using` seqList rwhnf)

data L a = L a
fromL (L a) = a

fetch'   :: Trans a => RD a -> L a
fetch' cc = new (\c a -> parfill cc c (L a))

--transform a list of local data into a corresponding remote data list
releaseAll :: Trans a => [a] -> [RD a]
releaseAll as = [release a | a <- as]

--transform a list of remote data into a corresponding local data list
--fetchAll :: Trans a => [RD a] -> [a]
--fetchAll das = id das

--auxiliary functions
createChans :: Trans x => Int -> ([ChanName x], [x])
createChans 0 = ([],[])
createChans n = new (\chX valX ->
             let (cs,xs) = createChans (n-1)
             in (chX:cs,valX:xs))

multifill :: Trans x => [ChanName x] -> [x] -> b -> b
multifill []  _  b = b
multifill _  []  b = b
multifill (c:cs) (x:xs) b = parfill c x (multifill cs xs b)
\end{code}
}

%\begin{figure} [t!]
%\begin{code}
%-- g.f on remote processes in different versions
%r1, r2 :: (Trans a, Trans b, Trans c)
%          => (a -> b) -> (b -> c) -> a -> c
%--simple
%r1 f g a = process g # (process f # a)
%
%--optimised with remote data
%r2 f g a = process (g . fetch) # (process (release . f) # a)
%\end{code}
%\ign{
%--optimised with dynamic channels
%r2 f g a = process f' # (a,cg) `seq` r
%  where
%    f' (a,cg) = parfill cg (f a) ()
%    g' _      = new (\cg b -> (cg,g b))
%    (cg,r)    = process g' # ()
%}

%
%\subsection{Remote data definition}
\begin{figure}[t]
\begin{code}
-- remote data
type RD a = ChanName (ChanName a)

-- convert local data into corresponding remote data
release :: Trans a => a -> RD a
release x = new (\cc c -> parfill c x cc)

-- convert remote data into corresponding local data
fetch   :: Trans a => RD a -> a
fetch cc = new (\c x -> parfill cc c x)
\end{code}
%--example
%circle = process fetch # (process release # a)
%\begin{center}
%\input{tikz/rd-impl.tex}
%\end{center}
\caption{Remote data definition}
\label{rd-def}
\end{figure}

\ign{
Dummy implementation
\begin{nocode}
--remote Data
type RD a = a

--transform local data into corresponding remote data
release :: Trans a => a -> RD a
release a = a

--transform remote data into corresponding local data
fetch   :: Trans a => RD a -> a
fetch cc = cc

\end{nocode}
}

The implementation of remote data in Eden (Figure \ref{rd-def}) is simple and
elegant. To \cd{release} a local data \cd{x} of type \cd{a} we create -- using the function \cd{new} --
a channel name \cd{cc} of type \cd{ChanName (ChanName a)} via which a channel
\cd{c} of type \cd{ChanName a} will be received. Using \cd{parfill} a thread is
forked that subsequently sends the local data \cd{x} via the channel \cd{c}. The result of the \cd{release} function is the newly created channel \cd{cc :: ChanName (ChanName a)}. Note that the remote data type \cd{RD a} is a synonym of \cd{cc}'s type. Data of type \cd{RD a} is merely a channel name and thus very lightweight with low communication costs. To access remote data we need to \cd{fetch} it by again creating a channel \cd{c :: ChanName a} using the function \cd{new}. This channel is sent via the remote data handle, i.e. the channel \cd{cc} of type \cd{RD a}. The proper data is then received via channel \cd{c} and returned as the result of the \cd{fetch} function.


A problem arises when remote data needs to be duplicated. Channel names
(of type \cd{ChanName a}) cannot be used more than once to retain referential
transparency \cite{Eden}. As remote data is implemented as a specialized
channel name, it must not be duplicated and fetched several times in parallel. A manual work\-around to duplicate remote data on a node would be to fetch the data and release it again repeatedly. We considered more sophisticated versions which make the use of remote data
more comfortable, but they expose nondeterminism and should therefore not be
implemented in the actual version of Eden.

%There is some overhead in
%communication. Compared to the common way of defining explicit communication
%starting at the receiver, there is one additional call to \cd{parfill}. This
%should be no serious penalty as the additional transmitted value is of type
%\cd{ChanName a}, which is quite small and fast to transmit.
Our new way of communication creates a slight overhead. In comparison to the common way of defining explicit communication we have an additional channel per direct connection that is used only before the transmission of the actual data begins. However, as this channel only transports a value of type \cd{ChanName a} which is quite small the increase in communication cost should not be noticeable.




\begin{figure}[t]
\begin{center}
\input{tikz/rd-motivation.tex}
\end{center}
\vspace {-1.5em}
\caption{Using remote data}
\label{rd-ex}
\vspace{-1em}
\end{figure}
\paragraph{Example.} We show a small example where the remote data concept is used to establish a direct channel connection between sibling processes. Given functions \cd{f} and \cd{g}, one
can calculate \cd{(g . f) a} in parallel creating a process for each function.
Figure \ref{rd-ex} shows two different ways to implement this.
Simply replacing the function calls by process instantiations
\\
\centerline{\cd{r1 a = process g # (process f # a)}}
leads to the following behaviour (visualised in the left part of Fig.~\ref{rd-ex}):
Function \cd{r1} instantiates the first process calculating f, passes its
input to this process and receives the remotely calculated result. It instantiates a second process calculating
\cd{g} and passes the result of process \cd{f} to this new process. The output
of the second process is also sent back to the caller. The drawback of this
approach is that the result of the first process will not be sent directly to
the second process. This causes unnecessary communication costs.
%
% The second approach implemented by function \cd{r2} uses dynamic reply channels
% to avoid this bottleneck, but is obviously more complicated. Functions \cd{f} and
% \cd{g} are extended by explicit channel creation and use. Function \cd{g' :: (c,
% ChanName b)->()} creates a \cd{new} channel and returns it's ``name'' \cd{cg} of
% type \cd{ChanName b} aside the subsequent calculated result \cd{r}. Function
% \cd{f' :: (a, ChanName b)->()} gets this ``name'' \cd{cg} aside the real
% calculation input \cd{a}. \cd{f'} uses \cd{parfill} to send the result of the
% computation \cd{f a} directly to \cd{g'}. Processes evaluating \cd{f'} and
% \cd{g'} are instantiated conveniently. The difficulty is that communication needs
% to be build up backwards from \cd{g} to \cd{f} which is kind of unnatural for a
% programmer to express. This leads also to a structural change in function types
% for \cd{f' and g'}. The output (respectively input) represented by a channel is
% passed as function input (respectively output).


We use remote data \cd{RD a} in the second
%third and more intuitive
implementation
\\
\centerline{\cd{r2 a = process (g . fetch) # (process (release . f) # a)}. }
It uses function \cd{release} to produce a handle of
type \cd{RD a} for data of type \cd{a}. Calling \cd{fetch} with remote data returns the value
released before. The definition of \cd{r2} is identical to \cd{r1} except for
the conversion of the result type of \cd{f}'s process and the input type of
\cd{g}'s process to remote data. The use of remote data leads to a direct
communication of the actual data between the processes of \cd{f} and \cd{g} (see the right part of Fig.~\ref{rd-ex}).
% like in function \cd{r2}.
%In contrast to the use of \cd{new} and \cd{parfill} in previous example,
The remote data handles are treated like the original data in the first version. The basic structure of the program, i.e. the composition of two process instantiations, remains the same.

\medskip
To prepare the composition of skeletons which will have remote data both as parameter type and result type we introduce the function \cd{liftRD} to lift functions acting on data to functions performing the same computation on remote data. 
\begin{code}
liftRD   :: (Trans a, Trans b) => (a->b) -> RD a -> RD b
liftRD f =  release . f . fetch
\end{code}


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Composing Predefined Skeletons}\label{compose}
Before handling the composition of skeletons using the remote data concept, we
show the lifting of a simple parallel map skeleton to a remote data interface.
Then we define a parallel all-to-all skeleton which generates a number of
processes each of which exchanges data with any of the others. Using these skeletons
with their remote data interfaces enables us to define a sequence consisting of a parallel map, a parallel transpose (realised using the all-to-all skeleton) and a second parallel map. This can be useful in an
implementation of a parallel FFT skeleton \cite{fftpact09} or a
Google Map-Reduce skeleton \cite{gmreuropar09}. In
\cite{gmreuropar09,fftpact09}, corresponding parallel map-transpose skeletons
have been defined as monolithic skeletons without composing simpler skeletons. With the remote data interface, we can define the same skeleton as a composition of the three component skeletons. This leads to a much better understandable definition
while achieving the same performance. Finally,  we present another elegant and
concise definition of an even more complex communication pattern: a butterfly
scheme which is used to define an allreduce-skeleton.

\begin{figure}[t!]
\begin{nocode}
parmap :: (Trans a,Trans b) => (a->b) -> [a] -> [b]
parmap f xs = spawn pfs xs
    where pfs = repeat (process f)
\end{nocode}
\begin{code}
-- code for spawn:
spawn :: (Trans a,Trans b) => [Process a b] -> [a] -> [b]
spawnAt :: (Trans a,Trans b) => [Int] -> [Process a b] -> [a] -> [b]
spawnAt pos ps is 
    = unsafePerformIO $ 
           sequence
              [instantiateAt st p i |
              (st,p,i) <- zip3 (cycle pos) ps is]

spawn = spawnAt [0]
\end{code}
\begin{code}
parmapDC :: (Trans a,Trans b) => (a->b) -> [RD a] -> [RD b]
parmapDC f xs = spawn pfs xs
    where pfs = repeat (process (liftRD f))
\end{code}
%-- "Direct connect process" abstraction
%processDC :: (Trans a, Trans b) =>
%             (a->b) -> Process (RD a) (RD b)
%processDC f = process (release . f . fetch)
\begin{code}
processDC :: (Trans a, Trans b) =>
             (a->b) -> Process (RD a) (RD b)

processDC f = process $ liftRD f
\end{code}
% --usage example (dummy): map f g
% mfg :: (Enum a, Num a,Trans a) => [RD a] -> [RD [a]]
% mfg = (parmapDC g) . (parmapDC f)
%     where
%       --f :: Num a => (a -> [a])
%         f x   = zipWith l [1..100] [x,(2*x)..(100*x)]
%       --g :: Num a => ([a] -> [a])
%         g     = zipWith l [1..100]
%         l x y = foldl (+) y [1..x*x]
% \end{code}
\vspace{-0.5em}
\caption{The \cd{parmap} and \cd{parmapDC} skeletons}
\label{code:parmapDC}
\vspace{2em}
\end{figure}


\begin{wrapfigure}[13]{r}{.38\textwidth}
\vspace{-6.7em}
\hspace{-5.3em}
\centering
  \input{tikz/parmapDC}
  \vspace{-1em}
  \caption{Visualization of the \cd{parmapDC} skeleton}
  \label{fig:parmapDC}
\end{wrapfigure}
% \begin{figure}
% \begin{center}
% \input{tikz/parmapDC}
% \end{center}
% \caption{Visualization of the \cd{parmapDC} skeleton}
% \label{fig:parmapDC}
% \end{figure}

\subsection{The \cd{parmapDC} skeleton}
%We focus here on the main issue of the paper: The composition of
%arbitrary process topologies wrapped up in skeletons. Figure \ref{parmap-comp} shows the skeleton
%\cd{parmapDC}, a parallel map variant which takes remote data as input and
%produces remote data as output. The basic version of \cd{parmap} can be expressed
%as a slight variant of the Eden library function \cd{spawn} by transforming the
%parameter function into a list of processes. The definition of \cd{parmapDC} is
%identical to the definition of the basic \cd{parmap} skeleton in Eden, except
%that \cd{processDC}\footnote{``\cd{DC}'' in \cd{parmapDC} and
%\cd{processDC} is short for \textbf{D}irect \textbf{C}omposable with remote
%data} is used instead of process. To work with remote data, \cd{processDC} applies \cd{fetch} before, and \cd{release} after applying the processes
%parameter function \cd{f}. So we can express a pipeline of parallel maps.
%
%The definition of remote data
%further offers the possibility to define functionality as independent skeletons that crucially
%depends on distributed input. Examples are a parallel transpose or a parallel
%reduce skeleton. Such functionality was hard coded before inside each skeleton
%that depends on it or was performed sequentially by the root process. Therefore
%remote data supports enhanced modularity, promoting simple composable skeletons.
%
A parallel map creates a process for each element of the input list. In Eden, it
can easily be defined using the function \cd{spawn} (see Fig.~\ref{code:parmapDC}).
Note that this definition implies that the process evaluating \cd{parmap} creates
as many processes as there are elements in the input list and sends each of theses elements to the corresponding process. Using a remote data interface, each
process only gets a handle to its list element. It can then use this handle to fetch
the element directly from the remote place where this element is located. In
order to achieve this behaviour, we simply replace the parameter function
\cd{f} in the process abstraction by its lifted pendant \cd{liftRD f} (see
Fig.~\ref{code:parmapDC}). This leads to the skeleton \cd{parmapDC} where the
ending \cd{DC} stands
%Figure \ref{parmap-comp} shows the skeleton
%\cd{parmapDC}, a parallel map variant which takes remote data as input and
%produces remote data as output. The basic version of \cd{parmap} can be expressed
%as a slight variant of the Eden library function \cd{spawn} by transforming the
%parameter function into a list of processes. The definition of \cd{parmapDC} is
%identical to the definition of the basic \cd{parmap} skeleton in Eden, except
%that \cd{processDC}\footnote{``\cd{DC}'' in \cd{parmapDC} and
%\cd{processDC} is short 
for \textbf{D}irectly \textbf{C}omposable due to the remote data interface. 
%is used instead of process. To work with remote data, \cd{processDC} applies \cd{fetch} before, and \cd{release} after applying the processes
%parameter function \cd{f}. So we can express a pipeline of parallel maps.
This interface makes it possible for skeletons to receive distributed input and to produce distributed output which is crucial for an efficient composition of skeletons.
Fig.~\ref{fig:parmapDC} visualises the behaviour and communication pathes of the \cd{parmapDC} skeleton. The upper circle represents the process evaluating the \cd{parmapDC} instantiation. It generates the other processes whose task is to apply the parameter function \cd{f} to input of type \cd{a} and produce output of type \cd{b}. Note that only remote data handles for the input and the output values are communicated between the generator process and its child processes. The proper data is communicated via dynamic channel connections indicated by dashed lines. 
%The definition of remote data
%further offers the possibility to define functionality as independent skeletons that crucially
%depends on distributed input. Examples are a parallel transpose or a parallel
%reduce skeleton. 

\subsection{The \cd{allToAllDC} skeleton}

\begin{figure}[b!]
\begin{code}
allToAllDC :: forall a b i. (Trans a, Trans b, Trans i) =>
          --(#Elements, data in, data out)
            (Int->a->[i]) ->       -- transform before transpose
            ([i]->b) ->            -- transform after transpose
            [RD a] -> [RD b]
allToAllDC t1 t2 xs = res where
  t1' = t1 (length xs)           --same amount of procs as #xs
  (res,iss) = unzip $ spawn procs inp
  inp       = lazy2Zip xs (transpose iss)

  procs      = repeat $ process $ uncurry p
  p :: (Trans a,Trans b,Trans i)=> RD a-> [RD i]-> (RD b,[RD i])
  p x theirIs = (res, myIs) where
    res   = (release . t2 . fetchAll) theirIs
    myIs  = (releaseAll . t1' . fetch) x

--lazy in second argument
lazy2Zip (x:xs) ~(y:ys) = (x,y): lazy2Zip xs ys
lazy2Zip []     _     = []
\end{code}
\ign{$}
\vspace{-1em}
\begin{center}
\input{tikz/allToAll}
\end{center}
%
\vspace{-2em}
\caption{The \cd{allToAllDC} skeleton: code and visualisation.
(The darker shading of the arrows from the uppermost child process emphasizes the connectivity of a single process.)}
\label{allToAll}
\vspace{-1em}
\end{figure}

In Figure \ref{allToAll} we present an all-to-all skeleton \cd{allToAllDC}. 
This skeleton depends inherently on its inner communication pattern which
we will implement using remote data. We need the following variants of the remote data interface functions in order to fetch or release a list of remote data: 
\begin{itemize}
\item \cd{releaseAll :: [a] -> [RD a]} is defined as \cd{map release}.
\item \cd{fetchAll :: [RD a] -> [a]} is semantically equivalent to \cd{map
fetch}, but needs a special eager implementation which
initiates to fetch each input list element without waiting for the result of
this action.
\end{itemize}
The input of the \cd{allToAllDC} skeleton is a list of remote data with, say, $n$ elements and two transformation functions \cd{t1} and \cd{t2} to allow the processes to transform the input data before sending data to all other processes and after receiving data from all other processes, respectively. The length
of the input list determines the number of processes to be created by \cd{spawn}.  Every
process will fetch its remote input \cd{x} and transform it with the 
transformation function \cd{t1}. This yields a list of intermediate data for each child process which is released elementwise by \cd{releaseAll}, giving the list \cd{myIs :: [RD i]} with remote data handles. Note that this list must have the same number $n$ of elements as the input list.
This list of remote data handles is returned to the root process in the second component
of each process's result tuple. % of each process \cd{p} to the root process. 
The root process receives one such list from each of its child
processes resulting in the $n \times n$ matrix \cd{iss :: [[RD i]]}. It
transposes this matrix and sends the result back to the processes as its second,
lazily supplied parameter \cd{theirIs}. Each process gets thus one remote intermediate
value of type \cd{RD i} of each sibling process and of itself. The values are gathered using
\cd{fetchAll}, 
%\footnote{\cd{fetchAll :: [RD a] -> [a]}: a eager cascading variant
%of \cd{fetch} for lists}, 
transformed by the second parameter function \cd{t2} to the output type \cd{b} 
and released. 
% to be passed directly to a possible successor process.
The visualisation in Fig.~\ref{allToAll} again shows the exchange of remote data handles between the root process the child processes and using dashed arrow the direct communication of data between the processes. 


%Such functionality was hard coded before inside each skeleton
%that depends on it or was performed sequentially by the root process.



%-}
% \begin{figure}
% \begin{code}
% --usage example (dummy): (map g) . transpose . (map f)
% mfTmg :: (Enum a, Num a,Trans a) => [RD a] -> [RD [[a]]]
% mfTmg = parmapDC (map g) . parTranspose . parmapDC f
%     where
%       --parTranspose :: [RD [a]]->[RD[[a]]]
%         parTranspose = allToAll unshuffleN transpose
%       --f :: Num a => (a -> [a])
%         f x   = zipWith l [1..100] [x,(2*x)..(100*x)]
%       --g :: Num a => ([[a]] -> [[a]])
%         g     = zipWith l [1..100]
%         l x y = foldl (+) y [1..x*x]
\begin{figure}[t]
\begin{nocode}
mtmDC :: (Trans a, Trans b, Trans c)
         => (a->[[b]]) -> ([[b]]->c) -> [RD a] -> [RD c]
mtmDC f g = parmapDC g . parTransposeDC . parmapDC f

parTransposeDC :: Trans b =>  [RD [[b]]]->[RD[[b]]]
parTransposeDC = allToAllDC (\ n -> unshuffleN n . transpose)
                            (map shuffle . transpose)

-- round robin / segmented distribution
unshuffleN     , splitEvery :: Int -> [a] -> [[a]]
unshuffleN n xs = transpose $ splitEvery n xs
shuffle :: [[a]] -> [a]  -- inverse function
shuffle = concat . transpose
\end{nocode}
\caption{Composition of parmap and transpose skeletons}
\label{parmap-transpose}
\vspace{-2em}
\end{figure}
\ign{
$
\begin{code}
mtmDC :: (Trans a, Trans b, Trans c)
         => (a->[[b]]) -> ([[b]]->c) -> [RD a] -> [RD c]
mtmDC f g = parmapDC g . parTransposeDC . parmapDC f

parTransposeDC :: Trans b =>  [RD [[b]]]->[RD[[b]]]
parTransposeDC = allToAllDC
                 (\ n -> map (:[]) . unshuffleN n . transpose)
                 (map shuffle . transpose . map concat)

-- round robin / segmented distribution
unshuffleN   , splitEvery :: Int -> [a] -> [[a]]
unshuffleN n xs = transpose $ splitEvery n xs
shuffle :: [[a]] -> [a]  -- inverse function
shuffle = concat . transpose

--segmentation
splitEvery n [] = []
splitEvery n xs = let (xs1,xs2) = splitAt n xs
                  in  xs1:(splitEvery n xs2)
\end{code}
$
}

\subsection{Composing Skeletons with Remote Data Interface}
The \cd{allToAllDC} skeleton can be used to express
arbitrary data exchange that requires an all-to-all network. A common special
case is the transposition of a matrix which is distributed over several
processes. 
%The generality of the skeleton's interface is
% useful for the case of matrix transpositions. 
The way the matrix is
distributed over the processes can be manifold. Each process might be assigned
e.g. to one row or --- more general --- to several rows of the matrix. In the example
skeleton \cd{parTransposeDC} of Fig.~\ref{parmap-transpose}, we
implement the more general case. Thus, we are not restricted to 1:1 relations between
rows and processes. We assume that rows are distributed round robin over the
processes. The advantage against a block distribution is that the matrix
can be assigned partially to the processes without knowledge of the overall number of rows.
Hence, the transposition skeleton has to assign the columns of the overall
matrix (rows of the transposed matrix) round robin to the processes. 
The first transformation function of type \cd{Int -> [[b]] -> [[[b]]]} 
first transposes a list of rows to get the list of the former columns.
In a second step, these are round robin distributed to sublists, one for each
process. Process $i$ will consequently receive one row-sliced and column-sliced
partial matrix from each process. The second transformation of type \cd{[[[b]]] -> [[b]]} will shuffle the row-slices (transposed column-slices) into each other to recover the rows of the
overall transposed matrix. This is done by flipping the outer dimension (the list
of partial matrices) with the row-dimension using \cd{transpose}. Thus every outer list
element contains all partial rows belonging to the same row of the overall
matrix. The transformation \cd{map shuffle} re-establishes each row.

Now, we can combine the \cd{parmapDC} skeleton of
Fig.~\ref{code:parmapDC} and the parallel transpose skeleton \cd{parTransposeDC}  
in the function \cd{mtmDC} (cf. Fig.~\ref{parmap-transpose}), a parallel 
version of the function composition 
%\centerline{\cd{map g . transpose . map f},} 
\cd{map g . transpose . map f}
%see function
%\cd{mtmDC} in Fig.~\ref{parmap-transpose}. 
Without remote data a naive parallel implementation would be \\
%To show the efficiency of this
%skeleton composition using remote data, we compare runtime activity profiles of the
%composed skeleton \cd{mtmDC} against one of the following naive version (without remote data interface):\\
\centerline{\cd{parmap g . unshuffleN n . transpose . shuffle .  parmap f}}
This version gathers the data for the intermediate transposition step in the
caller process. 

We compared runtime activity profiles of the \cd{mtmDC} skeleton with the naive version.
In our example executions, the parameter functions \cd{f} and \cd{g} have been set to the dummy 
function \cd{map (scanl1 (+))} which creates rows of prefix sums. The input matrix contained the number 1 in each position.

\begin{figure}[t!]
\begin{center}
\includegraphics[height=.408\textwidth]{Traces/mTmRDTest800_1_1_+RTS_-qN16}
\includegraphics[height=.41\textwidth]{Traces/mTmRDTest800_1_0_+RTS_-qN16}\vspace{1em}\\
\includegraphics[width=.8\textwidth]{Traces/mTmRDTest800_1_1_+RTS_-qN16-ZOOM-p01}
\end{center}
\vspace{-2em}
\caption{Runtime behaviour of the skeleton \cd{mtmDC} in the global view(left),
the zoomed process on Machine 16 (bottom) vs. the local transposition version
(right). \protect\\ (Note the different scaling of the x-axes in the upper traces and that the zoomed view has been taken from a processes-per-machine view, here showing the activity bars of the three processes on Machine~16.)
}
\label{parTranspose}
\end{figure}

%We adjusted the functionality slightly to get runtime traces which are easier to
%interpret. Functions \cd{f} and \cd{g} are modified such that \cd{f} expects an empty
%list as input and generates itself the afore mentioned input matrix made of
%ones. Function \cd{g} evaluates it's result to normal form and returns
%afterwards an empty list. Thus we can focus on communications in the middle part of the composed skeletons.
In order to focus on communications in the middle part of the composed skeletons, input and output communications have been suppressed in the runtime traces underlying the activity profiles. Moreover, the default streaming mode of the communication has been replaced by a single message mode to reduce the number of messages exchanged between the processes. 
%We further changed 
%the communication mode of the transposition was from
%the default streaming mode to a single message for each source, destination
%pair. This reduces the amount of messages drastically and makes it
%easier to comprehend the traces. We do this by nesting the messages in singleton
%lists. 

Each skeleton was instantiated with an input matrix of size $800 \times 800$ and evaluated on 8 Intel Core 2 Duo machines with a Fast Ethernet connection, where each processor core hosted two virtual machines of the Eden runtime system.
%
In Fig.~\ref{parTranspose}\footnote{best viewed in colour}, we present the activity profiles of the corresponding runtime traces for the two skeletons. The trace visualisations show the activity of each machine on a horizontal bar.  The different
activity phases of the virtual machines (runnable, running, blocked) are indicated by different colours explained in the traces legend. Messages are depicted by black lines with an
arrow (black dot) on the receiver side. The $x$-axis shows the time in
seconds.

The upper left trace in Fig.~\ref{parTranspose} clearly reveals the distributed transposition 
by the multitude of messages exchanged right after the
initial data generation phase and the first \cd{map}-phase, which is depicted green in the trace. The exchange of remote data starts very early overlapping the \cd{map}-phase
and forming dense bundles of messages. The second \cd{map}-phase at the end of the program execution is rather short. Note that the overall runtime was less than 0.5 seconds.

We have placed the $i$th process of every skeleton on the same machine, such
that communication costs are low.
%The lower zoomed view shows the three activity phases corresponding to the first map (1), transpose (2) and second map(3) for the three processes located on virtual machine 16. 
The lower zoomed view of the figure shows the activity bars of the three processes located on the virtual machine 16.
%Four processes are depicted, one bar for each. 
The lowest bar belongs to a child of the first \cd{parmapDC}-instantiation. The upper two bars show the processes of the parallel transpose skeleton and the second \cd{parmap} instantiation. 
%One can see the three activity phases corresponding to the first map (-1-), transpose (-2-) and second map(-3-).
%transposer- and the second mapper- process. Such 
With this information, we can easily identify the
different types of messages.
During phase 1 the process of the first \cd{parmapDC} skeleton sends its results to the \cd{parTransposeDC} process. In the second phase the intermediate data is exchanged with the processes on the other machines. Finally, in phase 3, the result of the transposition is passed on to the second \cd{parmapDC} process.
%The root process sends and receives only the remote
%data handles from and to the other processes. Every other process of the
%composed skeleton evaluates its results and sends them to its successor process.
%The messages for the distributed
%transposition are those which are received from or send to
%other machines on the third process.

The upper right trace in Fig.~\ref{parTranspose} belongs to the naive version which performs a local transposition in the root process. As expected, this version is much slower with an overall runtime of approximately 3 seconds. The conspicuously fast
communication between machine 1 and machine 10 is because the two virtual
machines share the same physical machine. Further tests with varying input
sizes (not shown) confirmed the enormous runtime advantages of the distributed version.


\subsection{The \cd{allReduceDC} skeleton}

\begin{figure}[b]
\begin{code}
bitFlipF :: Int -> [a] -> [a]
bitFlipF step xs = (shuffle . flipAtHalfF . unshuffleN d) xs where
  d = (2 ^ step)
  flipAtHalfF xs = let (xs1, xs2) = splitAt (d `div` 2) xs
                   in xs2 ++ xs1
\end{code}
\caption{Flip of values at bit \cd{ldi}}
\label{shiftFlip}
\vspace{-1em}
\end{figure}
The all-reduce skeleton combines distributed data using a binary reduction function. It leaves the result duplicated on all processes involved in the reduction. Usually, it is implemented using the classical butterfly scheme which is also a common way to efficiently
synchronise data between parallel processes. As for the \cd{allToAllDC} skeleton, it is
crucial for the all-reduce skeleton that data is transferred to and from the skeleton in a distributed way. 
The butterfly reduction for $n$ processes is done in $log n$ parallel
communication and local reduction steps. In each step, the communication
partner of process $k$ is usually calculated with the boolean function $k$
\cd{xor} $2^{step-1}$.\\
Fig.~\ref{shiftFlip} shows the definition of the function \cd{bitFlipF} which applies a transformational way to determine the communication partner for the current \cd{step}. The input list \cd{xs} contains at position $j$ the value of process
$j$. \cd{xs} is distributed round robin to \cd{d=(2^step)} sublists.
%In step
%1, a value of index $k$ in the first inner list is exchanged with
%the $k$'th value of the second inner list. They have been neighbours in the
%original list. In general
The values to be exchanged are in the same columns of the transformed matrix.
Their indexes differ by $2^{step-1}$ which equals \cd{d `div` 2} or half the
number of inner lists. We flip the first half of inner lists with the
second half and achieve the desired value exchange. A function call to
\cd{shuffle} re-establishes the original list structure.

\begin{figure}
\begin{code}
allReduceDC :: forall a b. (Trans a, Trans b) =>
             (a -> b) ->            --initial transform function
             (b -> b -> b) ->       --reduce function
             [RD a] -> [RD b]
allReduceDC initF redF rdAs = rdBss !! steps where
  steps = (floor . logBase 2 . fromIntegral . length) rdAs
  rdAs' = take (2^steps) rdAs          --cut input to power of 2

 -- topology, inputs and instantiation
  rdBss = (transpose . spawn procs) inp         --steps in rows
  bufly = zipWith bitFlipF [1..steps] rdBss    --only init rdBss
  inp   = lazy2Zip rdAs' (transpose bufly)     --steps in cols

 -- process functionality and abstraction
  procs = repeat $ process $ uncurry p
  p :: (Trans a, Trans b) => RD a -> [RD b] -> [RD b]
  p rdA theirReds = (releaseAll . scanl1 redF) toReduce where
    toReduce = (initF .  fetch) rdA : fetchAll theirReds'
    theirReds'= lazy2ZipWith (curry snd) [0..steps] theirReds
\end{code}
\caption{The \cd{allReduceDC} skeleton}
\label{allReduce}
\vspace{-1em}
\end{figure}
The \cd{allReduceDC} (see Figure \ref{allReduce}) skeleton uses the function
\cd{bitFlipF} to rearrange lists of remote data in the caller process which
represent the results of the intermediate reduction steps of the skeleton's
processes. The rearranged lists are sent back to the processes. Thus, each process gets the remote values released by one partner in every step. Fetching 
these values establishes the butterfly communication topology.

The skeleton's input is a list with $2^{steps}$ remote data handles\footnote{The \cd{allToAllDC} skeleton only works for input lists where the length is a power of two. Other lists are cut to the next smaller power of two.}. For each handle a process will be instantiated.
The skeleton takes two parameter functions: 
function \cd{initF :: a -> b} is used to transform the initial remote value of
each process after it is fetched. This transformation allows to work with different types for the input values and the reduction function inputs.
%type for the reduction than for the input list. 
The 
reduction function \cd{redF :: b -> b -> b} which should be associative and commutative is applied in each step to the results of the previous step of a process and of its partner. This behaviour can concisely be expressed with \cd{scanl1 redF} applied to the stream \cd{toReduce} of values to be reduced. The stream \cd{toReduce} is composed of the initial value and the stream input \cd{theirReds}. The latter contains the partners'
values for all steps. Note that the complete list structure of \cd{theirReds} is already built
in \cd{theirReds'} even before its first element is received. Thus the request
for all remote values can be eagerly initiated by the function \cd{fetchAll} which would otherwise block on an incomplete list structure.
%It blocks only on the list structure but not on the list elements. 
The result
of the \cd{scanl1} application is elementwise released in every process,
resulting in a list of remote data which is also generated in advance. This
happens because the evaluation of \cd{releaseAll} equally depends only on its
parameter list's structure. Thus the exchange of remote data handles via the root
process can happen in advance, independently of the parallel reduction steps.

The caller process gathers the result streams of all processes in a nested list.
We transpose this list to have all remote values of a
step in each inner list of \cd{rdBss}. Applying the function \cd{bitFlipF}
to the first \cd{steps} lists permutes these according to the butterfly scheme. We transpose this permutation \cd{bufly} such that each process's input is located in one inner
list. This transposed list is lazily zipped with the initially supplied
input list \cd{rdAs} using \cd{lazy2Zip} and passed back to the
processes. The final result consists of the results of the last reduction step, i.e. the last element of the list \cd{rdBss}.

\ign{
\begin{code}
allReduce' :: (Trans a, Trans b) =>
             (a -> b) ->      --initial transform function
             (b -> b -> b) -> --reduce function
             [RD a] -> [RD b]
allReduce' initF redF rdAs = head ress
 where
  steps = ceiling $ logBase 2 $fromIntegral $ length rdAs
  (rdBss,ress)= splitAt steps $ transpose $ spawn procs inp
  procs = repeat $ process p
  inp   = lazy2Zip rdAs (buflyF rdBss)
  buflyF = (transpose . (shiftFlipF' steps) . (fillF steps))
-- p :: (Trans a, Trans b)
-- => (RD a,[Maybe RD b]) -> ([RD b])
  p (rdA,theirReds) = allRedF theirReds
   where
    init = (initF . fetch) rdA
 -- allRedF :: Trans b => [Maybe (RD b)] -> [RD b]
    allRedF = (map fst) . scanl  redF' (release init,init)
 -- redF' :: Trans b => (RD b, b) -> Maybe (RD b) -> (RD b,b)
    redF' t@(_, a) = maybe t (\b->let rAB=redF a (fetch b)
                                  in (release rAB,rAB))
\end{code}



\begin{code}
allRedF :: (Trans b,Show b)=> [Maybe (RD b)] -> b -> [RD b]
allRedF rd init
 = ((map fst) . scanl  (trace  "redF" redF') (release init,init))  rd
redF' :: (Trans b,Show b) => (RD b,b) -> Maybe (RD b) -> (RD b,b)
redF' t@(_, a) = trace ("scan"++show a)
                 maybe t (\b->let rAB=fetch b
                               in trace ("release"++show rAB)(release rAB,rAB))


parmapAtDC :: (Trans a,Trans b) => [Int] -> (a->b) -> [RD a] -> [RD b]
parmapAtDC pos f xs = spawnAt pos pfs xs
    where pfs = repeat (processDC f)

parmapAt :: (Trans a,Trans b) => [Int] -> (a->b) -> [a] -> [b]
parmapAt pos f xs = spawnAt pos pfs xs
    where pfs = repeat (process f)
--Auxiliary functions:

--Fill rows to the power of ldn with Nothing, map Just to the rest
fillF :: Int -> [[a]] -> [[Maybe a]]
fillF ldn ass = map fillRow ass
  where
    n = 2 ^ ldn
    fillRow as = take n $ (map Just as) ++ (repeat Nothing)

shiftFlipF' :: Int -> [[a]] -> [[a]]
shiftFlipF' ldn rdBss = zipWith shiftFlipRow [1..ldn] rdBss
  where
    shiftFlipRow ldi rdBs = (shuffle . flipAtHalfF . unshuffleN i) rdBs
      where
       i = 2 ^ ldi
       flipAtHalfF xs = let (xs1, xs2) = splitAt (i`div`2) xs in xs2++xs1


transpose' :: [[a]] -> [[a]]
transpose' []       = []
transpose' (xs:[]) = (transposeRow xs)
transpose' (xs:xss) = lazy2ZipWith (++) (transposeRow xs) (transpose' xss)

lazy2ZipWith :: (a->b->c) -> [a] -> [b] -> [c]
lazy2ZipWith f [] _ = []
lazy2ZipWith f (x:xs) ~(y:ys) = (f x y) : (lazy2ZipWith f xs ys)

transposeRow []     = []
transposeRow (x:xs) = [x]:(transposeRow xs)
\end{code}
}
\ign{$}


\begin{figure}
\begin{center}
\includegraphics[width=.75\textwidth]{Traces/allReduceTest_200000_-qN9_-qQ8M_-qPm_-RTS}
\end{center}
\vspace{-1em}
\caption{Runtime behaviour of the \cd{allReduceDC} skeleton}
\label{allReduceTrace}
\end{figure}

We have tested the \cd{allReduceDC} skeleton with a dummy example which we executed on an 8 core Intel Xeon machine. 
%, we chose it for its immaculate appearance on the trace picture. 
%Similar results have been observed on
%distributed systems but are not of such synchronicity and the topology is not
%that easy to recognise. 
The initial transformation function \cd{initF} serves as
generator and generates the list \cd{[1..nElems]}, where \cd{nElems} is a
parameter of the program and in our example set to 200000. The trace visualisation in
Fig.~\ref{allReduceTrace} reveals interchanging computation and communication phases. The butterfly interconnection scheme can clearly be recognised in the messages exchanged between the processes. 
The generation of elements is depicted as the first
green phase.
The reduction network has been set up before, by exchanging the
remote data messages via the root process on Machine~1 (initial messages).
Three reduction phases
follow. First the direct neighbours exchange their lists leading to the typical
butterfly pattern of messages. The processes reduce their lists using the
reduction function \cd{redF} which is set to \cd{zipWith (+)}. For the next
steps, 
%the step size for the data exchange is doubled at a time. 
the distance to the partner process is doubled every time.
Finally, a
\cd{parmapDC} skeleton is called to consume the data and return an empty list to
the root process.



