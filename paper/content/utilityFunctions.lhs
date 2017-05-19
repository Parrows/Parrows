\section{Utility Functions}\label{utilfns}
To be able to go into detail on parallel arrows, we introduce some utility combinators first, that will help us later: |map|, |foldl| and |zipWith| on arrows.

The |mapArr| combinator (Fig.~\ref{fig:mapArr}) lifts any arrow |arr a b| to an arrow |arr [a] [b]| \cite{programming_with_arrows}:
\begin{figure}[h]
\begin{code}
mapArr :: ArrowChoice arr => arr a b -> arr [a] [b]
mapArr f =
	arr listcase >>>
	arr (const []) ||| (f *** mapArr f >>> arr (uncurry (:)))
	where listcase [] = Left ()
	      listcase (x:xs) = Right (x,xs)
\end{code}
\caption{|map| over arrows}
\label{fig:mapArr}
\end{figure}
Similarly, we can also define |foldlArr| (Fig.~\ref{fig:foldlArr}) that lifts any arrow |arr (b, a) b| with a neutral element |b| to |arr [a] b|:
\begin{figure}[h]
\begin{code}
foldlArr :: (ArrowChoice arr, ArrowApply arr) => arr (b, a) b -> b -> arr [a] b
foldlArr f b =
	arr listcase >>>
	arr (const b) |||
		(first (arr (\a -> (b, a)) >>> f >>> arr (foldlArr f)) >>> app)
	where listcase [] = Left []
	      listcase (x:xs) = Right (x,xs)
\end{code}
\caption{|foldl| over arrows}
\label{fig:foldlArr}
\end{figure}
\fixme{pipepipepipe does not work with lhs2TeX}
Finally, with the help of |mapArr| (Fig.~\ref{fig:mapArr}), we can define |zipWithArr| (Fig.~\ref{fig:zipWithArr}) that lifts any arrow |arr (a, b) c| to an arrow |arr ([a], [b]) [c]|.
\begin{figure}[h]
\begin{code}
zipWithArr :: ArrowChoice arr => arr (a, b) c -> arr ([a], [b]) [c]
zipWithArr f = (arr $ \(as, bs) -> zipWith (,) as bs) >>> mapArr f
\end{code}
\caption{|zipWith| over arrows}
\label{fig:zipWithArr}
\end{figure}
 %$ %% formatting
These combinators make use of the |ArrowChoice| type class which provides the \pipepipepipe\olcomment{CHECK!} combinator. It takes two arrows |arr a c| and |arr b c| and combines them into a new arrow |arr (Either a b) c| which pipes all |Left a|'s to the first arrow and all |Right b|'s to the second arrow.
\begin{figure}[h]
\begin{code}
(|||) :: ArrowChoice arr a c -> arr b c -> arr (Either a b) c
\end{code}
\caption{Type signature of \pipepipepipe}
\label{fig:codeSigPipePipePipe}
\end{figure}
\fixme{pipepipepipe does not work with lhs2TeX}\olcomment{trying
  direct one}

With the zipWithArr combinator we can also write a combinator |listApp| (Fig.~\ref{fig:listApp}), that lifts a list of arrows |[arr a b]| to an arrow |arr [a] [b]|.
\begin{figure}[h]
\begin{code}
listApp :: (ArrowChoice arr, ArrowApply arr) => [arr a b] -> arr [a] [b]
listApp fs = (arr $ \as -> (fs, as)) >>> zipWithArr app
\end{code}
\caption{Definition of |listApp|}
\label{fig:listApp}
\end{figure}
% $ %% formatting
. Note that  this additionally makes use of the |ArrowApply| typeclass that allows us to evaluate arrows with |app :: arr (arr a b, a) c|.

% $ %% formatting

\subsection{Omitted Funtion Definitions}

We have omitted some function definitions in the main text for
brevity, and redeem this here.
%
We warp Eden's build-in Futures in PArrows as in Figure~\ref{fig:RDFuture}.

\begin{figure}[h]
\begin{code}
data RemoteData a = RD { rd :: RD a }

instance (Trans a) => Future RemoteData a where
    put = arr (\a -> RD { rd = release a })
    get = arr rd >>> arr fetch
\end{code}
\caption{|RD|-based |RemoteData| version of |Future| for the Eden backend.}
\label{fig:RDFuture}
\end{figure}

The full definition of |farmChunk| is in Figure~\ref{fig:farmChunk}.

\begin{figure}[h]
\begin{code}
farmChunk :: (ArrowParallel arr a b conf, ArrowParallel arr [a] [b] conf, ArrowChoice arr, ArrowApply arr) =>
	conf -> ChunkSize -> NumCores -> arr a b -> arr [a] [b]
farmChunk conf chunkSize numCores f =
	unshuffle numCores >>>
	parEvalNLazy conf chunkSize (repeat (mapArr f)) >>>
	shuffle
\end{code}
\caption{Definition of |farmChunk|.}
\label{fig:farmChunk}
\end{figure}

Eden definition of |ring| skeleton is in Figure~\ref{fig:ringEden}.

\begin{figure}[h]
\begin{code}
ringSimple :: (Trans i, Trans o, Trans r) =>
   (i -> r -> (o,r)) -> [i] -> [o]
ringSimple f is =  os
  where (os,ringOuts) = unzip (parMap (toRD $ uncurry f) (zip is $ lazy ringIns))
        ringIns = rightRotate ringOuts

toRD :: (Trans i, Trans o, Trans r) =>
        ((i,r) -> (o,r)) -> ((i, RD r) -> (o, RD r))
toRD  f (i, ringIn)  = (o, release ringOut)
  where (o, ringOut) = f (i, fetch ringIn)

rightRotate    :: [a] -> [a]
rightRotate [] =  []
rightRotate xs =  last xs : init xs

lazy :: [a] -> [a]
lazy ~(x:xs) = x : lazy xs
\end{code}
\caption{Eden's definition of the |ring| skeleton.}
\label{fig:ringEden}
\end{figure}



%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
