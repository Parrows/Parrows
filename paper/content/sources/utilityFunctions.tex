\section{Utility Functions}\label{utilfns}
To be able to go into detail on parallel arrows, we introduce some utility combinators first, that will help us later: |map|, |foldl| and |zipWith| on arrows.

The |mapArr| combinator (Fig.~\ref{fig:mapArr}) lifts any arrow |arr a
b| to an arrow |arr [a] [b]| \cite{programming_with_arrows}. 
Similarly, we can also define |foldlArr| (Fig.~\ref{fig:foldlArr}) that lifts any arrow |arr (b, a) b| with a neutral element |b| to |arr [a] b|.

\begin{figure}[htb]
\begin{code}
mapArr :: ArrowChoice arr => arr a b -> arr [a] [b]
mapArr f =
	arr listcase >>>
	arr (const []) ||| (f *** mapArr f >>> arr (uncurry (:)))

listcase [] = Left ()
listcase (x:xs) = Right (x,xs)
\end{code}
\caption{The definition of |map| over Arrows and the |listcase| helper
function.}
\label{fig:mapArr}
\end{figure}
\begin{figure}[htb]
\begin{code}
foldlArr :: (ArrowChoice arr, ArrowApply arr) => arr (b, a) b -> b -> arr [a] b
foldlArr f b =
	arr listcase >>>
	arr (const b) |||
		(first (arr (\a -> (b, a)) >>> f >>> arr (foldlArr f)) >>> app)
\end{code}
\caption{The definition of |foldl| over Arrows.}
\label{fig:foldlArr}
\end{figure}
%\fixme{pipepipepipe does not work with lhs2TeX}

Finally, with the help of |mapArr| (Fig.~\ref{fig:mapArr}), we can define |zipWithArr| that lifts any arrow |arr (a, b) c| to an arrow |arr ([a], [b]) [c]|.
%\begin{figure}[htb]
\begin{code}
zipWithArr :: ArrowChoice arr => arr (a, b) c -> arr ([a], [b]) [c]
zipWithArr f = (arr $ \(as, bs) -> zipWith (,) as bs) >>> mapArr f
\end{code}
% \caption{|zipWith| over arrows}
% \label{fig:zipWithArr}
% \end{figure}
 %$ %% formatting
These combinators make use of the |ArrowChoice| type class which provides the \pipepipepipe\olcomment{CHECK!} combinator. It takes two arrows |arr a c| and |arr b c| and combines them into a new arrow |arr (Either a b) c| which pipes all |Left a|'s to the first arrow and all |Right b|'s to the second arrow.
% \begin{figure}[htb]
\begin{code}
(|||) :: ArrowChoice arr a c -> arr b c -> arr (Either a b) c
\end{code}
% \caption{Type signature of \pipepipepipe}
% \label{fig:codeSigPipePipePipe}
% \end{figure}

With the zipWithArr combinator we can also write a combinator |listApp|, that lifts a list of arrows |[arr a b]| to an arrow |arr [a] [b]|.
% \begin{figure}[htb]
\begin{code}
listApp :: (ArrowChoice arr, ArrowApply arr) => [arr a b] -> arr [a] [b]
listApp fs = (arr $ \as -> (fs, as)) >>> zipWithArr app
\end{code}
% \caption{Definition of |listApp|}
% \label{fig:listApp}
% \end{figure}
% $ %% formatting
Note that  this additionally makes use of the |ArrowApply| typeclass that allows us to evaluate arrows with |app :: arr (arr a b, a) c|.

% $ %% formatting

\section{Omitted Funtion Definitions}

We have omitted some function definitions in the main text for
brevity, and redeem this here.
%
We warp Eden's build-in Futures in PArrows as in
Figure~\ref{fig:RDFuture}.
The full definition of |farmChunk| is in Figure~\ref{fig:farmChunk}.
Eden definition of |ring| skeleton following \citep{Loogen2012} is in Figure~\ref{fig:ringEden}.


\begin{figure}[htb]
\begin{code}
data RemoteData a = RD { rd :: RD a }

instance (Trans a) => Future RemoteData a where
    put = arr (\a -> RD { rd = release a })
    get = arr rd >>> arr fetch
\end{code}
\caption{|RD|-based |RemoteData| version of |Future| for the Eden backend.}
\label{fig:RDFuture}
\end{figure}


\begin{figure}[htb]
\begin{code}
farmChunk :: (ArrowParallel arr a b conf, ArrowParallel arr [a] [b] conf, 
             ArrowChoice arr, ArrowApply arr) =>
	conf -> ChunkSize -> NumCores -> arr a b -> arr [a] [b]
farmChunk conf chunkSize numCores f =
	unshuffle numCores >>>
	parEvalNLazy conf chunkSize (repeat (mapArr f)) >>>
	shuffle
\end{code}
\caption{Definition of |farmChunk|.}
\label{fig:farmChunk}
\end{figure}


\begin{figure}[htb]
\begin{code}
ringSimple :: (Trans i, Trans o, Trans r) => (i -> r -> (o,r)) -> [i] -> [o]
ringSimple f is =  os
  where (os,ringOuts) = unzip (parMap (toRD $ uncurry f) (zip is $ lazy ringIns))
        ringIns = rightRotate ringOuts

toRD :: (Trans i, Trans o, Trans r) => ((i,r) -> (o,r)) -> ((i, RD r) -> (o, RD r))
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
