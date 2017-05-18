\section{Utility Functions}\label{utilfns}
To be able to go into detail on parallel arrows, we introduce some utility combinators first, that will help us later: \inlinecode{map}, \inlinecode{foldl} and \inlinecode{zipWith} on arrows.

The \inlinecode{mapArr} combinator (Fig.~\ref{fig:mapArr}) lifts any arrow \inlinecode{arr a b} to an arrow \inlinecode{arr [a] [b]} \cite{programming_with_arrows}:
\begin{figure}[h]
\begin{code}
mapArr :: ArrowChoice arr => arr a b -> arr [a] [b]
mapArr f =
	arr listcase >>>
	arr (const []) pipepipepipe (f *** mapArr f >>> arr (uncurry (:)))
	where listcase [] = Left ()
	      listcase (x:xs) = Right (x,xs)
\end{code}
\caption{\inlinecode{map} over arrows}
\label{fig:mapArr}
\end{figure}
Similarly, we can also define \inlinecode{foldlArr} (Fig.~\ref{fig:foldlArr}) that lifts any arrow \inlinecode{arr (b, a) b} with a neutral element \inlinecode{b} to \inlinecode{arr [a] b}:
\begin{figure}[h]
\begin{code}
foldlArr :: (ArrowChoice arr, ArrowApply arr) => arr (b, a) b -> b -> arr [a] b
foldlArr f b =
	arr listcase >>>
	arr (const b) pipepipepipe
		(first (arr (\a -> (b, a)) >>> f >>> arr (foldlArr f)) >>> app)
	where listcase [] = Left []
	      listcase (x:xs) = Right (x,xs)
\end{code}
\caption{\inlinecode{foldl} over arrows}
\label{fig:foldlArr}
\end{figure}
\fixme{pipepipepipe does not work with lhs2TeX}
Finally, with the help of \inlinecode{mapArr} (Fig.~\ref{fig:mapArr}), we can define \lstinline{zipWithArr} (Fig.~\ref{fig:zipWithArr}) that lifts any arrow \inlinecode{arr (a, b) c} to an arrow \inlinecode{arr ([a], [b]) [c]}.
\begin{figure}[h]
\begin{code}
zipWithArr :: ArrowChoice arr => arr (a, b) c -> arr ([a], [b]) [c]
zipWithArr f = (arr $ \(as, bs) -> zipWith (,) as bs) >>> mapArr f
\end{code}
\caption{\inlinecode{zipWith} over arrows}
\label{fig:zipWithArr}
\end{figure}
 %$ %% formatting
These combinators make use of the \inlinecode{ArrowChoice} type class which provides the \inlinecode{(\textpipe\textpipe\textpipe)} combinator. It takes two arrows \inlinecode{arr a c} and \inlinecode{arr b c} and combines them into a new arrow \inlinecode{arr (Either a b) c} which pipes all \inlinecode{Left a}'s to the first arrow and all \inlinecode{Right b}'s to the second arrow.
\begin{figure}[h]
\begin{code}
(@pipepipepipe@) :: ArrowChoice arr a c -> arr b c -> arr (Either a b) c
\end{code}
\caption{Type signature of \inlinecode{(\textpipe\textpipe\textpipe)}}
\label{fig:codeSigPipePipePipe}
\end{figure}
\fixme{pipepipepipe does not work with lhs2TeX}

With the zipWithArr combinator we can also write a combinator \inlinecode{listApp} (Fig.~\ref{fig:listApp}), that lifts a list of arrows \inlinecode{[arr a b]} to an arrow \inlinecode{arr [a] [b]}.
\begin{figure}[h]
\begin{code}
listApp :: (ArrowChoice arr, ArrowApply arr) => [arr a b] -> arr [a] [b]
listApp fs = (arr $ \as -> (fs, as)) >>> zipWithArr app
\end{code}
\caption{Definition of \inlinecode{listApp}}
\label{fig:listApp}
\end{figure}
% $ %% formatting
. Note that  this additionally makes use of the \inlinecode{ArrowApply} typeclass that allows us to evaluate arrows with \inlinecode{app :: arr (arr a b, a) c}.

% $ %% formatting


%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
