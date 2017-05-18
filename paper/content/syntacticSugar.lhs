\subsubsection{Syntactic Sugar} \label{syntacticSugar}
For basic arrows, we have the \inlinecode{(***)} combinator (Fig.~\ref{fig:***Img},~\ref{fig:***}) which allows us to combine two arrows \inlinecode{arr a b} and \inlinecode{arr c d} into an arrow \inlinecode{arr (a, c) (b, d)} which does both computations at once. This can easily be translated into a parallel version \inlinecode{(|***|)} (Fig.~\ref{fig:par***}) with the use of \inlinecode{parEval2}, but for this we require a backend which has an implementation that does not require any configuration (hence the \inlinecode{()} as the conf parameter in Fig.~\ref{fig:par***}).
\begin{figure}[h]
\begin{code}
(|***|) :: (ArrowChoice arr, ArrowParallel arr (Either a c) (Either b d) ())) =>
	arr a b -> arr c d -> arr (a, c) (b, d)
(|***|) = parEval2 ()
\end{code}
\caption{Definition of (|***|) - the parallel version of (***)}
\label{fig:par***}
\end{figure}
% With this we can analogously to the serial \inlinecode{&&&}
We define the parallel \inlinecode{(|\&\&\&|)} (Fig.~\ref{fig:par&&&}) in a similar manner to its sequential pendant \inlinecode{(\&\&\&)} (Fig.~\ref{fig:&&&Img},~\ref{fig:&&&}).
\begin{figure}[h]
\begin{code}
(|&&&|) :: (ArrowChoice arr, ArrowParallel arr (Either a a) (Either b c) ()) =>
	arr a b -> arr a c -> arr a (b, c)
(|&&&|) f g = (arr $ \a -> (a, a)) >>> f |***| g
\end{code} % $ %% formatting
\caption{Definition of (|\&\&\&|) - the parallel version of (\&\&\&)}
\label{fig:par&&&}
\end{figure}
