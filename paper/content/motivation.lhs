%\section{Motivation}
%Arrows were introduced in John Hughes paper as a general interface for computation and therefore as an alternative to monads for API design \citHughes. In the paper Hughes describes how arrows are a generalization of monads and how they are not as restrictive. In this paper we will use this concept to express parallelism.

\section{Introduction}
\label{sec:introduction}
\olcomment{todo, reuse 5.5, and more}

blablabla arrows, parallel, haskell.

\paragraph{Contribution}

\olcomment{HIT HERE REALLY STRONG}

%\subsection{Impact of parallel Arrows}
%\olcomment{move this to Contributions in the front or something}
We wrap parallel Haskells inside of our |ArrowParallel| interface, but why do we aim to abstract parallelism this way and what does this approach do better than the other parallel Haskells?
\begin{itemize}
	\item \textbf{Arrow API benefits}:
	With the |ArrowParallel| typeclass we do not lose any benefits of using arrows as |parEvalN| is just yet another arrow combinator. The resulting arrow can be used in the same way a potential serial version could be used. This is a big advantage of this approach, especially compared to the monad solutions as we do not introduce any new types. We can just \enquote{plug} in parallel parts into our sequential programs without having to change anything.
	\item \textbf{Abstraction}:
	With the |ArrowParallel| typeclass, we abstracted all parallel implementation logic away from the business logic. This gives us the beautiful situation of being able to write our code against the interface the typeclass gives us without being bound to any parallel Haskell. So as an example, during development, we can run the code on the simple Multicore version and afterwards deploy it on a cluster by converting it into an Eden version, by just replacing the actual |ArrowParallel| instance.
\end{itemize}


\paragraph{Structure}
The remaining text is structures as follows. Section~\ref{sec:background} briefly introduces known parallel Haskell flavours and gives an overview of Arrows to the reader (Sec.~\ref{sec:arrows}). Section~\ref{sec:related-work} discusses related work. Section~\ref{sec:parallel-arrows} defines Parallel Arrows and presents a basic interface. Section~\ref{futures} defines Futures for Parallel Arrows, this concept enables better communication. Section~\ref{sec:map-skeletons} presents some basic algorithmic skeletons (parallel |map| with and without load balancing, |map-reduce|) in our newly defined dialect. More advanced ones are showcased in Section~\ref{sec:topology-skeletons} (|pipe|, |ring|, |torus|). Section~\ref{sec:benchmarks} shows the benchmark results. Section~\ref{sec:conclusion} discusses future work and concludes.

%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
