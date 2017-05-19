
\section{Conclusion}
\label{sec:conclusion}
Arrows are a generic concept that allows for powerful composition combinators. To our knowledge we are the first ones to represent parallel computation with arrows.\olcomment{that strange arrows-based robot interaction paper from 1993 or so! clearly discuss in related work} \done

Arrows turn out to be a useful tool for composing in parallel programs. We do not have to introduce new monadic types that wrap the computation. Instead use arrows just like regular sequential pure functions. 
%
This work features multiple parallel backends: the already available parallel Haskell flavours. Parallel Arrows feature an implementation of the |ArrowParallel| interface for Multicore Haskell, |Par| Monad, and Eden. With our approach parallel programs can be ported across these flavours with no effort.
%
%
Performancewise, Parallel Arrows are on par with existing parallel Haskells, as they do not introduce any notable overhead.\olcomment{PROOFS. Many proofs in benchmarks!}

\mbcomment{ArrowApply (or equivalent) are needed because we basically want to be able to produce intermediary results, this is by definition of the parallel evaluation combinators}

\olcomment{Remove websites from citations, put them into footnotes!}

\olcomment{Parrows + accelerate = love? Metion port to Frege. Mention the Par monad troubles.}


\subsection{Future Work}
\label{sec:future-work}

Our PArrows interface can be expanded to futher parallel Haskells. More specifically we target HdpH \cite{Maier:2014:HDS:2775050.2633363} a modern distributed Haskell that would benefit from our Arrows notation. Future-aware special versions of Arrow combinated can be extended and further improved. We would look into more transparency of the API, it should basically infuse as little overhead as possible.

Of course, definitions of further skeletons are viable and needed. We are looking into more experiences with seamless porting of parallel PArrow-based programs across the backends.

Accelerate~\cite{Chakravarty:2011:AHA:1926354.1926358} is not related to our approach. It would be interesting to see a hybrid of both APIs.

\olcomment{replace API with DSL globally?}
