\section{Parallel Arrows}
\label{sec:parallel-arrows}
We have seen what Arrows are and how they can be used as a general interface to computation. In the following section we will discuss how Arrows constitute a general interface not only to computation, but to \textbf{parallel computation} as well. We start by introducing the interface and explaining the reasonings behind it. Then, we discuss some implementations using exisiting parallel Haskells. Finally, we explain why using Arrows for expressing parallelism is beneficial.
\subsection{The ArrowParallel typeclass}
As we have seen earlier, in its purest form, parallel computation (on functions) can be seen as the execution of some functions \inlinecode{a -> b} in parallel, \inlinecode{parEvalN} (Fig.~\ref{fig:parEvalNTypeSig},~\ref{fig:parEvalN}).
Translating this into arrow terms gives us a new operator \inlinecode{parEvalN} that lifts a list of arrows \inlinecode{[arr a b]} to a parallel arrow \inlinecode{arr [a] [b]} (Fig.~\ref{fig:parEvalNArrowFn}) (This combinator is similar to our utility function \lstinline{listApp} from Appendix \ref{utilfns}, but does parallel instead of serial evaluation).
\begin{figure}[h]
\begin{code}
parEvalN :: (Arrow arr) => [arr a b] -> arr [a] [b]
\end{code}
\caption{parEvalN Arrow combinator as a function}
\label{fig:parEvalNArrowFn}
\end{figure}
With this definition of \inlinecode{parEvalN}, parallel execution is yet another arrow combinator. But as the implementation may differ depending on the actual type of the arrow \inlinecode{arr} and we want this to be an interface for different backends, we introduce a new typeclass \inlinecode{ArrowParallel arr a b} to host this combinator (Fig.~\ref{fig:parEvalNArrowTypeClass1}).
\begin{figure}[h]
\begin{code}
class Arrow arr => ArrowParallel arr a b where
	parEvalN :: [arr a b] -> arr [a] [b]
\end{code}
\caption{parEvalN Arrow combinator in a first version of the ArrowParallel typeclass}
\label{fig:parEvalNArrowTypeClass1}
\end{figure}
Sometimes parallel Haskells require or allow for additional configuration parameters, \eg an information about the execution environment or the level of evaluation (weak-head normalform vs. normalform). For this reason we also introduce an additional \inlinecode{conf} parameter to the function. We also do not want \inlinecode{conf} to be a fixed type, as the configuration parameters can differ for different instances of \inlinecode{ArrowParallel}. So we add it to the type signature of the typeclass as well and get \inlinecode{ArrowParallel arr a b conf} (Fig.~\ref{fig:parEvalNArrowTypeClassFinal}).
\begin{figure}[h]
\begin{code}
class Arrow arr => ArrowParallel arr a b conf where
	parEvalN :: conf -> [arr a b] -> arr [a] [b]
\end{code}
\caption{parEvalN Arrow combinator in the final version of the ArrowParallel typeclass}
\label{fig:parEvalNArrowTypeClassFinal}
\end{figure}
Note that we don't require the \inlinecode{conf} parameter in every implementation. If it is not needed, we usually just default the \inlinecode{conf} type parameter to () and even blank it out in the parameter list of the implemented \inlinecode{parEvalN}, as we will see in the implementation of the Multicore and the ParMonad backend.

\subsection{ArrowParallel instances}

\subsubsection{Multicore Haskell} \label{sec:parrows:multicore}
The Multicore Haskell implementation of this class is implemented in a straightforward manner by using listApp from appendix \ref{utilfns} combined with the \inlinecode{withStrategy :: Strategy a -> a -> a} and \inlinecode{pseq :: a -> b -> b} combinators from Multicore Haskell, where \inlinecode{withStrategy} is the same as \inlinecode{using :: a -> Strategy a -> a} but with flipped parameters.
\begin{figure}[h]
\begin{code}
instance (NFData b, ArrowApply arr, ArrowChoice arr) =>
	ArrowParallel arr a b () where
    	parEvalN _ fs =
       		listApp fs >>>
        	arr (withStrategy (parList rdeepseq)) &&& arr id >>>
        	arr (uncurry pseq)
\end{code}% $ %% formatting
\caption{Fully evaluating ArrowParallel instance for the Multicore Haskell backend}
\label{fig:ArrowParallelMulticoreRdeepseq}
\end{figure}
For most cases a fully evaluating version like in Fig.~\ref{fig:ArrowParallelMulticoreRdeepseq} would probably suffice, but as the Multicore Haskell interface allows the user to specify the level of evaluation to be done via the \inlinecode{Strategy} interface, we want to the user not to lose this ability because of using our API. We therefore introduce the \inlinecode{Conf a} data-type that simply wraps a \inlinecode{Strategy a} (Fig.~\ref{fig:confa}). We can't directly use the \inlinecode{Strategy a} type here as GHC (at least in the versions used for development in this paper) does not allow type synonyms in type class instances.
\begin{figure}[h]
\begin{code}
data Conf a = Conf (Strategy a)
\end{code}
\caption{Definition of Conf a}
\label{fig:confa}
\end{figure}
To get our configurable \inlinecode{ArrowParallel} instance, we simply unwrap the strategy and pass it to \inlinecode{parList} like in the fully evaluating version (Fig.~\ref{fig:ArrowParallelMulticoreConfigurable}).
\begin{figure}[h]
\begin{code}
instance (NFData b, ArrowApply arr, ArrowChoice arr) =>
	ArrowParallel arr a b (Conf b) where
    	parEvalN (Conf strat) fs =
        	listApp fs >>>
        	arr (withStrategy (parList strat)) &&& arr id >>>
        	arr (uncurry pseq)
\end{code}
\caption{Configurable ArrowParallel instance for the Multicore Haskell backend}
\label{fig:ArrowParallelMulticoreConfigurable}
\end{figure}
\subsubsection{Par Monad}
The ParMonad implementation (Fig.~\ref{fig:ArrowParallelParMonad}) makes use of Haskells laziness and ParMonad's \inlinecode{spawnP :: NFData a =>\ \ a -> Par (IVar a)} function. The latter forks away the computation of a value and returns an \inlinecode{IVar} containing the result in the \inlinecode{Par} monad.


We therefore apply each function to its corresponding input value with \inlinecode{listApp} (Fig.~\ref{fig:listApp}) and then fork the computation away with \inlinecode{arr spawnP} inside a \inlinecode{zipWithArr} call. This yields a list \inlinecode{[Par (IVar b)]}, which we then convert into \inlinecode{Par [IVar b]} with \inlinecode{arr sequenceA}. In order to wait for the computation to finish, we map over the \inlinecode{IVar}s inside the ParMonad with \inlinecode{arr (>>= mapM get)}. The result of this operation is a \inlinecode{Par [b]} from which we can finally remove the monad again by running \inlinecode{arr runPar} to get our output of \inlinecode{[b]}.
\begin{figure}[h]
\begin{code}
instance (NFData b, ArrowApply arr, ArrowChoice arr) =>
	ArrowParallel arr a b conf where
		parEvalN _ fs = 
			(arr $ \as -> (fs, as)) >>>
			zipWithArr (app >>> arr spawnP) >>>
			arr sequenceA >>>
			arr (>>= mapM get) >>>
			arr runPar
\end{code} %$ %% formatting
\caption{ArrowParallel instance for the Par Monad backend}
\label{fig:ArrowParallelParMonad}
\end{figure}

\subsubsection{Eden}
For the Multicore and ParMonad implementation we could use general instances of \inlinecode{ArrowParallel} that just require the \inlinecode{ArrowApply} and \inlinecode{ArrowChoice} typeclasses. With Eden this is not the case as we can only spawn a list of functions and we cannot extract simple functions out of arrows. While we could still manage to have only one class in the module by introducing a typeclass \inlinecode{ArrowUnwrap} (Fig.~\ref{fig:ArrowUnwrap}).
\begin{figure}[h]
\begin{code}
class (Arrow arr) => ArrowUnwrap arr where
	arr a b -> (a -> b)
\end{code}
\caption{possible ArrowUnwrap typeclass}
\label{fig:ArrowUnwrap}
\end{figure}
We don't do it here for aesthetic resons, though. For now, we just implement \inlinecode{ArrowParallel} for normal functions (Fig.~\ref{fig:ArrowParallelEdenFns})
\begin{figure}[h]
\begin{code}
instance (Trans a, Trans b) => ArrowParallel (->) a b conf where
parEvalN _ fs as = spawnF fs as
\end{code}
\caption{ArrowParallel instance for functions in the Eden backend}
\label{fig:ArrowParallelEdenFns}
\end{figure}
and the Kleisli type (Fig.~\ref{fig:ArrowParallelEdenKleisli}).
\begin{figure}[h]
\begin{code}
instance (Monad m, Trans a, Trans b, Trans (m b)) =>
	ArrowParallel (Kleisli m) a b conf where
parEvalN conf fs =
	(arr $ parEvalN conf (map (\(Kleisli f) -> f) fs)) >>>
	(Kleisli $ sequence)
\end{code}
\caption{ArrowParallel instance for the Kleisli type in the Eden backend}
\label{fig:ArrowParallelEdenKleisli}
\end{figure}

\FloatBarrier

\subsection{Impact of parallel Arrows}
\olcomment{move this to Contributions in the front or something}
We have seen that we can wrap parallel Haskells inside of the \inlinecode{ArrowParallel} interface, but why do we abstract parallelism this way and what does this approach do better than the other parallel Haskells?
\begin{itemize}
	\item \textbf{Arrow API benefits}:
	With the \inlinecode{ArrowParallel} typeclass we do not lose any benefits of using arrows as \inlinecode{parEvalN} is just yet another arrow combinator. The resulting arrow can be used in the same way a potential serial version could be used. This is a big advantage of this approach, especially compared to the monad solutions as we do not introduce any new types. We can just \enquote{plug} in parallel parts into our sequential programs without having to change anything.
	\item \textbf{Abstraction}:
	With the \inlinecode{ArrowParallel} typeclass, we abstracted all parallel implementation logic away from the business logic. This gives us the beautiful situation of being able to write our code against the interface the typeclass gives us without being bound to any parallel Haskell. So as an example, during development, we can run the code on the simple Multicore version and afterwards deploy it on a cluster by converting it into an Eden version, by just replacing the actual \inlinecode{ArrowParallel} instance.
\end{itemize}


%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
