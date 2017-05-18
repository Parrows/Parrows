\section{Related Work}
\label{sec:related-work}

\olcomment{arrows or Arrows?}

\subsection{Parallel Haskells}
Of course, the three parallel Haskell flavours we have presented above: the GpH \cite{Trinder1998a,Trinder1999} parallel Haskell dialect and its multicore version \cite{Marlow2009}, the |Par| monad \cite{par-monad,Foltzer:2012:MPC:2398856.2364562}, and Eden \cite{eden,Loogen2012} are related to this work. We use these languages as backends: our library can switch from one to other at user's command.

HdpH \cite{Maier:2014:HDS:2775050.2633363,stewart_maier_trinder_2016} is an extension of |Par| monad to heterogeneous clusters. LVish \cite{Kuper:2014:TPE:2666356.2594312} is a communication-centred extension of |Par| monad.
%
Further parallel Haskell approaches include pH \cite{ph-book}, research work done on distributed variants of GpH \cite{Trinder1996,Aljabri:2013:DIG:2620678.2620682,Aljabri2015} and low-level Eden implementation \cite{JostThesis,berthold_loidl_hammond_2016}. Skeleton composition \cite{dieterle_horstmeyer_loogen_berthold_2016}, communication \cite{Dieterle2010}, and generation of process networks \cite{Horstmeyer2013} are recent in-focus research topics in Eden. This also includes the definitions of new skeletons \cite{doi:10.1142/S0129626403001380,Eden:PARCO05,Berthold2009-mr,Berthold2009-fft,dieterle2010skeleton,delaEncina2011,Dieterle2013,janjic2013space}.

More different approaches include data parallelism \cite{Chakravarty2007,Keller:2010:RSP:1932681.1863582,}, GPU-based approaches \cite{Mainland:2010:NEC:2088456.1863533,obsidian-phd}, software transactional memory \cite{Harris:2005:CMT:1065944.1065952,Perfumo:2008:LST:1366230.1366241}.
%
The Haskell--GPU bridge Accelerate \cite{Chakravarty:2011:AHA:1926354.1926358,CMCK14,McDonell:2015:TRC:2887747.2804313} deserves a special mention. Accelerate is completely orthogonal to our approach. \citeauthor{marlow2013parallel} authored a recent book \citeyear{marlow2013parallel} on parallel Haskells.

\subsection{Algorithmic skeletons}

Algorithmic skeletons were introduced by \citet{Cole1989}.
Early efforts include \cite{darlington1993parallel,botorog1996efficient,p3l97,Gorlatch1998,Lengauer1997}. \citet{SkeletonBook} consolidated early reports on high-level programming approaches.
The effort is ongoing, including topological skeletons \cite{Eden:PARCO05}, special-purpose skeletons for computer algebra \cite{Berthold2009-fft,lobachev-phd,Lobachev2012,janjic2013space}, iteration skeletons \cite{Dieterle2013}. The idea of \citet{scscp} is to use a parallel Haskell to orchestrate further software systems to run in parallel. \citet{dieterle_horstmeyer_loogen_berthold_2016} compare the composition of skeleton to stable process networks.

\subsection{Arrows}

Arrows were introduced by \citet{HughesArrows}, basically they are a generalised function arrow~|->|. \citet{Hughes2005} is a tutorial on arrows. Some theoretical details on arrows \cite{jacobs_heunen_hasuo_2009,LINDLEY201197,ATKEY201119} are viable. \citet{Paterson:2001:NNA:507669.507664} introduced a new notation for arrows. Arrows have applications in information flow research \cite{1648705,LI20101974,Russo:2008:LLI:1411286.1411289}, invertible programming \cite{Alimarine:2005:BAA:1088348.1088357}, and quantum computer simulation \cite{vizzotto_altenkirch_sabry_2006}. But perhaps most prominent application of arrows is functional reactive programming \cite{Hudak2003}.\olcomment{cite more!}

\citet{Liu:2009:CCA:1631687.1596559} formally define a more special kind of arrows that capsule the computation more than regular arrows do and thus enable optimizations. Their approach would allow parallel composition, as their special arrows would not interfere with each other in concurrent execution. In a contrast, we capture a whole parallel computation as a single entity: our main instantiation function |parEvalN| makes a single (parallel) arrow out of list of arrows.\olcomment{ugh, take care!}
\citet{Huang2007} utilise arrows for parallelism, but strikingly different from our approach. They basically use arrows to orchestrate several tasks in robotics. We propose a general interface for parallel programming, remaining completely in Haskell.

\subsection{Other languages}

Although this work is centred on Haskell implementation of arrows, it is applicable to any functional programming language where parallel evaluation and arrows can be defined. Our experiments with our approach in Frege language (which is basically Haskell on the JVM) were quite successful, we were able to use typical Java libraries for parallelism. However, it is beyond the scope of this work.

\citet{achten2004arrows,achten2007arrow} use an arrow implementation in Clean for better handling of typical GUI tasks. \citet{Dagand:2009:ORD:1481861.1481870} used arrows in OCaml in the implementation of a distributed system.


%%% Local Variables:
%%% mode: latex
%%% TeX-master: "main"
%%% End:
