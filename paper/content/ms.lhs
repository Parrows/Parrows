% This is our submission, modified from:
% the file JFP2egui.lhs
% release v1.02, 27th September 2001
%   (based on JFPguide.lhs v1.11 for LaLhs 2.09)
% Copyright (C) 2001 Cambridge University Press

\NeedsTeXFormat{LaTeX2e}

\documentclass{jfp1}

%include custom.fmt

%%% Macros for the guide only %%%
%\providecommand\AMSLaTeX{AMS\,\LaTeX}
%\newcommand\eg{\emph{e.g.}\ }
%\newcommand\etc{\emph{etc.}}
%\newcommand\bcmdtab{\noindent\bgroup\tabcolsep=0pt%
%  \begin{tabular}{@{}p{10pc}@{}p{20pc}@{}}}
%\newcommand\ecmdtab{\end{tabular}\egroup}
%\newcommand\rch[1]{$\longrightarrow\rlap{$#1$}$\hspace{1em}}
%\newcommand\lra{\ensuremath{\quad\longrightarrow\quad}}

\jdate{January 2018}
\pubyear{2018}
\pagerange{\pageref{firstpage}--\pageref{lastpage}}
\doi{...}

%\newtheorem{lemma}{Lemma}[section]

%include lhs2TeXSafeCommands.tex
%include prelude.tex


\title{Arrows for Parallel Computation}
\ifthenelse{\boolean{anonymous}}{%
\author{Submission ID xxxxxx}
}{%
%\author{Martin Braun, Phil Trinder, and Oleg Lobachev}
%\affiliation{University Bayreuth, Germany and Glasgow University, UK}
 \author[M. Braun, O. Lobachev, and P. Trinder]%
        {\textls*{MARTIN BRAUN}\\
         University Bayreuth, 95440 Bayreuth, Germany\\
		 \textls*{OLEG LOBACHEV}\\
		 University Bayreuth, 95440 Bayreuth, Germany\\
		 \and\ \textls*{PHIL TRINDER}\\
		 Glasgow University, Glasgow, G12 8QQ, Scotland}
}% end ifthenelse



\begin{document}

\label{firstpage}

\def\SymbReg{\textsuperscript{\textregistered}}

\maketitle

%% environment inside
%include abstract.tex

\tableofcontents

	%
	%%---include abstract.lhs
	%
	%\newpage
	%\pagebreak
	%include motivation.tex
	%include relwork.tex
        \section{Background}
	\label{sec:background}
	As an introduction, we here give a short overview over Arrows and GpH, the |Par| Monad, and Eden, the three parallel Haskells which we base our DSL on.
	%include arrows.tex
	%include parallelHaskells.tex
	%\pagebreak
	%\pagebreak
	%\pagebreak
	%include parrows.tex
	%\pagebreak
	%include basicSkeletons.tex
	%\pagebreak
	%include syntacticSugar.tex
	%\pagebreak
	%include futures.tex
	%\pagebreak
	%include mapSkeletons.tex
	%\pagebreak
	%include topologySkeletons.tex
	%\pagebreak
	%include benchmarks.tex
	%%\pagebreak
	%include conclusion.tex
	%\pagebreak
        %\bibliographystyle{jfp}
	%\bibliography{references,main}
        %include main.bbl
        \appendix
	%include utilityFunctions.tex
\end{document}
