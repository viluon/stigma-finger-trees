\documentclass{fit-teorsem}

%-------------------------------------------------------------------------------
%                 Fill in seminar information
%-------------------------------------------------------------------------------
\lecturername{Ondřej Kvapil}
\lectureremail{kvapiond@@fit.cvut.cz}
\papertitle{Finger trees: a simple general-purpose data structure}
\paperauthors{Ralf Hinze, Ross Paterson}
\paperlink{http://dx.doi.org/10.1017/S0956796805005769}

% lhs2TeX
%include polycode.fmt
%include forall.fmt
%include greek.fmt
%include formatting.fmt

%-------------------------------------------------------------------------------
%                 Use custom packages
%-------------------------------------------------------------------------------
\usepackage{enumitem}
\usepackage{amsmath}
\usepackage{amsfonts}
\usepackage{tikz}
\usetikzlibrary{arrows,cd,positioning,shapes,fit}

\tikzset{
	encircle/.style = {draw, circle, inner sep = 0.5mm, color = red},
	dot/.style = {circle, minimum size = 1mm, inner sep = 0mm, draw = black},
	reflexive dot/.style={loop,looseness=17,in=130,out=50},
	reflexive above/.style={->,loop,looseness=7,in=120,out=60},
	reflexive below/.style={->,loop,looseness=7,in=240,out=300},
	reflexive left/.style={->,loop,looseness=7,in=150,out=210},
	reflexive right/.style={->,loop,looseness=7,in=30,out=330}
}

\begin{document}
%-------------------------------------------------------------------------------
%                 Print seminar header
%-------------------------------------------------------------------------------
\maketsheader
%-------------------------------------------------------------------------------
%                 Create your content!
%-------------------------------------------------------------------------------
\thispagestyle{empty}

\section*{Definitions}
\begin{code}
data Node  a = Node2 a a | Node3 a a a
data Tree  a = Zero a | Succ (Tree (Node a))
\end{code}

\section*{Theorems}

\end{document}
