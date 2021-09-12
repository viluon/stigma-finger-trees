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
\usepackage{subcaption}
\usepackage{tikz}
\usetikzlibrary{arrows,cd,positioning,shapes,fit,trees}

\tikzset{
	encircle/.style = {draw, circle, inner sep = 0.5mm, color = red},
	dot/.style = {circle, minimum size = 1mm, inner sep = 0mm, draw = black},
	reflexive dot/.style={loop,looseness=17,in=130,out=50},
	reflexive above/.style={->,loop,looseness=7,in=120,out=60},
	reflexive below/.style={->,loop,looseness=7,in=240,out=300},
	reflexive left/.style={->,loop,looseness=7,in=150,out=210},
	reflexive right/.style={->,loop,looseness=7,in=30,out=330},
	treenode/.style = {draw, circle, align=center, inner sep=0.5pt, minimum size=4pt, text centered},
	every node/.style={treenode},
}

\let\svthefootnote\thefootnote
\newcommand\blankfootnote[1]{%
  \let\thefootnote\relax\footnotetext{#1}%
  \let\thefootnote\svthefootnote%
}
\let\svfootnote\footnote
\renewcommand\footnote[2][?]{%
  \if\relax#1\relax%
    \blankfootnote{#2}%
  \else%
    \if?#1\svfootnote{#2}\else\svfootnote[#1]{#2}\fi%
  \fi
}
\newcommand{\rrowar}{%
  {\scriptscriptstyle%
  \mathbin{%
    >
    \mathrel{\mkern-5mu}%
    \mathrel{-}%
  }}%
}
\newcommand{\rrowal}{%
  {\scriptscriptstyle%
  \mathbin{%
    \mathrel{-}
    \mathrel{\mkern-5mu}%
    <%
  }}%
}
%  ⤙   leftwards  arrow tail 
%  𝈄   Greek vocal notation symbol-5 
%  𝈠   Greek instrumental notation symbol-5 
%  ⤚   Rightwards arrow-tail 
%  ᚛   Ogham feather mark
\DeclareUnicodeCharacter{1D204}{\ensuremath{\rrowal}}
\DeclareUnicodeCharacter{1D220}{\ensuremath{\rrowar}}

% for hiding code from the document
\long\def\ignore#1{}

\begin{document}
%-------------------------------------------------------------------------------
%                 Print seminar header
%-------------------------------------------------------------------------------
\maketsheader
%-------------------------------------------------------------------------------
%                 Create your content!
%-------------------------------------------------------------------------------
\thispagestyle{empty}

\ignore{
\begin{code}
import Prelude hiding (Monoid, mappend, mempty)
\end{code}}

\section*{Data structures}
\footnote[]{This file is literate Haskell, source available at
\url{https://github.com/viluon/stigma-finger-trees}}

\begin{figure}[h]
    \centering

    \begin{subfigure}{.5\textwidth}
        \centering
        \begin{tikzpicture}[
            -,
            >=stealth',
            level distance = 12pt,
            level/.style={sibling distance = 100pt/(#1*#1) + 20pt/#1 + 0pt},
            level 3/.style={every node/.style={treenode, minimum size=10pt}, sibling distance=12pt},
        ]
        \node {}
            child{ node {}
                child{ node {}
                    child{ node {t} }
                    child{ node {h} }
                }
                child{ node {}
                    child{ node {i} }
                    child{ node {s} }
                }
                child{ node {}
                    child{ node {i} }
                    child{ node {s} }
                }
            }
            child{ node {}
                child{ node {}
                    child{ node {n} }
                    child{ node {o} }
                    child{ node {t} }
                }
                child{ node {}
                    child{ node {a} }
                    child{ node {t} }
                }
                child{ node {}
                    child{ node {r} }
                    child{ node {e} }
                    child{ node {e} }
                }
            }
        ;
        \end{tikzpicture}
        \caption{A 2-3 tree with data stored in the leaves.}
        \label{fig:two-three}
    \end{subfigure}%
    \begin{subfigure}{.5\textwidth}
        \centering
        \begin{tikzpicture}[
            -,
            >=stealth',
            level distance = 12pt,
            digit/.style={treenode, minimum size=10pt},
        ]

        % NODES

        % spine
        \node (deep0l) at (5,9) {};
        \node (deep0r) at (6,9) {};

        \node (deep1l) at (5.25,8) {};
        \node (deep1r) at (5.75,8) {};

        \node (empty)  at (5.5,7) {};


        % first level
        \node (node0l1) at (3,8) [digit] {t};
        \node (node0l2) at (4,8) [digit] {h};

        \node (node0r1) at (7,8) [digit] {r};
        \node (node0r2) at (8,8) [digit] {e};
        \node (node0r3) at (9,8) [digit] {e};


        % second level
        \node (node1l1) at (3,7) {};
        \node (node1l2) at (4.35,7) {};

        \node (node1r1) at (7.5,7) {};
        \node (node1r2) at (9.3,7) {};


        % third level
        \node (nodenode1l11) at (2.5,6)  [digit] {i};
        \node (nodenode1l12) at (3.25,6) [digit] {s};
        \node (nodenode1l21) at (4,6)    [digit] {i};
        \node (nodenode1l22) at (4.75,6) [digit] {s};

        \node (nodenode1r11) at (6.75,6) [digit] {n};
        \node (nodenode1r12) at (7.5,6)  [digit] {o};
        \node (nodenode1r13) at (8.25,6) [digit] {t};
        \node (nodenode1r21) at (9,6)    [digit] {a};
        \node (nodenode1r22) at (9.75,6) [digit] {t};

        % EDGES

        % spine
        \draw (deep0l) -- (deep1l);
        \draw (deep0r) -- (deep1r);
        \draw (deep1l) -- (empty);
        \draw (deep1r) -- (empty);

        % first level
        \draw (deep0l) -- (node0l1);
        \draw (deep0l) -- (node0l2);
        \draw (deep0r) -- (node0r1);
        \draw (deep0r) -- (node0r2);
        \draw (deep0r) -- (node0r3);

        % second level
        \draw (deep1l) -- (node1l1);
        \draw (deep1l) -- (node1l2);
        \draw (deep1r) -- (node1r1);
        \draw (deep1r) -- (node1r2);

        % third level
        \draw (node1l1) -- (nodenode1l11);
        \draw (node1l1) -- (nodenode1l12);
        \draw (node1l2) -- (nodenode1l21);
        \draw (node1l2) -- (nodenode1l22);

        \draw (node1r1) -- (nodenode1r11);
        \draw (node1r1) -- (nodenode1r12);
        \draw (node1r1) -- (nodenode1r13);
        \draw (node1r2) -- (nodenode1r21);
        \draw (node1r2) -- (nodenode1r22);
        \end{tikzpicture}
        \caption{A finger tree.}
        \label{fig:finger-tree}
    \end{subfigure}
    \caption{Two trees of different types representing the string \texttt{thisisnotatree}.}
\end{figure}

\begin{code}
data Node  a = Node2 a a | Node3 a a a
data Tree  a = Zero a | Succ (Tree (Node a))

data FingerTree a  =  Empty
                   |  Single a
                   |  Deep (Digit a) (FingerTree (Node a)) (Digit a)
type Digit a = [a]
\end{code}

\section*{Type classes}

%format mempty = "\emptyset"
%format `mappend` = "\oplus"
%format mappend = "(\oplus)"

%format rdr = "(\rrowal)"
%format rdl = "(\rrowar)"

\begin{code}
class Monoid a where
  mempty   :: a
  mappend  :: a -> a -> a

instance Monoid [a] where
  mempty   = []
  mappend  = (++)

x = [] `mappend` []

foo :: [a]
foo = mempty

class Reduce f where
  reducer  :: (a -> b -> b) -> (f a -> b   -> b)
  reducel  :: (b -> a -> b) -> (b   -> f a -> b)

instance Reduce [] where
  reducer  rdr x z = foldr  rdr z x
  reducel  rdl x z = foldl  rdl x z
\end{code}

\section*{Algorithms}

\end{document}
