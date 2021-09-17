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


%format `rdr''` = "~\rrowal''''~"
%format `rdl''` = "~\rrowar''''~"
%format rdr'' = "(\rrowal'''')"
%format rdl'' = "(\rrowar'''')"

%format `rdr'` = "~\rrowal''~"
%format `rdl'` = "~\rrowar''~"
%format rdr' = "(\rrowal'')"
%format rdl' = "(\rrowar'')"

%format `rdr` = "~\rrowal~"
%format `rdl` = "~\rrowar~"
%format rdr = "(\rrowal)"
%format rdl = "(\rrowar)"

%-------------------------------------------------------------------------------
%                 Use custom packages
%-------------------------------------------------------------------------------
\usepackage{enumitem}
\usepackage{amsmath}
\usepackage{amsfonts}
\usepackage{subcaption}
\usepackage{tikz}
\usetikzlibrary{shapes.misc,arrows,cd,positioning,shapes,fit,trees}

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

\definecolor{ts-red}{HTML}{DC3522}

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
  \raise1.2pt\hbox{${\scriptscriptstyle%
  \mathbin{%
    \rangle
    \mathrel{\mkern-5mu}%
    \mathrel{-}%
  }}$}%
}
\newcommand{\rrowal}{%
  \raise1.2pt\hbox{${\scriptscriptstyle%
  \mathbin{%
    \mathrel{-}
    \mathrel{\mkern-5mu}%
    \langle%
  }}$}%
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

% spec environment to typeset, but hide from the compiler
% \ignore command to pass to the compiler but not typeset
% code environment to typeset and pass to the compiler

\ignore{
\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
import Prelude hiding (Monoid, mappend, mempty)
\end{code}}

\section*{Data structures}
\footnote[]{This file is literate Haskell, source available at
\url{https://github.com/viluon/stigma-finger-trees}.}

\vspace{-20pt}
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

        \node [
            rounded rectangle, inner sep = 2pt,
            fit = (deep0l) (deep0r), color = ts-red
        ] (spinelayer0) {};
        \node [
            rounded rectangle, inner sep = 2pt,
            fit = (deep1l) (deep1r), color = ts-red
        ] (spinelayer1) {};
        \node [
            rounded rectangle, inner sep = 2pt,
            fit = (empty), color = ts-red
        ] (spinelayer2) {};


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

\newcommand\fade[1]{{\color{gray}{#1}~}}
\begin{code}
data Node  {-"\fade{v}"-} a = Node2 {-"\fade{v}"-} a a | Node3 {-"\fade{v}"-} a a a
data Tree  {-"\fade{v}"-} a = Zero a | Succ (Tree (Node {-"\fade{v}"-} a))

data FingerTree {-"\fade{v}"-} a  =  Empty
                                  |  Single a
                                  |  Deep {-"\fade{v}"-} (Digit a) (FingerTree {-"\fade{v}"-} (Node {-"\fade{v}"-} a)) (Digit a)
type Digit a = [a]
data Split f a = Split (f a) a (f a)
\end{code}


\section*{Type classes}

%format mempty = "\emptyset"
%format `mappend` = "\oplus"
%format mappend = "(\oplus)"

%format measure a = "||" a "||"
%format measure (a) = "||" a "||"

\vspace{-15pt}
\begin{minipage}[c]{0.45\linewidth}
\begin{code}
class Monoid a where
  mempty   :: a
  mappend  :: a -> a -> a

class Reduce f where
  reducer  :: (a -> b -> b) -> (f a -> b   -> b)
  reducel  :: (b -> a -> b) -> (b   -> f a -> b)
\end{code}
\end{minipage}
\begin{minipage}[c]{0.45\linewidth}
\begin{code}
instance Monoid [a] where
  mempty   = []
  mappend  = (++)

instance Reduce [] where
  reducer  rdr x z = foldr  rdr z x
  reducel  rdl x z = foldl  rdl x z
\end{code}
\end{minipage}

\begin{code}
class Monoid v => Measured a v where
  measure :: a -> v
\end{code}

\section*{Algorithms}

\ignore{
\begin{code}
infixr 5 <|
(<|)                         :: a -> FingerTree a -> FingerTree a
a <| Empty                   = Single a
a <| Single b                = Deep [a] Empty [b]
a <| Deep [b, c, d, e] m sf  = Deep [a, b] (Node3 c d e <| m) sf
a <| Deep pr m sf            = Deep (a:pr) m sf

lhd' :: Reduce f => f a -> FingerTree a -> FingerTree a
lhd' = reducer (<|)

toTree :: Reduce f => f a -> FingerTree a
toTree s = s `lhd'` Empty
\end{code}
}

%format deepl = "deep_L"
%format deepr = "deep_R"

\vspace{-15pt}
\begin{code}
splitTree :: (Measured a v) => (v -> Bool) -> v -> FingerTree {-"\fade{v}"-} a -> Split (FingerTree {-"~\fade{v}\hspace{ -3pt}"-}) a
splitTree p i (Single x) = Split Empty x Empty
splitTree p i (Deep {-"\fade{\_}"-} pr m sf)
  | p vpr      =  let  Split l x r     = splitDigit p i pr
                  in   Split (toTree l) x (deepl r m sf)
  | p vm       =  let  Split ml xs mr  = splitTree p vpr m
                       Split l x r     = splitDigit p (vpr `mappend` measure ml) (toList xs)
                  in   Split (deepr pr ml l) x (deepl r mr sf)
  | otherwise  =  let  Split l x r     = splitDigit p vm sf
                  in   Split (deepr pr m l) x (toTree r)
  where  vpr  = i    `mappend` measure pr
         vm   = vpr  `mappend` measure m

{-"\\"-}

splitDigit :: (Measured a v) => (v -> Bool) -> v -> Digit a -> Split [] a
splitDigit p i [a]  = Split [] a []
splitDigit p i (a:as)
  | p i'            = Split [] a as
  | otherwise       = let Split l x r = splitDigit p i' as in Split (a:l) x r
  where i' = i `mappend` measure a
\end{code}

\begin{code}
instance Reduce Node where
  reducer  rdr (Node2 a b)    z  = a `rdr` (b `rdr` z)
  reducer  rdr (Node3 a b c)  z  = a `rdr` (b `rdr` (c `rdr` z))

  reducel  rdl z (Node2 a b)     = (z `rdl` a) `rdl` b
  reducel  rdl z (Node3 a b c)   = ((z `rdl` a) `rdl` b) `rdl` c

instance Reduce FingerTree where
  reducer  rdr Empty           z = z
  reducer  rdr (Single x)      z = x `rdr` z
  reducer  rdr (Deep pr m sf)  z = pr `rdr'` (m `rdr''` (sf `rdr'` z))
    where  rdr'   = reducer rdr
           rdr''  = reducer (reducer rdr)

  reducel  rdl z Empty           = z
  reducel  rdl z (Single x)      = z `rdl` x
  reducel  rdl z (Deep pr m sf)  = ((z `rdl'` pr) `rdl''` m) `rdl'` sf
    where  rdl'   = reducel rdl
           rdl''  = reducel (reducel rdl)
\end{code}

\section*{Applications}

\vspace{-15pt}

%format Nat = "\mathbb{N}"

\ignore{
\begin{code}
type Nat = Int
\end{code}
}

\begin{code}
newtype Size =  Size {getSize :: Nat}
                deriving (Eq, Ord)

instance Monoid Size where
  mempty                   = Size 0
  Size m `mappend` Size n  = Size (m + n)

newtype Elem  a = Elem {getElem :: a}
newtype Seq   a = Seq (FingerTree {-"\fade{Size}"-} (Elem a))

instance Measured (Elem a) Size where
  measure (Elem _) = Size 1
\end{code}

\begin{code}
data Prio a =  MInfty | Prio a
               deriving (Eq, Ord)
newtype PQueue a = PQueue (FingerTree {-"\fade{(Prio~a)}"-} (Elem a))
instance (Ord a) => Monoid (Prio a) where
  mempty                    = MInfty
  MInfty  `mappend` p       = p
  p       `mappend` MInfty  = p
  Prio m  `mappend` Prio n  = Prio (m `max` n)

instance (Ord a) => Measured (Elem a) (Prio a) where
  measure (Elem x) = Prio x
\end{code}

\end{document}
