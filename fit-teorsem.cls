\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{fit-teorsem}[2017/02/26 CTU FIT Theoretical Seminar Class]

% Inherit options of article
\DeclareOption*{%
  \PassOptionsToClass{\CurrentOption}{article}
}
\ProcessOptions\relax
\LoadClass[10pt, a4paper]{article}

% Proper encoding
\RequirePackage[utf8]{inputenc}

% Needed to configure page layout
\RequirePackage{geometry}

% Needed to use \if-\then-\else statement
\RequirePackage{xifthen}

% Needed to manage colors
\RequirePackage{xcolor}

% Needed to deal hyperlink
\RequirePackage[hidelinks]{hyperref}
\hypersetup{%
    colorlinks,
    urlcolor=[HTML]{DC3522},
    pdftitle={},
    pdfauthor={},
    pdfsubject={},
pdfkeywords={}
}

%-------------------------------------------------------------------------------
%                Commands for seminar information
%-------------------------------------------------------------------------------

% Define lecturer who held the lecture
\newcommand*{\lecturername}[1]{\def\@lecturername{#1}}

% Define lecturer email
\newcommand*{\lectureremail}[1]{\def\@lectureremail{#1}}

% Define paper title
\newcommand*{\papertitle}[1]{\def\@papertitle{#1}}

% Define paper authors
\newcommand*{\paperauthors}[1]{\def\@paperauthors{#1}}

% Define paper link for download
\newcommand*{\paperlink}[1]{\def\@paperlink{#1}}

%-------------------------------------------------------------------------------
%                Commands for layout management
%-------------------------------------------------------------------------------

\newcommand*{\maketsheader}{
    \begin{center}
    \LARGE\rmfamily\bfseries\@lecturername%
    \\%
    \normalsize\ttfamily\mdseries\href{mailto:\@lectureremail}{\@lectureremail}%
    \\%
    \normalsize\rmfamily{presented:}\\%
    \LARGE\rmfamily\bfseries\@papertitle%
    \\%
    \large\rmfamily\mdseries\@paperauthors%
    \\%
    \normalsize\ttfamily\href{\@paperlink}{\@paperlink}%
    \\%
    \vspace{2mm}
    \hrule
    \end{center}
}

%-------------------------------------------------------------------------------
%                Configure layout
%-------------------------------------------------------------------------------
\geometry{left=2.0cm, top=1.5cm, right=2.0cm, bottom=1.5cm, footskip=0cm}

%-------------------------------------------------------------------------------
%                Configure fonts
%-------------------------------------------------------------------------------
%\RequirePackage[sc]{mathpazo}


