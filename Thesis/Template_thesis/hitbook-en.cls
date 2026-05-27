\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{hitbook-en}[2026/05/27 English thesis template class]

\LoadClass[14pt,a4paper,oneside,openany]{extbook}

\RequirePackage{iftex}
\ifXeTeX\else
  \ClassError{hitbook-en}{Please compile with XeLaTeX}{}
\fi

\RequirePackage{fontspec}
\RequirePackage{xeCJK}
\setmainfont{Times New Roman}
\setsansfont{Times New Roman}
\setmonofont{Courier New}
\setCJKmainfont{FandolSong-Regular}
\setCJKsansfont{FandolHei-Regular}

\RequirePackage[a4paper,left=30mm,right=15mm,top=20mm,bottom=20mm,headheight=14pt,headsep=8pt]{geometry}
\linespread{1.626}\selectfont

\RequirePackage{amsmath}
\RequirePackage{amssymb}
\RequirePackage{amsthm}
\RequirePackage{graphicx}
\RequirePackage{booktabs}
\RequirePackage{tabularx}
\RequirePackage{array}
\RequirePackage{float}
\RequirePackage{caption}
\RequirePackage{setspace}
\RequirePackage{subcaption}
\RequirePackage{enumitem}
\RequirePackage{titlesec}
\RequirePackage{titletoc}
\RequirePackage{tocloft}
\RequirePackage{indentfirst}
\RequirePackage{fancyhdr}
\RequirePackage{xparse}
\RequirePackage{etoolbox}
\RequirePackage{hyperref}
\RequirePackage[sort&compress]{gbt7714}
\RequirePackage{chngcntr}

\newcommand{\thesisuniversity}{Harbin Institute of Technology}
\newcommand{\thesistype}{Undergraduate Thesis}
\newcommand{\thesistitle}{Thesis Title}
\newcommand{\thesisauthor}{Author Name}
\newcommand{\thesisstudentid}{Student ID}
\newcommand{\thesisschool}{School Name}
\newcommand{\thesismajor}{Major Name}
\newcommand{\thesissupervisor}{Supervisor Name}
\newcommand{\thesisdegree}{Bachelor Degree}
\newcommand{\thesisdate}{Month Year}

\hypersetup{
  colorlinks=true,
  linkcolor=black,
  citecolor=black,
  urlcolor=black,
  pdfborder={0 0 0}
}

\setlength{\parindent}{2em}
\setlength{\parskip}{0pt}
\setlist{nosep}
\allowdisplaybreaks[4]

\AtBeginDocument{%
  \setlength{\abovedisplayskip}{6pt plus 2pt minus 2pt}%
  \setlength{\belowdisplayskip}{6pt plus 2pt minus 2pt}%
  \setlength{\abovedisplayshortskip}{2pt plus 1pt minus 1pt}%
  \setlength{\belowdisplayshortskip}{4pt plus 1pt minus 1pt}%
}

\newcommand{\wuhao}{\fontsize{12pt}{18pt}\selectfont}
\newcommand{\xiaosi}{\fontsize{14pt}{21pt}\selectfont}
\newcommand{\sihao}{\fontsize{14pt}{21pt}\selectfont}
\newcolumntype{Y}{>{\setlength{\parskip}{4pt}\setlength{\parindent}{0pt}\raggedright\arraybackslash}X}

\DeclareCaptionFont{captiontwelve}{\fontsize{12pt}{12pt}\selectfont}
\DeclareCaptionFont{singletablespacing}{\setstretch{1}}
\captionsetup[figure]{font=captiontwelve,labelfont={bf,captiontwelve},labelsep=space}
\captionsetup[table]{font={captiontwelve,singletablespacing},labelfont={bf,captiontwelve},labelsep=space}
\setlength{\abovecaptionskip}{0.25\baselineskip}
\setlength{\belowcaptionskip}{0pt}

\renewcommand{\figurename}{Figure}
\renewcommand{\tablename}{Table}
\renewcommand{\contentsname}{Contents}
\renewcommand{\bibname}{References}
\renewcommand{\appendixname}{Appendix}
\renewcommand{\chaptername}{Chapter}

\counterwithin{figure}{chapter}
\counterwithin{table}{chapter}
\counterwithin{equation}{chapter}
\renewcommand{\thefigure}{\thechapter.\arabic{figure}}
\renewcommand{\thetable}{\thechapter.\arabic{table}}
\renewcommand{\theequation}{\thechapter-\arabic{equation}}
\newcounter{algorithm}[chapter]
\renewcommand{\thealgorithm}{\thechapter.\arabic{algorithm}}

\setcounter{secnumdepth}{4}
\setcounter{tocdepth}{3}
\setlength{\cftbeforetoctitleskip}{0pt}
\setlength{\cftaftertoctitleskip}{0.5\baselineskip}
\renewcommand{\cfttoctitlefont}{\normalfont\bfseries\fontsize{20pt}{30pt}\selectfont}
\setlength{\cftparskip}{0pt}
\setlength{\cftbeforechapskip}{0pt}
\setlength{\cftbeforesecskip}{0pt}
\setlength{\cftbeforesubsecskip}{0pt}
\renewcommand{\cftchapfont}{\normalfont\sffamily\bfseries\fontsize{14pt}{17pt}\selectfont}
\renewcommand{\cftchappagefont}{\normalfont\sffamily\bfseries\fontsize{14pt}{17pt}\selectfont}
\renewcommand{\cftchapleader}{\cftdotfill{\cftdotsep}}
\renewcommand{\cftsecfont}{\normalfont\fontsize{14pt}{17pt}\selectfont}
\renewcommand{\cftsecpagefont}{\normalfont\fontsize{14pt}{17pt}\selectfont}
\renewcommand{\cftsubsecfont}{\normalfont\fontsize{14pt}{17pt}\selectfont}
\renewcommand{\cftsubsecpagefont}{\normalfont\fontsize{14pt}{17pt}\selectfont}
\renewcommand{\cftchapafterpnum}{\vadjust{\vskip -2.5pt}}
\renewcommand{\cftsecafterpnum}{\vadjust{\vskip -2.5pt}}
\renewcommand{\cftsubsecafterpnum}{\vadjust{\vskip -2.5pt}}
\renewcommand{\cftsubsubsecafterpnum}{\vadjust{\vskip -2.5pt}}

\titleformat{\chapter}[block]
  {\normalfont\bfseries\fontsize{20pt}{30pt}\selectfont}
  {Chapter \thechapter}
  {1em}
  {}
\titlespacing*{\chapter}{0pt}{-12pt}{0.5\baselineskip}

\titleformat{name=\chapter,numberless}[block]
  {\normalfont\bfseries\fontsize{20pt}{30pt}\selectfont}
  {}
  {0pt}
  {}
\titlespacing*{name=\chapter,numberless}{0pt}{-12pt}{0.5\baselineskip}

\titleformat{\section}
  {\normalfont\bfseries\fontsize{18pt}{27pt}\selectfont}
  {\thesection}
  {1em}
  {}
\titlespacing*{\section}{0pt}{0.25\baselineskip}{0.25\baselineskip}

\titleformat{\subsection}
  {\normalfont\bfseries\fontsize{16pt}{24pt}\selectfont}
  {\thesubsection}
  {1em}
  {}
\titlespacing*{\subsection}{0pt}{0.15\baselineskip}{0.15\baselineskip}

\titleformat{\subsubsection}
  {\normalfont\bfseries\fontsize{14pt}{21pt}\selectfont}
  {\thesubsubsection}
  {1em}
  {}
\titlespacing*{\subsubsection}{0pt}{0.1\baselineskip}{0.1\baselineskip}

\pagestyle{fancy}
\fancyhf{}
\fancyfoot[C]{\fontsize{9pt}{11pt}\selectfont - \thepage\ -}
\renewcommand{\headrulewidth}{0pt}
\renewcommand{\footrulewidth}{0pt}

\fancypagestyle{plain}{
  \fancyhf{}
  \fancyfoot[C]{\fontsize{9pt}{11pt}\selectfont - \thepage\ -}
  \renewcommand{\headrulewidth}{0pt}
  \renewcommand{\footrulewidth}{0pt}
}

\theoremstyle{plain}
\newtheorem{theorem}{Theorem}[chapter]
\newtheorem{lemma}{Lemma}[chapter]
\newtheorem{proposition}{Proposition}[chapter]
\newtheorem{corollary}{Corollary}[chapter]
\newtheorem{assumption}{Assumption}[chapter]
\theoremstyle{definition}
\newtheorem{definition}{Definition}[chapter]
\newtheorem{example}{Example}[chapter]
\theoremstyle{remark}
\newtheorem{remark}{Remark}[chapter]
\renewcommand{\thetheorem}{\thechapter.\arabic{theorem}}
\renewcommand{\thelemma}{\thechapter.\arabic{lemma}}
\renewcommand{\theproposition}{\thechapter.\arabic{proposition}}
\renewcommand{\thecorollary}{\thechapter.\arabic{corollary}}
\renewcommand{\theassumption}{\thechapter.\arabic{assumption}}
\renewcommand{\thedefinition}{\thechapter.\arabic{definition}}
\renewcommand{\theexample}{\thechapter.\arabic{example}}
\renewcommand{\theremark}{\thechapter.\arabic{remark}}

\makeatletter
\let\hitbooken@appendix\appendix
\renewcommand{\appendix}{%
  \@mainmattertrue
  \hitbooken@appendix
  \renewcommand{\chaptername}{Appendix}%
  \renewcommand{\thechapter}{Appendix \Alph{chapter}:}%
  \titleformat{\chapter}[block]
    {\normalfont\bfseries\fontsize{20pt}{30pt}\selectfont}
    {\thechapter}
    {0.5em}
    {}%
  \addtocontents{toc}{\protect\setlength{\protect\cftchapnumwidth}{6.6em}}%
  \renewcommand{\thesection}{\Alph{chapter}.\arabic{section}}%
  \renewcommand{\thesubsection}{\thesection.\arabic{subsection}}%
  \renewcommand{\thesubsubsection}{\thesubsection.\arabic{subsubsection}}%
  \renewcommand{\thefigure}{\Alph{chapter}.\arabic{figure}}%
  \renewcommand{\thetable}{\Alph{chapter}.\arabic{table}}%
  \renewcommand{\theequation}{\Alph{chapter}-\arabic{equation}}%
  \renewcommand{\thealgorithm}{\Alph{chapter}.\arabic{algorithm}}%
  \renewcommand{\thetheorem}{\Alph{chapter}.\arabic{theorem}}%
  \renewcommand{\thelemma}{\Alph{chapter}.\arabic{lemma}}%
  \renewcommand{\theproposition}{\Alph{chapter}.\arabic{proposition}}%
  \renewcommand{\thecorollary}{\Alph{chapter}.\arabic{corollary}}%
  \renewcommand{\theassumption}{\Alph{chapter}.\arabic{assumption}}%
  \renewcommand{\thedefinition}{\Alph{chapter}.\arabic{definition}}%
  \renewcommand{\theexample}{\Alph{chapter}.\arabic{example}}%
  \renewcommand{\theremark}{\Alph{chapter}.\arabic{remark}}%
}
\makeatother

\AtBeginEnvironment{table}{\setstretch{1}}

\newenvironment{conclusions}{\chapter*{Conclusions}\addcontentsline{toc}{chapter}{Conclusions}}{}

\makeatletter
\newcommand\bibstyle@numerical{\bibpunct{[}{]}{,}{s}{,}{\textsuperscript{,}}}
\citestyle{numerical}
\DeclareRobustCommand\inlinecite[1]{%
  \begingroup
  \citestyle{inline}%
  \let\@cite\NAT@citenum
  \citep{#1}%
  \endgroup
}
\patchcmd\NAT@citexnum{%
  \@ifnum{\NAT@ctype=\z@}{%
    \if*#2*\else\NAT@cmt#2\fi
  }{}%
  \NAT@mbox{\NAT@@close}%
}{%
  \NAT@mbox{\NAT@@close}%
}{}{}
\makeatother

\newcommand{\makeenglishcover}{%
  \begin{titlepage}
    \thispagestyle{empty}
    \centering
    \vspace*{18mm}
    {\fontsize{20pt}{30pt}\selectfont\bfseries \thesisuniversity\par}
    \vspace{18mm}
    {\fontsize{20pt}{30pt}\selectfont\bfseries \thesistype\par}
    \vspace{22mm}
    {\fontsize{20pt}{30pt}\selectfont\bfseries \thesistitle\par}
    \vfill
    \vspace*{-16mm}
    \begin{minipage}{0.56\textwidth}
      \raggedright
      \fontsize{14pt}{21pt}\selectfont
      Author: \thesisauthor\par
      Student ID: \thesisstudentid\par
      School: \thesisschool\par
      Major: \thesismajor\par
      Supervisor: \thesissupervisor\par
      Degree: \thesisdegree\par
      Date: \thesisdate\par
    \end{minipage}
    \vspace*{18mm}
  \end{titlepage}
}

\newenvironment{abstractpage}{%
  \chapter*{Abstract}
  \addcontentsline{toc}{chapter}{Abstract}
  \normalsize
  \linespread{1.626}\selectfont
}{}

\newcommand{\keywords}[1]{%
  \par\vspace{12pt}\noindent{\fontsize{14pt}{21pt}\selectfont\bfseries Keywords:\ }{\fontsize{14pt}{21pt}\selectfont #1}\par
}

\newcommand{\authorization}{%
  \chapter*{哈尔滨工业大学本科毕业论文（设计）原创性声明和使用权限}
  \addcontentsline{toc}{chapter}{哈尔滨工业大学本科毕业论文（设计）原创性声明和使用权限}
  本人郑重声明：在哈尔滨工业大学攻读学士学位期间，所提交的毕业设计（论文）《\thesistitle》，是本人在导师指导下独立进行研究工作所取得的成果。对本文的研究工作做出重要贡献的个人和集体，均已在文中以明确方式注明，其它未注明部分不包含他人已发表或撰写过的研究成果，不存在购买、由他人代写、剽窃和伪造数据等作假行为。

  \vspace{1.5\baselineskip}
  本人愿为此声明承担法律责任。

  \vspace{2\baselineskip}
  \noindent 学生签名：\hfill 日期：\hspace{30mm}
}
