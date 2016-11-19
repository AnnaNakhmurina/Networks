---
title: "Activist paper preliminary output"
author: "Anya Nakhmurina"
fontsize: 12pt
linestretch: 1.5
date: "08 November, 2016"
bibliography: networks.bib
output: pdf_document
header-includes: 
  - \usepackage{booktabs}
  - \usepackage{dcolumn}
  - \usepackage{caption}
  - \usepackage{fixltx2e}
  - \usepackage[flushleft]{threeparttable}
  - \usepackage{amsmath}
  - \usepackage{graphics}
  - \usepackage{float}
  - \usepackage{longtable}
  - \usepackage{array}
  - \usepackage{rotating}
  - \usepackage{float}
---


\section{The purpose of this document}

The research question of this paper is to learn whether there are any network effects among the 'active' and 'passive' activist investors over the span of the activists' campaign. This document contains a summary of my progress with this project. Research approach section is basically copied from my original proposal to remind the reader about my research approach. 



\footnotesize
\singlespacing

\begin{center}
  \begin{longtable}{p{9cm} p{2cm} p{2cm}p{2cm} }
		\caption{ \footnotesize \textbf{Summary of events by hedge fund stated goals - the whole 2015.} The sample consists of 467 activist campaigns in 2015, of which 352 contain demands.} \\
		 \hline 
				 \multicolumn{1}{l}{Activist' Objective} & \multicolumn{1}{l}{Num. of events} & \multicolumn{1}{l}{\% of Sample}  & \multicolumn{1}{l}{\% of Success} \\
		\hline \hline 
	\input{"C:/Users/anakhmur/Documents/Networks/Analysis/Code/markdown/sum_table_tex.tex"}
	\hline \hline
   \end{longtable}
\end{center}


\begin{center}
  \begin{longtable}{p{9cm} p{2cm} p{2cm}p{2cm} }
		\caption{\footnotesize  \textbf{Summary of events by hedge fund stated goals - the merged subsample of 2015.} The sample consists of 104 activist campaigns in 2015, of which 104 contain demands. The campaigns that fall into general undervaluation category are not considered here.} \\
		 \hline 
				 \multicolumn{1}{l}{Activist' Objective} & \multicolumn{1}{l}{Num. of events} & \multicolumn{1}{l}{\% of Sample}  & \multicolumn{1}{l}{\% of Success} \\
		\hline \hline 
	\input{"C:/Users/anakhmur/Documents/Networks/Analysis/Code/markdown/sum.table_tex_short.tex"}
	\hline \hline
   \end{longtable}
\end{center}

\begin{center}
  \begin{longtable}{p{5cm} p{2cm} p{2cm}p{2cm}p{2cm} }
		\caption{\footnotesize \textbf{Sussess rate by stage - the whole 2015.} This table provides the breakdown of stages at which the campaign is terminated. The table is based on the sample of all campaigns that took place in 2015. The data on campaign avalability comes from SharkWatch database. Campaigns were manually classified. } \\
		 \hline \\[0.2ex]
\multicolumn{1}{l}{Exit after} & \multicolumn{1}{l}{Num. of campaigns} & \multicolumn{1}{l}{\% of Sample} & \multicolumn{1}{l}{Number of Successes} & \multicolumn{1}{l}{\% of Successes} \\
		\hline \hline 
	\input{"C:/Users/anakhmur/Documents/Networks/Analysis/Code/markdown/success_stage_all_tex.tex"}
	\hline \hline
   \end{longtable}
\end{center}

\begin{center}
  \begin{longtable}{p{5cm} p{2cm} p{2cm}p{2cm}p{2cm} }
		\caption{\footnotesize \textbf{Sussess rate by stage - the merged subsample.}  This table provides the breakdown of stages at which the campaign is terminated. The table is based on the observations that are left after the campaigns data is merged with 13F data. The data on campaign avalability comes from SharkWatch database. Campaigns were manually classified. } \\
		 \hline \\[0.2ex]
\multicolumn{1}{l}{Exit after} & \multicolumn{1}{l}{Num. of campaigns} & \multicolumn{1}{l}{\% of Sample} & \multicolumn{1}{l}{Number of Successes} & \multicolumn{1}{l}{\% of Successes} \\
		\hline \hline 
	\input{"C:/Users/anakhmur/Documents/Networks/Analysis/Code/markdown/success_stage_merged_tex.tex"}
	\hline \hline
   \end{longtable}
\end{center}

\pagebreak
\begin{center}\scriptsize
  \begin{longtable}{p{2.8cm} p{3.7cm} p{1.1cm}p{1cm}p{1cm}p{1cm}p{1cm}p{1cm}p{1cm} }
		\caption{\footnotesize  \textbf{Descriptive statistics.} This table provides summary statistics on the variables used in preliminary analysis. The variables are grouped by type. \textit{won\_brep\_percent} is the percentage of board seats won out of the number of activists' nominees. \textit{won\_brep\_dummy} is an indicator variable equal to 1 when at least 1 activist nominee was elected to the board.\textit{success\_of\_stated\_obj} is an indicator of fulfillment of activists' demands. \textit{sales\_growth} is the growth of sales over the span of the campaign. \textit{oper\_profit\_growth} is an operational profitability growth over the span of the campaign. Operational profitability is defined as in  Ball et. al (2016). \textit{active.activist.size} correponds to the total assets of an activist group, computed from 13F filings. \textit{investor.number} is a total number of institutional investors that hold shares of a company. \textit{total.activist.number} is the number of passive activist investors that hold shares of the company. Activist investor is defined as any investor that appeared in SharkWatch database at least once. \textit{activist.size.vweghted} is the sum of all the company's activists' assets weighted by the share of investments in the company. \textit{activist.size.average} is an average of total assets of company's activists. \textit{spring measure} corresponds to the edges of Spring Network, which is described above. \textit{number of connections} corresponds to Number of Connections Network, where the weight of the edge is number of connections between two activists. \textit{size} is the market value of the company. \textit{age} is the age of the company. \textit{leverage} is the leverage of the company. \textit{mtb} is the market-to-book ratio of the company. \textit{oper\_profit} is an operating profitability of the company. \textit{roa} is return on company's assets. \textit{tobins\_q} is the company's Tobin's Q. \textit{asset\_turnover} is the company's asset turnover. \textit{rd\_to\_assets} is a share of R\&D expenditures to the company's assets. \textit{revtq} is the  quartely revenue, and \textit{saleq} are the company's sales. } \\
		\hline \hline 
		 \multicolumn{1}{l}{Variable type} & \multicolumn{1}{l}{Variable} & \multicolumn{1}{l}{mean}  & \multicolumn{1}{l}{sd} & \multicolumn{1}{l}{min} & \multicolumn{1}{l}{p25}& \multicolumn{1}{l}{median} & \multicolumn{1}{l}{p75} & \multicolumn{1}{l}{max}\\ \hline \\[0.2ex]
	\input{"C:/Users/anakhmur/Documents/Networks/Analysis/Code/markdown/short_summary_tex.tex"}
	\hline \hline
   \end{longtable}
\end{center}

\fontsize{10}{10}\selectfont
\begin{center}
\begin{sidewaystable}[p]\scriptsize
		\caption{\footnotesize \textbf{Correlation table.} \textit{won\_brep\_percent} is the percentage of board seats won out of the number of activists' nominees. \textit{won\_brep\_dummy} is an indicator variable equal to 1 when at least 1 activist nominee was elected to the board.\textit{success\_of\_stated\_obj} is an indicator of fulfillment of activists' demands. \textit{sales\_growth} is the growth of sales over the span of the campaign. \textit{oper\_profit\_growth} is an operational profitability growth over the span of the campaign. Operational profitability is defined as in  Ball et. al (2016). \textit{active.activist.size} correponds to the total assets of an activist group, computed from 13F filings. \textit{investor.number} is a total number of institutional investors that hold shares of a company. \textit{total.activist.number} is the number of passive activist investors that hold shares of the company. Activist investor is defined as any investor that appeared in SharkWatch database at least once. \textit{activist.size.vweghted} is the sum of all the company's activists' assets weighted by the share of investments in the company. \textit{activist.size.average} is an average of total assets of company's activists. \textit{spring measure} corresponds to the edges of Spring Network, which is described above. \textit{number of connections} corresponds to Number of Connections Network, where the weight of the edge is number of connections between two activists. \textit{size} is the market value of the company. \textit{age} is the age of the company. \textit{leverage} is the leverage of the company. \textit{mtb} is the market-to-book ratio of the company. \textit{oper\_profit} is an operating profitability of the company. \textit{roa} is return on company's assets. \textit{tobins\_q} is the company's Tobin's Q. \textit{asset\_turnover} is the company's asset turnover. \textit{rd\_to\_assets} is a share of R\&D expenditures to the company's assets. \textit{revtq} is the  quartely revenue, and \textit{saleq} are the company's sales}\centering \tiny
\begin{tabular}{p{3.4cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}}
\hline\hline
	\input{"C:/Users/anakhmur/Documents/Networks/Analysis/Code/markdown/upper_tex.tex"}
	\hline\hline
\end{tabular}
\end{sidewaystable}
\end{center}

\pagebreak
 \section{Preliminary results}
 
 This section contains the tables with output of some preliminary OLS regressions. 


\begin{sidewaystable}[!htbp] \centering 
  \caption{Logit regressions with robust standard errors } 
  \label{} 
\tiny 
\begin{tabular}{@{\extracolsep{5pt}}lcccccccc} 
\\[-1.8ex]\hline 
\hline \\[-1.8ex] 
 & \multicolumn{8}{c}{\textit{Dependent variable:}} \\ 
\cline{2-9} 
\\[-1.8ex] & \multicolumn{8}{c}{won\_board\_ind} \\ 
\\[-1.8ex] & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8)\\ 
\hline \\[-1.8ex] 
 total.activist.number & 0.0155 &  & 0.0274 &  & 0.0139 & 0.0258 &  &  \\ 
  & t = 2.9395$^{***}$ &  & t = 3.1411$^{***}$ &  & t = 2.4871$^{**}$ & t = 2.8278$^{***}$ &  &  \\ 
  & & & & & & & & \\ 
 top20\_number &  & 0.0366 &  & 0.0578 &  &  & 0.0334 & 0.0546 \\ 
  &  & t = 3.0801$^{***}$ &  & t = 3.2048$^{***}$ &  &  & t = 2.6589$^{***}$ & t = 2.9569$^{***}$ \\ 
  & & & & & & & & \\ 
 log(active.activist.size) &  &  & 0.1483 & 0.1308 &  & 0.0166 &  & 0.1333 \\ 
  &  &  & t = 1.8315$^{*}$ & t = 1.7353$^{*}$ &  & t = 0.1352 &  & t = 1.7259$^{*}$ \\ 
  & & & & & & & & \\ 
 exit\_s\_board:log(active.activist.size) &  &  &  &  &  & 0.5372 &  &  \\ 
  &  &  &  &  &  & t = 1.5993 &  &  \\ 
  & & & & & & & & \\ 
 exit\_s\_proxy:log(active.activist.size) &  &  &  &  &  & 0.1562 &  &  \\ 
  &  &  &  &  &  & t = 1.3170 &  &  \\ 
  & & & & & & & & \\ 
 exit\_s\_board &  &  &  &  & $-$1.5144 & $-$6.9805 & $-$1.5230 & $-$1.5536 \\ 
  &  &  &  &  & t = $-$1.5835 & t = $-$1.7390$^{*}$ & t = $-$1.5935 & t = $-$1.6602$^{*}$ \\ 
  & & & & & & & & \\ 
 exit\_s\_proxy &  &  &  &  & $-$0.8520 & $-$2.3501 & $-$0.8451 & $-$0.8533 \\ 
  &  &  &  &  & t = $-$2.1029$^{**}$ & t = $-$1.9368$^{*}$ & t = $-$2.0829$^{**}$ & t = $-$2.0632$^{**}$ \\ 
  & & & & & & & & \\ 
 Constant & 0.0366 & $-$0.1276 & $-$2.0602 & $-$1.9868 & 0.7824 & $-$0.0565 & 0.6172 & $-$1.2621 \\ 
  & t = 0.1093 & t = $-$0.3480 & t = $-$1.7325$^{*}$ & t = $-$1.7455$^{*}$ & t = 1.5523 & t = $-$0.0357 & t = 1.1707 & t = $-$1.0870 \\ 
  & & & & & & & & \\ 
\hline \\[-1.8ex] 
Observations & 198 & 198 & 198 & 198 & 198 & 198 & 198 & 198 \\ 
\hline 
\hline \\[-1.8ex] 
\multicolumn{9}{l} {\parbox[t]{15cm}{ \textit{Notes:} Logistic regression  of the equation $Y = a + b x + g N +controls+e$.
 \textit{won\_brep\_dummy} is an indicator variable equal to 1 when at least 1 activist nominee was elected to the board.\textit{success\_of\_stated\_obj} is an indicator of fulfillment of activists' demands.  \textit{active.activist.size} correponds to the total assets of an activist group, computed from 13F filings. \textit{investor.number} is a total number of institutional investors that hold shares of a company. \textit{total.activist.number} is the number of passive activist investors that hold shares of the company. Activist investor is defined as any investor that appeared in SharkWatch database at least once. Robust standard errors in parenthesis.  }} \\
\end{tabular} 
\end{sidewaystable} 

\begin{sidewaystable}[!htbp] \centering 
  \caption{Logit regressions with robust standard errors } 
  \label{} 
\tiny 
\begin{tabular}{@{\extracolsep{5pt}}lcccccccc} 
\\[-1.8ex]\hline 
\hline \\[-1.8ex] 
 & \multicolumn{8}{c}{\textit{Dependent variable:}} \\ 
\cline{2-9} 
\\[-1.8ex] & \multicolumn{8}{c}{success\_of\_stated\_obj} \\ 
\\[-1.8ex] & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8)\\ 
\hline \\[-1.8ex] 
 total.activist.number & 0.0522 &  & 0.0431 &  & 0.0528 & 0.0445 &  &  \\ 
  & t = 10.7074$^{***}$ &  & t = 7.1963$^{***}$ &  & t = 10.4862$^{***}$ & t = 7.1405$^{***}$ &  &  \\ 
  & & & & & & & & \\ 
 top20\_number &  & 0.1023 &  &  &  &  & 0.1063 & 0.0886 \\ 
  &  & t = 10.1807$^{***}$ &  &  &  &  & t = 9.8505$^{***}$ & t = 7.0466$^{***}$ \\ 
  & & & & & & & & \\ 
 investor.number &  &  &  & 0.0431 &  &  &  &  \\ 
  &  &  &  & t = 7.1989$^{***}$ &  &  &  &  \\ 
  & & & & & & & & \\ 
 log(active.activist.size) &  &  & $-$0.2031 & $-$0.2033 &  & $-$0.0593 &  & $-$0.2078 \\ 
  &  &  & t = $-$3.9029$^{***}$ & t = $-$3.9058$^{***}$ &  & t = $-$0.8154 &  & t = $-$3.9903$^{***}$ \\ 
  & & & & & & & & \\ 
 exit\_s\_board &  &  &  &  & $-$1.5636 & 0.6069 & $-$1.7702 & $-$1.3104 \\ 
  &  &  &  &  & t = $-$3.1119$^{***}$ & t = 0.3549 & t = $-$3.5081$^{***}$ & t = $-$2.5199$^{**}$ \\ 
  & & & & & & & & \\ 
 exit\_s\_proxy &  &  &  &  & 0.2449 & 1.7962 & 0.1837 & 0.2856 \\ 
  &  &  &  &  & t = 0.8815 & t = 1.7171$^{*}$ & t = 0.6774 & t = 1.0110 \\ 
  & & & & & & & & \\ 
 log(active.activist.size):exit\_s\_board &  &  &  &  &  & $-$0.2108 &  &  \\ 
  &  &  &  &  &  & t = $-$1.2833 &  &  \\ 
  & & & & & & & & \\ 
 log(active.activist.size):exit\_s\_proxy &  &  &  &  &  & $-$0.1777 &  &  \\ 
  &  &  &  &  &  & t = $-$1.6638$^{*}$ &  &  \\ 
  & & & & & & & & \\ 
 Constant & $-$3.4707 & $-$3.3511 & $-$1.0770 & $-$1.1196 & $-$3.4576 & $-$2.3722 & $-$3.3803 & $-$1.0547 \\ 
  & t = $-$10.2759$^{***}$ & t = $-$9.9296$^{***}$ & t = $-$1.4025 & t = $-$1.4489 & t = $-$8.8998$^{***}$ & t = $-$2.5371$^{**}$ & t = $-$8.5767$^{***}$ & t = $-$1.3997 \\ 
  & & & & & & & & \\ 
\hline \\[-1.8ex] 
Observations & 362 & 362 & 362 & 362 & 362 & 362 & 362 & 362 \\ 
\hline 
\hline \\[-1.8ex] 
\multicolumn{9}{l} {\parbox[t]{15cm}{ \textit{Notes:} Logistic regression  of the equation $Y = a + b x + g N +controls+e$.
 \textit{won\_brep\_dummy} is an indicator variable equal to 1 when at least 1 activist nominee was elected to the board.\textit{success\_of\_stated\_obj} is an indicator of fulfillment of activists' demands.  \textit{active.activist.size} correponds to the total assets of an activist group, computed from 13F filings. \textit{investor.number} is a total number of institutional investors that hold shares of a company. \textit{total.activist.number} is the number of passive activist investors that hold shares of the company. Activist investor is defined as any investor that appeared in SharkWatch database at least once. Robust standard errors in parenthesis.  }} \\
\end{tabular} 
\end{sidewaystable} 

\begin{sidewaystable}[!htbp] \centering 
  \caption{OLS regressions with robust standard errors.} 
  \label{} 
\scriptsize 
\begin{tabular}{@{\extracolsep{5pt}}lccccccc} 
\\[-1.8ex]\hline 
\hline \\[-1.8ex] 
 & \multicolumn{7}{c}{\textit{Dependent variable:}} \\ 
\cline{2-8} 
\\[-1.8ex] & \multicolumn{7}{c}{won\_board\_ind} \\ 
\\[-1.8ex] & (1) & (2) & (3) & (4) & (5) & (6) & (7)\\ 
\hline \\[-1.8ex] 
 total.activist.number & 0.0032 &  & 0.0028 &  & 0.0048 &  & 0.0039 \\ 
  & t = 2.9288$^{***}$ &  & t = 2.4610$^{**}$ &  & t = 3.1332$^{***}$ &  & t = 1.9288$^{*}$ \\ 
  & & & & & & & \\ 
 investor.number &  & 0.0032 &  & 0.0028 &  & 0.0048 &  \\ 
  &  & t = 2.9334$^{***}$ &  & t = 2.4652$^{**}$ &  & t = 3.1388$^{***}$ &  \\ 
  & & & & & & & \\ 
 exit\_s\_board &  &  & $-$0.2992 & $-$0.2991 & $-$0.2961 & $-$0.2960 & $-$0.2465 \\ 
  &  &  & t = $-$1.3834 & t = $-$1.3827 & t = $-$1.4389 & t = $-$1.4382 & t = $-$1.0279 \\ 
  & & & & & & & \\ 
 exit\_s\_proxy &  &  & $-$0.1444 & $-$0.1444 & $-$0.1432 & $-$0.1431 & $-$0.1527 \\ 
  &  &  & t = $-$2.2791$^{**}$ & t = $-$2.2786$^{**}$ & t = $-$2.2634$^{**}$ & t = $-$2.2630$^{**}$ & t = $-$2.0706$^{**}$ \\ 
  & & & & & & & \\ 
 age &  &  &  &  &  &  & 0.0005 \\ 
  &  &  &  &  &  &  & t = 0.1840 \\ 
  & & & & & & & \\ 
 log(size) &  &  &  &  &  &  & 0.0128 \\ 
  &  &  &  &  &  &  & t = 0.5209 \\ 
  & & & & & & & \\ 
 leverage &  &  &  &  &  &  & $-$0.0059 \\ 
  &  &  &  &  &  &  & t = $-$0.2097 \\ 
  & & & & & & & \\ 
 mtb &  &  &  &  &  &  & 0.0101 \\ 
  &  &  &  &  &  &  & t = 0.9579 \\ 
  & & & & & & & \\ 
 log(active.activist.size) &  &  &  &  & 0.0260 & 0.0261 & 0.0177 \\ 
  &  &  &  &  & t = 1.8674$^{*}$ & t = 1.8703$^{*}$ & t = 0.9074 \\ 
  & & & & & & & \\ 
 Constant & 0.5225 & 0.5189 & 0.6519 & 0.6487 & 0.2856 & 0.2803 & 0.2976 \\ 
  & t = 6.6356$^{***}$ & t = 6.5048$^{***}$ & t = 6.7697$^{***}$ & t = 6.6651$^{***}$ & t = 1.3545 & t = 1.3217 & t = 1.2294 \\ 
  & & & & & & & \\ 
\hline \\[-1.8ex] 
Observations & 198 & 198 & 198 & 198 & 198 & 198 & 162 \\ 
R$^{2}$ & 0.0459 & 0.0461 & 0.0749 & 0.0750 & 0.0937 & 0.0938 & 0.1129 \\ 
Adjusted R$^{2}$ & 0.0411 & 0.0412 & 0.0606 & 0.0607 & 0.0749 & 0.0751 & 0.0665 \\ 
\hline 
\hline \\[-1.8ex] 
\multicolumn{8}{l} {\parbox[t]{15cm}{ \textit{Notes:} OLS regression  of the equation $Y = a + b x + g N +controls+e$.
 \textit{won\_brep\_dummy} is an indicator variable equal to 1 when at least 1 activist nominee was elected to the board.\textit{success\_of\_stated\_obj} is an indicator of fulfillment of activists' demands.  \textit{active.activist.size} correponds to the total assets of an activist group, computed from 13F filings. \textit{investor.number} is a total number of institutional investors that hold shares of a company. \textit{total.activist.number} is the number of passive activist investors that hold shares of the company. Activist investor is defined as any investor that appeared in SharkWatch database at least once. \textit{size} is the market value of the company. \textit{age} is the age of the company. \textit{leverage} is the leverage of the company.  Robust standard errors in parenthesis.  }} \\
\end{tabular} 
\end{sidewaystable} 

\begin{sidewaystable}[!htbp] \centering 
  \caption{OLS regressions with robust standard errors.} 
  \label{} 
\scriptsize 
\begin{tabular}{@{\extracolsep{5pt}}lccccccc} 
\\[-1.8ex]\hline 
\hline \\[-1.8ex] 
 & \multicolumn{7}{c}{\textit{Dependent variable:}} \\ 
\cline{2-8} 
\\[-1.8ex] & \multicolumn{7}{c}{success\_of\_stated\_obj} \\ 
\\[-1.8ex] & (1) & (2) & (3) & (4) & (5) & (6) & (7)\\ 
\hline \\[-1.8ex] 
 total.activist.number & 0.0094 &  & 0.0094 &  & 0.0076 &  & 0.0082 \\ 
  & t = 18.2083$^{***}$ &  & t = 17.3319$^{***}$ &  & t = 8.8271$^{***}$ &  & t = 7.3148$^{***}$ \\ 
  & & & & & & & \\ 
 investor.number &  & 0.0094 &  & 0.0094 &  & 0.0076 &  \\ 
  &  & t = 18.2017$^{***}$ &  & t = 17.3238$^{***}$ &  & t = 8.8260$^{***}$ &  \\ 
  & & & & & & & \\ 
 exit\_s\_board &  &  & $-$0.2825 & $-$0.2827 & $-$0.2327 & $-$0.2328 & $-$0.2325 \\ 
  &  &  & t = $-$3.8714$^{***}$ & t = $-$3.8721$^{***}$ & t = $-$3.1644$^{***}$ & t = $-$3.1645$^{***}$ & t = $-$2.8695$^{***}$ \\ 
  & & & & & & & \\ 
 exit\_s\_proxy &  &  & 0.0440 & 0.0438 & 0.0483 & 0.0482 & 0.0525 \\ 
  &  &  & t = 0.9173 & t = 0.9133 & t = 1.0142 & t = 1.0113 & t = 0.9734 \\ 
  & & & & & & & \\ 
 log(active.activist.size) &  &  &  &  & $-$0.0257 & $-$0.0257 & $-$0.0214 \\ 
  &  &  &  &  & t = $-$2.8224$^{***}$ & t = $-$2.8263$^{***}$ & t = $-$1.7991$^{*}$ \\ 
  & & & & & & & \\ 
 age &  &  &  &  &  &  & $-$0.0025 \\ 
  &  &  &  &  &  &  & t = $-$1.2410 \\ 
  & & & & & & & \\ 
 log(size) &  &  &  &  &  &  & $-$0.0032 \\ 
  &  &  &  &  &  &  & t = $-$0.1923 \\ 
  & & & & & & & \\ 
 leverage &  &  &  &  &  &  & $-$0.0129 \\ 
  &  &  &  &  &  &  & t = $-$0.6996 \\ 
  & & & & & & & \\ 
 mtb &  &  &  &  &  &  & 0.0047 \\ 
  &  &  &  &  &  &  & t = 0.7689 \\ 
  & & & & & & & \\ 
 Constant & $-$0.1019 & $-$0.1114 & $-$0.0898 & $-$0.0991 & 0.2547 & 0.2474 & 0.2423 \\ 
  & t = $-$3.5815$^{***}$ & t = $-$3.8641$^{***}$ & t = $-$1.9813$^{**}$ & t = $-$2.1688$^{**}$ & t = 1.8120$^{*}$ & t = 1.7510$^{*}$ & t = 1.5312 \\ 
  & & & & & & & \\ 
\hline \\[-1.8ex] 
Observations & 362 & 362 & 362 & 362 & 362 & 362 & 298 \\ 
R$^{2}$ & 0.2756 & 0.2756 & 0.3121 & 0.3121 & 0.3269 & 0.3270 & 0.3571 \\ 
Adjusted R$^{2}$ & 0.2736 & 0.2736 & 0.3064 & 0.3064 & 0.3194 & 0.3194 & 0.3393 \\ 
\hline 
\hline \\[-1.8ex] 
\multicolumn{8}{l} {\parbox[t]{15cm}{ \textit{Notes:} OLS regression  of the equation $Y = a + b x + g N +controls+e$.
 \textit{won\_brep\_dummy} is an indicator variable equal to 1 when at least 1 activist nominee was elected to the board.\textit{success\_of\_stated\_obj} is an indicator of fulfillment of activists' demands.  \textit{active.activist.size} correponds to the total assets of an activist group, computed from 13F filings. \textit{investor.number} is a total number of institutional investors that hold shares of a company. \textit{total.activist.number} is the number of passive activist investors that hold shares of the company. Activist investor is defined as any investor that appeared in SharkWatch database at least once. \textit{size} is the market value of the company. \textit{age} is the age of the company. \textit{leverage} is the leverage of the company.  Robust standard errors in parenthesis.  }} \\
\end{tabular} 
\end{sidewaystable} 

\begin{sidewaystable}[!htbp] \centering 
  \caption{Basic spillower OLS regressions with robust standard errors} 
  \label{} 
\scriptsize 
\begin{tabular}{@{\extracolsep{5pt}}lcccccccc} 
\\[-1.8ex]\hline 
\hline \\[-1.8ex] 
 & \multicolumn{8}{c}{\textit{Dependent variable:}} \\ 
\cline{2-9} 
\\[-1.8ex] & \multicolumn{8}{c}{won\_board\_ind} \\ 
\\[-1.8ex] & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8)\\ 
\hline \\[-1.8ex] 
 log(activist.size.average) & $-$0.0194 &  & $-$0.0158 &  & $-$0.0475 &  & $-$0.0269 &  \\ 
  & t = $-$1.9144$^{*}$ &  & t = $-$1.5132 &  & t = $-$2.3048$^{**}$ &  & t = $-$1.0150 &  \\ 
  & & & & & & & & \\ 
 log(activist.size.vweighted) &  & $-$0.0199 &  & $-$0.0164 &  & $-$0.0449 &  & $-$0.0260 \\ 
  &  & t = $-$1.7992$^{*}$ &  & t = $-$1.4437 &  & t = $-$2.1075$^{**}$ &  & t = $-$1.0697 \\ 
  & & & & & & & & \\ 
 exit\_s\_board &  &  & $-$0.3183 & $-$0.3217 &  &  & $-$0.2677 & $-$0.2764 \\ 
  &  &  & t = $-$1.4676 & t = $-$1.4881 &  &  & t = $-$1.0959 & t = $-$1.1201 \\ 
  & & & & & & & & \\ 
 exit\_s\_proxy &  &  & $-$0.1557 & $-$0.1579 &  &  & $-$0.1704 & $-$0.1770 \\ 
  &  &  & t = $-$2.4374$^{**}$ & t = $-$2.4810$^{**}$ &  &  & t = $-$2.3082$^{**}$ & t = $-$2.3915$^{**}$ \\ 
  & & & & & & & & \\ 
 log(active.activist.size) &  &  &  &  & 0.0347 & 0.0291 & 0.0156 & 0.0120 \\ 
  &  &  &  &  & t = 1.6133 & t = 1.4286 & t = 0.5337 & t = 0.4894 \\ 
  & & & & & & & & \\ 
 age &  &  &  &  &  &  & 0.0009 & 0.0009 \\ 
  &  &  &  &  &  &  & t = 0.3084 & t = 0.3357 \\ 
  & & & & & & & & \\ 
 log(size) &  &  &  &  &  &  & 0.0198 & 0.0255 \\ 
  &  &  &  &  &  &  & t = 0.7816 & t = 1.1166 \\ 
  & & & & & & & & \\ 
 leverage &  &  &  &  &  &  & $-$0.0134 & $-$0.0135 \\ 
  &  &  &  &  &  &  & t = $-$0.4737 & t = $-$0.4766 \\ 
  & & & & & & & & \\ 
 mtb &  &  &  &  &  &  & 0.0120 & 0.0121 \\ 
  &  &  &  &  &  &  & t = 1.1146 & t = 1.0892 \\ 
  & & & & & & & & \\ 
 Constant & 0.9576 & 0.9742 & 1.0256 & 1.0434 & 0.9766 & 1.0193 & 0.8499 & 0.8490 \\ 
  & t = 7.6247$^{***}$ & t = 6.8236$^{***}$ & t = 8.2287$^{***}$ & t = 7.3328$^{***}$ & t = 7.6954$^{***}$ & t = 6.8573$^{***}$ & t = 3.4917$^{***}$ & t = 3.5844$^{***}$ \\ 
  & & & & & & & & \\ 
\hline \\[-1.8ex] 
Observations & 198 & 198 & 198 & 198 & 198 & 198 & 162 & 162 \\ 
R$^{2}$ & 0.0208 & 0.0190 & 0.0543 & 0.0535 & 0.0396 & 0.0333 & 0.0960 & 0.0962 \\ 
Adjusted R$^{2}$ & 0.0158 & 0.0140 & 0.0397 & 0.0389 & 0.0298 & 0.0233 & 0.0488 & 0.0489 \\ 
\hline 
\hline \\[-1.8ex] 
\multicolumn{9}{l} {\parbox[t]{15cm}{ \textit{Notes:} OLS regression  of the equation $Y = \alpha + \beta x + \gamma \bar{x} +controls+\epsilon$.
 \textit{won\_brep\_dummy} is an indicator variable equal to 1 when at least 1 activist nominee was elected to the board.\textit{success\_of\_stated\_obj} is an indicator of fulfillment of activists' demands.  \textit{active.activist.size} correponds to the total assets of an activist group, computed from 13F filings.   \textit{activist.size.vweghted} is the sum of all the company's activists' assets weighted by the share of investments in the company. \textit{activist.size.average} is an average of total assets of company's activists. Activist investor is defined as any investor that appeared in SharkWatch database at least once. \textit{size} is the market value of the company. \textit{age} is the age of the company. \textit{leverage} is the leverage of the company.  Robust standard errors in parenthesis.  }} \\
\end{tabular} 
\end{sidewaystable} 

\begin{sidewaystable}[!htbp] \centering 
  \caption{Basic spillower OLS regressions with robust standard errors} 
  \label{} 
\scriptsize 
\begin{tabular}{@{\extracolsep{5pt}}lcccccccc} 
\\[-1.8ex]\hline 
\hline \\[-1.8ex] 
 & \multicolumn{8}{c}{\textit{Dependent variable:}} \\ 
\cline{2-9} 
\\[-1.8ex] & \multicolumn{8}{c}{success\_of\_stated\_obj} \\ 
\\[-1.8ex] & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8)\\ 
\hline \\[-1.8ex] 
 log(activist.size.average) & $-$0.0859 &  & $-$0.0844 &  & $-$0.0807 &  & $-$0.0808 &  \\ 
  & t = $-$21.2154$^{***}$ &  & t = $-$19.0187$^{***}$ &  & t = $-$7.2268$^{***}$ &  & t = $-$5.9617$^{***}$ &  \\ 
  & & & & & & & & \\ 
 log(activist.size.vweighted) &  & $-$0.0824 &  & $-$0.0797 &  & $-$0.0624 &  & $-$0.0594 \\ 
  &  & t = $-$16.2885$^{***}$ &  & t = $-$14.1335$^{***}$ &  & t = $-$5.6525$^{***}$ &  & t = $-$4.8652$^{***}$ \\ 
  & & & & & & & & \\ 
 exit\_s\_board &  &  & $-$0.2221 & $-$0.2200 &  &  &  &  \\ 
  &  &  & t = $-$2.8764$^{***}$ & t = $-$2.7484$^{***}$ &  &  &  &  \\ 
  & & & & & & & & \\ 
 exit\_s\_proxy &  &  & 0.0470 & 0.0276 &  &  &  &  \\ 
  &  &  & t = 0.9888 & t = 0.5649 &  &  &  &  \\ 
  & & & & & & & & \\ 
 log(active.activist.size) &  &  &  &  & $-$0.0066 & $-$0.0247 & $-$0.0053 & $-$0.0293 \\ 
  &  &  &  &  & t = $-$0.4992 & t = $-$2.0198$^{**}$ & t = $-$0.3140 & t = $-$1.9993$^{**}$ \\ 
  & & & & & & & & \\ 
 age &  &  &  &  &  &  & $-$0.0018 & $-$0.0018 \\ 
  &  &  &  &  &  &  & t = $-$0.8422 & t = $-$0.8288 \\ 
  & & & & & & & & \\ 
 log(size) &  &  &  &  &  &  & $-$0.0006 & 0.0156 \\ 
  &  &  &  &  &  &  & t = $-$0.0365 & t = 0.9886 \\ 
  & & & & & & & & \\ 
 leverage &  &  &  &  &  &  & $-$0.0158 & $-$0.0157 \\ 
  &  &  &  &  &  &  & t = $-$0.7790 & t = $-$0.7875 \\ 
  & & & & & & & & \\ 
 mtb &  &  &  &  &  &  & 0.0045 & 0.0047 \\ 
  &  &  &  &  &  &  & t = 0.6714 & t = 0.7091 \\ 
  & & & & & & & & \\ 
 Constant & 1.5310 & 1.5348 & 1.5153 & 1.5116 & 1.5300 & 1.5152 & 1.5684 & 1.4519 \\ 
  & t = 21.7832$^{***}$ & t = 18.8555$^{***}$ & t = 21.3178$^{***}$ & t = 17.9380$^{***}$ & t = 21.7178$^{***}$ & t = 19.0400$^{***}$ & t = 12.6829$^{***}$ & t = 12.2636$^{***}$ \\ 
  & & & & & & & & \\ 
\hline \\[-1.8ex] 
Observations & 362 & 362 & 362 & 362 & 362 & 362 & 298 & 298 \\ 
R$^{2}$ & 0.2940 & 0.2487 & 0.3185 & 0.2696 & 0.2945 & 0.2576 & 0.3141 & 0.2763 \\ 
Adjusted R$^{2}$ & 0.2920 & 0.2466 & 0.3128 & 0.2635 & 0.2906 & 0.2535 & 0.2999 & 0.2614 \\ 
\hline 
\hline \\[-1.8ex] 
\multicolumn{9}{l} {\parbox[t]{15cm}{ \textit{Notes:} OLS regression  of the equation $Y = \alpha + \beta x + \gamma \bar{x} +controls+\epsilon$.
 \textit{won\_brep\_dummy} is an indicator variable equal to 1 when at least 1 activist nominee was elected to the board.\textit{success\_of\_stated\_obj} is an indicator of fulfillment of activists' demands.  \textit{active.activist.size} correponds to the total assets of an activist group, computed from 13F filings.   \textit{activist.size.vweghted} is the sum of all the company's activists' assets weighted by the share of investments in the company. \textit{activist.size.average} is an average of total assets of company's activists. Activist investor is defined as any investor that appeared in SharkWatch database at least once. \textit{size} is the market value of the company. \textit{age} is the age of the company. \textit{leverage} is the leverage of the company.  Robust standard errors in parenthesis.  }} \\
\end{tabular} 
\end{sidewaystable} 

\begin{sidewaystable}[!htbp] \centering 
  \caption{Basic spillower OLS regressions with robust standard errors} 
  \label{} 
\scriptsize 
\begin{tabular}{@{\extracolsep{5pt}}lcccccccc} 
\\[-1.8ex]\hline 
\hline \\[-1.8ex] 
 & \multicolumn{8}{c}{\textit{Dependent variable:}} \\ 
\cline{2-9} 
\\[-1.8ex] & \multicolumn{8}{c}{won\_board\_ind} \\ 
\\[-1.8ex] & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8)\\ 
\hline \\[-1.8ex] 
 scale(inv\_size\_nw\_s) & $-$0.0799 &  & $-$0.0558 &  & $-$0.6249 &  & $-$0.6800 &  \\ 
  & t = $-$0.4603 &  & t = $-$0.2665 &  & t = $-$0.9410 &  & t = $-$1.4988 &  \\ 
  & & & & & & & & \\ 
 scale(inv\_size\_nw\_spr) &  & 0.0114 &  & 0.0149 &  & $-$0.6768 &  & 0.0063 \\ 
  &  & t = 0.1841 &  & t = 0.2146 &  & t = $-$3.1521$^{***}$ &  & t = 0.0721 \\ 
  & & & & & & & & \\ 
 exit\_s\_board &  &  & $-$0.3090 & $-$0.3427 &  &  & $-$0.3022 & $-$0.2491 \\ 
  &  &  & t = $-$1.2730 & t = $-$1.6504$^{*}$ &  &  & t = $-$1.0694 & t = $-$0.8595 \\ 
  & & & & & & & & \\ 
 exit\_s\_proxy &  &  & $-$0.1741 & $-$0.1720 &  &  & $-$0.1897 & $-$0.1691 \\ 
  &  &  & t = $-$2.7763$^{***}$ & t = $-$2.7029$^{***}$ &  &  & t = $-$2.5998$^{***}$ & t = $-$2.2337$^{**}$ \\ 
  & & & & & & & & \\ 
 act\_size\_nw\_s &  &  &  &  & 0.0000 &  & 0.0000 & 0.0000 \\ 
  &  &  &  &  & t = 0.9176 &  & t = 1.4326 & t = 0.0400 \\ 
  & & & & & & & & \\ 
 act\_size\_nw\_spr &  &  &  &  &  & 0.000000 &  &  \\ 
  &  &  &  &  &  & t = 3.4001$^{***}$ &  &  \\ 
  & & & & & & & & \\ 
 age &  &  &  &  &  &  & 0.0025 & 0.0034 \\ 
  &  &  &  &  &  &  & t = 0.9138 & t = 1.1892 \\ 
  & & & & & & & & \\ 
 scale(size) &  &  &  &  &  &  & $-$0.0380 & $-$0.0486 \\ 
  &  &  &  &  &  &  & t = $-$0.2936 & t = $-$0.3439 \\ 
  & & & & & & & & \\ 
 leverage &  &  &  &  &  &  & $-$0.0265 & $-$0.0223 \\ 
  &  &  &  &  &  &  & t = $-$0.8944 & t = $-$0.7271 \\ 
  & & & & & & & & \\ 
 mtb &  &  &  &  &  &  & 0.0147 & 0.0139 \\ 
  &  &  &  &  &  &  & t = 1.4413 & t = 1.2505 \\ 
  & & & & & & & & \\ 
 Constant & 0.7100 & 0.7181 & 0.8363 & 0.8426 & 0.5956 & 0.5460 & 0.6458 & 0.7428 \\ 
  & t = 18.4923$^{***}$ & t = 21.8732$^{***}$ & t = 15.7760$^{***}$ & t = 17.0581$^{***}$ & t = 4.3074$^{***}$ & t = 9.0531$^{***}$ & t = 4.8560$^{***}$ & t = 6.5114$^{***}$ \\ 
  & & & & & & & & \\ 
\hline \\[-1.8ex] 
Observations & 198 & 198 & 198 & 198 & 198 & 198 & 162 & 162 \\ 
R$^{2}$ & 0.0040 & 0.0002 & 0.0425 & 0.0411 & 0.0238 & 0.0245 & 0.1067 & 0.0745 \\ 
Adjusted R$^{2}$ & $-$0.0011 & $-$0.0049 & 0.0277 & 0.0262 & 0.0137 & 0.0145 & 0.0600 & 0.0261 \\ 
\hline 
\hline \\[-1.8ex] 
\multicolumn{9}{l} {\parbox[t]{14cm}{ \textit{Notes:} OLS regression  of the equation $Y = \alpha + \beta x + \gamma \bar{x} +controls+\epsilon$.
 \textit{won\_brep\_dummy} is an indicator variable equal to 1 when at least 1 activist nominee was elected to the board.\textit{success\_of\_stated\_obj} is an indicator of fulfillment of activists' demands.  \textit{active.activist.size} correponds to the total assets of an activist group, computed from 13F filings.   \textit{activist.size.vweghted} is the sum of all the company's activists' assets weighted by the share of investments in the company. \textit{activist.size.average} is an average of total assets of company's activists. Activist investor is defined as any investor that appeared in SharkWatch database at least once. \textit{size} is the market value of the company. \textit{age} is the age of the company. \textit{leverage} is the leverage of the company.  Robust standard errors in parenthesis.  }} \\
\end{tabular} 
\end{sidewaystable} 

\begin{sidewaystable}[!htbp] \centering 
  \caption{Basic spillower OLS regressions with robust standard errors} 
  \label{} 
\scriptsize 
\begin{tabular}{@{\extracolsep{5pt}}lcccccccc} 
\\[-1.8ex]\hline 
\hline \\[-1.8ex] 
 & \multicolumn{8}{c}{\textit{Dependent variable:}} \\ 
\cline{2-9} 
\\[-1.8ex] & \multicolumn{8}{c}{success\_of\_stated\_obj} \\ 
\\[-1.8ex] & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8)\\ 
\hline \\[-1.8ex] 
 scale(inv\_size\_nw\_s) & $-$0.0874 &  & $-$0.0584 &  & 0.0988 &  & 0.1320 &  \\ 
  & t = $-$2.6595$^{***}$ &  & t = $-$2.0074$^{**}$ &  & t = 0.1769 &  & t = 0.2900 &  \\ 
  & & & & & & & & \\ 
 scale(inv\_size\_nw\_spr) &  & $-$0.1064 &  & $-$0.0788 &  & $-$0.3195 &  & $-$0.0936 \\ 
  &  & t = $-$2.8302$^{***}$ &  & t = $-$2.2056$^{**}$ &  & t = $-$0.6055 &  & t = $-$2.1846$^{**}$ \\ 
  & & & & & & & & \\ 
 exit\_s\_board &  &  & $-$0.3108 & $-$0.2853 &  &  & $-$0.3168 & $-$0.2987 \\ 
  &  &  & t = $-$3.8301$^{***}$ & t = $-$3.4723$^{***}$ &  &  & t = $-$3.1340$^{***}$ & t = $-$2.8982$^{***}$ \\ 
  & & & & & & & & \\ 
 exit\_s\_proxy &  &  & $-$0.0518 & $-$0.0481 &  &  & $-$0.0499 & $-$0.0482 \\ 
  &  &  & t = $-$0.9354 & t = $-$0.8718 &  &  & t = $-$0.8072 & t = $-$0.7852 \\ 
  & & & & & & & & \\ 
 act\_size\_nw\_s &  &  &  &  & $-$0.0000 &  & $-$0.0000 & 0.0000 \\ 
  &  &  &  &  & t = $-$0.3365 &  & t = $-$0.3900 & t = 0.6974 \\ 
  & & & & & & & & \\ 
 act\_size\_nw\_spr &  &  &  &  &  & 0.0000 &  &  \\ 
  &  &  &  &  &  & t = 0.4103 &  &  \\ 
  & & & & & & & & \\ 
 age &  &  &  &  &  &  & 0.0003 & 0.0007 \\ 
  &  &  &  &  &  &  & t = 0.1388 & t = 0.3122 \\ 
  & & & & & & & & \\ 
 scale(size) &  &  &  &  &  &  & $-$0.0495 & $-$0.0493 \\ 
  &  &  &  &  &  &  & t = $-$2.4534$^{**}$ & t = $-$2.5522$^{**}$ \\ 
  & & & & & & & & \\ 
 leverage &  &  &  &  &  &  & $-$0.0125 & $-$0.0140 \\ 
  &  &  &  &  &  &  & t = $-$0.6467 & t = $-$0.7214 \\ 
  & & & & & & & & \\ 
 mtb &  &  &  &  &  &  & 0.0039 & 0.0047 \\ 
  &  &  &  &  &  &  & t = 0.5789 & t = 0.6979 \\ 
  & & & & & & & & \\ 
 Constant & 0.4945 & 0.4945 & 0.5502 & 0.5458 & 0.5318 & 0.4436 & 0.5703 & 0.5175 \\ 
  & t = 18.9872$^{***}$ & t = 19.1093$^{***}$ & t = 14.0134$^{***}$ & t = 13.8868$^{***}$ & t = 4.5555$^{***}$ & t = 3.4149$^{***}$ & t = 4.8771$^{***}$ & t = 7.9951$^{***}$ \\ 
  & & & & & & & & \\ 
\hline \\[-1.8ex] 
Observations & 362 & 362 & 362 & 362 & 362 & 362 & 298 & 298 \\ 
R$^{2}$ & 0.0305 & 0.0452 & 0.0601 & 0.0699 & 0.0317 & 0.0473 & 0.0805 & 0.0895 \\ 
Adjusted R$^{2}$ & 0.0278 & 0.0425 & 0.0522 & 0.0621 & 0.0263 & 0.0420 & 0.0550 & 0.0643 \\ 
\hline 
\hline \\[-1.8ex] 
\multicolumn{9}{l} {\parbox[t]{14cm}{ \textit{Notes:} OLS regression  of the equation $Y = \alpha + \beta x + \gamma \bar{x} +controls+\epsilon$.
 \textit{won\_brep\_dummy} is an indicator variable equal to 1 when at least 1 activist nominee was elected to the board.\textit{success\_of\_stated\_obj} is an indicator of fulfillment of activists' demands.  \textit{active.activist.size} correponds to the total assets of an activist group, computed from 13F filings.   \textit{activist.size.vweghted} is the sum of all the company's activists' assets weighted by the share of investments in the company. \textit{activist.size.average} is an average of total assets of company's activists. Activist investor is defined as any investor that appeared in SharkWatch database at least once. \textit{size} is the market value of the company. \textit{age} is the age of the company. \textit{leverage} is the leverage of the company.  Robust standard errors in parenthesis.  }} \\
\end{tabular} 
\end{sidewaystable} 

\begin{sidewaystable}[!htbp] \centering 
  \caption{Basic spillower OLS regressions with robust standard errors} 
  \label{} 
\scriptsize 
\begin{tabular}{@{\extracolsep{5pt}}lcccccccc} 
\\[-1.8ex]\hline 
\hline \\[-1.8ex] 
 & \multicolumn{8}{c}{\textit{Dependent variable:}} \\ 
\cline{2-9} 
\\[-1.8ex] & \multicolumn{8}{c}{won\_board\_ind} \\ 
\\[-1.8ex] & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8)\\ 
\hline \\[-1.8ex] 
 scale(top20\_size\_nw\_s) & $-$0.0802 &  & $-$0.0562 &  & $-$0.0681 &  & $-$0.0633 &  \\ 
  & t = $-$0.4596 &  & t = $-$0.2670 &  & t = $-$0.3351 &  & t = $-$0.2876 &  \\ 
  & & & & & & & & \\ 
 scale(top20\_size\_nw\_spr) &  & 0.0696 &  & 0.1083 &  & 0.1312 &  & 0.2981 \\ 
  &  & t = 1.3118 &  & t = 1.3500 &  & t = 1.7465$^{*}$ &  & t = 1.8591$^{*}$ \\ 
  & & & & & & & & \\ 
 exit\_s\_board &  &  & $-$0.3087 & $-$0.3979 &  &  & $-$0.2556 & $-$0.2677 \\ 
  &  &  & t = $-$1.2715 & t = $-$1.9142$^{*}$ &  &  & t = $-$0.8997 & t = $-$0.9670 \\ 
  & & & & & & & & \\ 
 exit\_s\_proxy &  &  & $-$0.1742 & $-$0.1698 &  &  & $-$0.1965 & $-$0.1877 \\ 
  &  &  & t = $-$2.7768$^{***}$ & t = $-$2.6781$^{***}$ &  &  & t = $-$2.6833$^{***}$ & t = $-$2.5451$^{**}$ \\ 
  & & & & & & & & \\ 
 log(active.activist.size) &  &  &  &  & $-$0.0031 & $-$0.0137 &  &  \\ 
  &  &  &  &  & t = $-$0.2361 & t = $-$1.0822 &  &  \\ 
  & & & & & & & & \\ 
 act\_size\_nw\_s &  &  &  &  &  &  &  & $-$0.0000 \\ 
  &  &  &  &  &  &  &  & t = $-$0.9466 \\ 
  & & & & & & & & \\ 
 age &  &  &  &  &  &  & 0.0009 & 0.0009 \\ 
  &  &  &  &  &  &  & t = 0.3124 & t = 0.3236 \\ 
  & & & & & & & & \\ 
 log(size) &  &  &  &  &  &  & 0.0263 & 0.0182 \\ 
  &  &  &  &  &  &  & t = 1.2775 & t = 0.8375 \\ 
  & & & & & & & & \\ 
 leverage &  &  &  &  &  &  & $-$0.0172 & $-$0.0108 \\ 
  &  &  &  &  &  &  & t = $-$0.6015 & t = $-$0.3696 \\ 
  & & & & & & & & \\ 
 mtb &  &  &  &  &  &  & 0.0122 & 0.0100 \\ 
  &  &  &  &  &  &  & t = 1.2515 & t = 1.0946 \\ 
  & & & & & & & & \\ 
 Constant & 0.7100 & 0.7247 & 0.8363 & 0.8538 & 0.7407 & 0.8622 & 0.6318 & 0.7494 \\ 
  & t = 18.4686$^{***}$ & t = 22.6909$^{***}$ & t = 15.7697$^{***}$ & t = 17.0322$^{***}$ & t = 5.1974$^{***}$ & t = 6.6931$^{***}$ & t = 3.9837$^{***}$ & t = 4.1448$^{***}$ \\ 
  & & & & & & & & \\ 
\hline \\[-1.8ex] 
Observations & 198 & 198 & 198 & 198 & 198 & 198 & 162 & 162 \\ 
R$^{2}$ & 0.0041 & 0.0032 & 0.0426 & 0.0479 & 0.0045 & 0.0105 & 0.0841 & 0.0949 \\ 
Adjusted R$^{2}$ & $-$0.0010 & $-$0.0019 & 0.0278 & 0.0332 & $-$0.0057 & 0.0004 & 0.0425 & 0.0476 \\ 
\hline 
\hline \\[-1.8ex] 
\multicolumn{9}{l} {\parbox[t]{14cm}{ \textit{Notes:} OLS regression  of the equation $Y = \alpha + \beta x + \gamma \bar{x} +controls+\epsilon$.
 \textit{won\_brep\_dummy} is an indicator variable equal to 1 when at least 1 activist nominee was elected to the board.\textit{success\_of\_stated\_obj} is an indicator of fulfillment of activists' demands.  \textit{active.activist.size} correponds to the total assets of an activist group, computed from 13F filings.   \textit{activist.size.vweghted} is the sum of all the company's activists' assets weighted by the share of investments in the company. \textit{activist.size.average} is an average of total assets of company's activists. Activist investor is defined as any investor that appeared in SharkWatch database at least once. \textit{size} is the market value of the company. \textit{age} is the age of the company. \textit{leverage} is the leverage of the company.  Robust standard errors in parenthesis.  }} \\
\end{tabular} 
\end{sidewaystable} 

\begin{sidewaystable}[!htbp] \centering 
  \caption{Basic spillower OLS regressions with robust standard errors} 
  \label{} 
\scriptsize 
\begin{tabular}{@{\extracolsep{5pt}}lcccccccc} 
\\[-1.8ex]\hline 
\hline \\[-1.8ex] 
 & \multicolumn{8}{c}{\textit{Dependent variable:}} \\ 
\cline{2-9} 
\\[-1.8ex] & \multicolumn{8}{c}{success\_of\_stated\_obj} \\ 
\\[-1.8ex] & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8)\\ 
\hline \\[-1.8ex] 
 scale(top20\_size\_nw\_s) & $-$0.0872 &  & $-$0.0582 &  & $-$0.00004 &  & $-$0.0459 &  \\ 
  & t = $-$2.6751$^{***}$ &  & t = $-$2.0173$^{**}$ &  & t = $-$0.0034 &  & t = $-$1.2035 &  \\ 
  & & & & & & & & \\ 
 scale(top20\_size\_nw\_spr) &  & $-$0.1036 &  & $-$0.0702 &  & $-$0.0188 &  & $-$0.0607 \\ 
  &  & t = $-$3.0281$^{***}$ &  & t = $-$2.2097$^{**}$ &  & t = $-$2.2998$^{**}$ &  & t = $-$2.1157$^{**}$ \\ 
  & & & & & & & & \\ 
 exit\_s\_board &  &  & $-$0.3111 & $-$0.2736 &  &  & $-$0.3601 & $-$0.3221 \\ 
  &  &  & t = $-$3.8366$^{***}$ & t = $-$3.0619$^{***}$ &  &  & t = $-$3.4743$^{***}$ & t = $-$3.0062$^{***}$ \\ 
  & & & & & & & & \\ 
 exit\_s\_proxy &  &  & $-$0.0519 & $-$0.0526 &  &  & $-$0.0511 & $-$0.0523 \\ 
  &  &  & t = $-$0.9355 & t = $-$0.9514 &  &  & t = $-$0.8201 & t = $-$0.8408 \\ 
  & & & & & & & & \\ 
 log(active.activist.size) &  &  &  &  & $-$0.0727 & $-$0.0703 &  &  \\ 
  &  &  &  &  & t = $-$10.8577$^{***}$ & t = $-$10.4263$^{***}$ &  &  \\ 
  & & & & & & & & \\ 
 age &  &  &  &  &  &  & $-$0.0012 & $-$0.0016 \\ 
  &  &  &  &  &  &  & t = $-$0.5359 & t = $-$0.6889 \\ 
  & & & & & & & & \\ 
 log(size) &  &  &  &  &  &  & 0.0068 & 0.0130 \\ 
  &  &  &  &  &  &  & t = 0.3917 & t = 0.7453 \\ 
  & & & & & & & & \\ 
 leverage &  &  &  &  &  &  & $-$0.0112 & $-$0.0131 \\ 
  &  &  &  &  &  &  & t = $-$0.5920 & t = $-$0.6781 \\ 
  & & & & & & & & \\ 
 mtb &  &  &  &  &  &  & 0.0037 & 0.0042 \\ 
  &  &  &  &  &  &  & t = 0.5575 & t = 0.6261 \\ 
  & & & & & & & & \\ 
 Constant & 0.4945 & 0.4945 & 0.5502 & 0.5466 & 1.1784 & 1.1564 & 0.5249 & 0.4866 \\ 
  & t = 18.9869$^{***}$ & t = 19.1019$^{***}$ & t = 14.0155$^{***}$ & t = 13.9197$^{***}$ & t = 15.1823$^{***}$ & t = 14.9378$^{***}$ & t = 4.5436$^{***}$ & t = 4.1810$^{***}$ \\ 
  & & & & & & & & \\ 
\hline \\[-1.8ex] 
Observations & 362 & 362 & 362 & 362 & 362 & 362 & 298 & 298 \\ 
R$^{2}$ & 0.0303 & 0.0428 & 0.0600 & 0.0639 & 0.2035 & 0.2047 & 0.0706 & 0.0763 \\ 
Adjusted R$^{2}$ & 0.0276 & 0.0402 & 0.0521 & 0.0560 & 0.1991 & 0.2003 & 0.0481 & 0.0540 \\ 
\hline 
\hline \\[-1.8ex] 
\multicolumn{9}{l} {\parbox[t]{14cm}{ \textit{Notes:} OLS regression  of the equation $Y = \alpha + \beta x + \gamma \bar{x} +controls+\epsilon$.
 \textit{won\_brep\_dummy} is an indicator variable equal to 1 when at least 1 activist nominee was elected to the board.\textit{success\_of\_stated\_obj} is an indicator of fulfillment of activists' demands.  \textit{active.activist.size} correponds to the total assets of an activist group, computed from 13F filings.   \textit{activist.size.vweghted} is the sum of all the company's activists' assets weighted by the share of investments in the company. \textit{activist.size.average} is an average of total assets of company's activists. Activist investor is defined as any investor that appeared in SharkWatch database at least once. \textit{size} is the market value of the company. \textit{age} is the age of the company. \textit{leverage} is the leverage of the company.  Robust standard errors in parenthesis.  }} \\
\end{tabular} 
\end{sidewaystable} 


\fontsize{10}{10}\selectfont
\begin{center}
\begin{sidewaystable}[p]\scriptsize
		\caption{\scriptsize \textbf{Correlation table.}  \textit{won\_brep\_dummy} is an indicator variable equal to 1 when at least 1 activist nominee was elected to the board.\textit{success\_of\_stated\_obj} is an indicator of fulfillment of activists' demands. \textit{active.activist.size} correponds to the total assets of an activist group, computed from 13F filings. \textit{investor.number} is a total number of institutional investors that hold shares of a company. \textit{total.activist.number} is the number of passive activist investors that hold shares of the company. Activist investor is defined as any investor that appeared in SharkWatch database at least once. \textit{activist.size.vweghted} is the sum of all the company's activists' assets weighted by the share of investments in the company. \textit{activist.size.average} is an average of total assets of company's activists. \textit{size} is the market value of the company. \textit{age} is the age of the company. \textit{leverage} is the leverage of the company. \textit{mtb} is the market-to-book ratio of the company. \textit{oper\_profit} is an operating profitability of the company. All the other variables are centrality measures of activist network. Centrality captures the importance of the node position in a network. Three centrality measures are used. Closeness centrality shows how close each node to any other node. Betweennes centrality captures how well situated a node is in terms of the paths that it lies on. Degree centrality, is defined a the number of links incident to a node. Bonacich centrality is a degree centrality adjusted for the centrality of the neighbours in a network. The centrality measures were computed for both Simple and Spring networks. (By construction, centrality measures for Simple network are identical to the centrality measures of Number of Connections network.) I aggregated the centrality measures for each campaign. That is, \textit{act\_simple\_closeness} is a sum of closeness centralities of every active activist participating in a campaign, and \textit{oth\_simple\_closeness} is a sum of closeness centralities of every passive activist that invested in the company but does not participate in a campaign.}\centering \tiny
\begin{tabular}{p{3.1cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}p{0.55cm}}
\hline\hline
	\input{"C:/Users/anakhmur/Documents/Networks/Analysis/Code/markdown/upper2_tex.tex"}
	\hline\hline
\end{tabular}
\end{sidewaystable}
\end{center}


\begin{sidewaystable}[!htbp] \centering 
  \caption{ OLS regressions with centrality measures, robust se} 
  \label{} 
\tiny 
\begin{tabular}{@{\extracolsep{5pt}}lcccccccc} 
\\[-1.8ex]\hline 
\hline \\[-1.8ex] 
 & \multicolumn{8}{c}{\textit{Dependent variable:}} \\ 
\cline{2-9} 
\\[-1.8ex] & \multicolumn{4}{c}{won\_board\_ind} & \multicolumn{4}{c}{success\_of\_stated\_obj} \\ 
\\[-1.8ex] & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8)\\ 
\hline \\[-1.8ex] 
 act\_s\_clos & $-$0.0685$^{***}$ &  &  &  & $-$0.0401 &  &  &  \\ 
  & (0.0189) &  &  &  & (0.0470) &  &  &  \\ 
  & & & & & & & & \\ 
 act\_s\_betw &  & $-$0.0958$^{***}$ &  &  &  & $-$0.0553$^{*}$ &  &  \\ 
  &  & (0.0266) &  &  &  & (0.0295) &  &  \\ 
  & & & & & & & & \\ 
 act\_sp\_clos &  &  & $-$0.0685$^{***}$ &  &  &  & $-$0.0401 &  \\ 
  &  &  & (0.0189) &  &  &  & (0.0470) &  \\ 
  & & & & & & & & \\ 
 act\_sp\_betw &  &  &  & $-$0.0958$^{***}$ &  &  &  & $-$0.0553$^{*}$ \\ 
  &  &  &  & (0.0266) &  &  &  & (0.0295) \\ 
  & & & & & & & & \\ 
 total.activist.number & 0.0037$^{***}$ & 0.0035$^{***}$ & 0.0037$^{***}$ & 0.0035$^{***}$ & 0.0097$^{***}$ & 0.0097$^{***}$ & 0.0097$^{***}$ & 0.0097$^{***}$ \\ 
  & (0.0011) & (0.0011) & (0.0011) & (0.0011) & (0.0006) & (0.0005) & (0.0006) & (0.0005) \\ 
  & & & & & & & & \\ 
 Constant & 0.4921$^{***}$ & 0.5003$^{***}$ & 0.4921$^{***}$ & 0.5003$^{***}$ & $-$0.1210$^{***}$ & $-$0.1181$^{***}$ & $-$0.1210$^{***}$ & $-$0.1181$^{***}$ \\ 
  & (0.0793) & (0.0790) & (0.0793) & (0.0790) & (0.0339) & (0.0287) & (0.0339) & (0.0287) \\ 
  & & & & & & & & \\ 
\hline \\[-1.8ex] 
Observations & 198 & 198 & 198 & 198 & 362 & 362 & 362 & 362 \\ 
R$^{2}$ & 0.0702 & 0.0730 & 0.0702 & 0.0730 & 0.2817 & 0.2876 & 0.2817 & 0.2876 \\ 
Adjusted R$^{2}$ & 0.0607 & 0.0635 & 0.0607 & 0.0635 & 0.2777 & 0.2836 & 0.2777 & 0.2836 \\ 
\hline 
\hline \\[-1.8ex] 
\multicolumn{13}{l} {\parbox[t]{24cm}{ \footnotesize \textit{Notes:} OLS regression  of the equation $ Y = \alpha + \beta x + \gamma N +controls+\epsilon$. The regressions are run using the centrality measures to proxy for persuasiveness of an activist. Centrality is a characteristic of a node that captures the importance of the node position in a network. I use three centrality measures for this analysis. Closeness centrality shows how close each node to any other node. Betweennes centrality captures how well situated a node is in terms of the paths that it lies on. Degree centrality, is defined a the number of links incident to a node. Bonacich centrality is a degree centrality adjusted for the centrality of the neighbours in a network. The centrality measures were computed for both Simple and Spring networks. (By construction, centrality measures for Simple network are identical to the centrality measures of Number of Connections network.) After that I aggregated the centrality measures for each campaign. That is, $act\_simple\_closeness$ is a sum of closeness centralities of every active activist participating in a campaign, and $oth\_simple\_closeness$ is a sum of closeness centralities of every passive activist that invested in the company but does not participate in a campaign. $total.activist.number$ is the number of passive activist investors that hold shares of the company. 
Robust standard errors in parenthesis.}} \\
\end{tabular} 
\end{sidewaystable} 

\begin{sidewaystable}[!htbp] \centering 
  \caption{ OLS regressions with centrality measures, robust se} 
  \label{} 
\tiny 
\begin{tabular}{@{\extracolsep{5pt}}lcccccccccccc} 
\\[-1.8ex]\hline 
\hline \\[-1.8ex] 
 & \multicolumn{12}{c}{\textit{Dependent variable:}} \\ 
\cline{2-13} 
\\[-1.8ex] & \multicolumn{6}{c}{won\_board\_ind} & \multicolumn{6}{c}{success\_of\_stated\_obj} \\ 
\\[-1.8ex] & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) & (9) & (10) & (11) & (12)\\ 
\hline \\[-1.8ex] 
 act\_s\_clos & $-$0.0673 &  &  &  &  &  & $-$0.0186 &  &  &  &  &  \\ 
  & t = $-$2.1449$^{**}$ &  &  &  &  &  & t = $-$0.7365 &  &  &  &  &  \\ 
  & & & & & & & & & & & & \\ 
 oth\_s\_clos & 0.1013 &  &  &  &  &  & 0.1784 &  &  &  &  &  \\ 
  & t = 2.3382$^{**}$ &  &  &  &  &  & t = 7.0661$^{***}$ &  &  &  &  &  \\ 
  & & & & & & & & & & & & \\ 
 act\_s\_betw &  & $-$0.0871 &  &  &  &  &  & $-$0.0258 &  &  &  &  \\ 
  &  & t = $-$2.1739$^{**}$ &  &  &  &  &  & t = $-$1.0533 &  &  &  &  \\ 
  & & & & & & & & & & & & \\ 
 oth\_s\_betw &  & 0.1244 &  &  &  &  &  & 0.1891 &  &  &  &  \\ 
  &  & t = 3.1884$^{***}$ &  &  &  &  &  & t = 7.7323$^{***}$ &  &  &  &  \\ 
  & & & & & & & & & & & & \\ 
 act\_s\_bon &  &  & 0.0697 &  &  &  &  &  & 0.0208 &  &  &  \\ 
  &  &  & t = 2.1115$^{**}$ &  &  &  &  &  & t = 0.8175 &  &  &  \\ 
  & & & & & & & & & & & & \\ 
 oth\_s\_bon &  &  & $-$0.1146 &  &  &  &  &  & $-$0.1796 &  &  &  \\ 
  &  &  & t = $-$2.5352$^{**}$ &  &  &  &  &  & t = $-$7.0428$^{***}$ &  &  &  \\ 
  & & & & & & & & & & & & \\ 
 act\_sp\_clos &  &  &  & $-$0.0673 &  &  &  &  &  & $-$0.0186 &  &  \\ 
  &  &  &  & t = $-$2.1449$^{**}$ &  &  &  &  &  & t = $-$0.7365 &  &  \\ 
  & & & & & & & & & & & & \\ 
 oth\_sp\_clos &  &  &  & 0.1013 &  &  &  &  &  & 0.1784 &  &  \\ 
  &  &  &  & t = 2.3382$^{**}$ &  &  &  &  &  & t = 7.0661$^{***}$ &  &  \\ 
  & & & & & & & & & & & & \\ 
 act\_sp\_betw &  &  &  &  & $-$0.0871 &  &  &  &  &  & $-$0.0258 &  \\ 
  &  &  &  &  & t = $-$2.1739$^{**}$ &  &  &  &  &  & t = $-$1.0533 &  \\ 
  & & & & & & & & & & & & \\ 
 oth\_sp\_betw &  &  &  &  & 0.1244 &  &  &  &  &  & 0.1891 &  \\ 
  &  &  &  &  & t = 3.1884$^{***}$ &  &  &  &  &  & t = 7.7323$^{***}$ &  \\ 
  & & & & & & & & & & & & \\ 
 act\_sp\_bon &  &  &  &  &  & 0.0680 &  &  &  &  &  & 0.0141 \\ 
  &  &  &  &  &  & t = 2.1420$^{**}$ &  &  &  &  &  & t = 0.5448 \\ 
  & & & & & & & & & & & & \\ 
 oth\_sp\_bon &  &  &  &  &  & $-$0.1233 &  &  &  &  &  & $-$0.1742 \\ 
  &  &  &  &  &  & t = $-$2.6871$^{***}$ &  &  &  &  &  & t = $-$6.7344$^{***}$ \\ 
  & & & & & & & & & & & & \\ 
 Constant & 0.7241 & 0.7225 & 0.7260 & 0.7241 & 0.7225 & 0.7265 & 0.4945 & 0.4945 & 0.4945 & 0.4945 & 0.4945 & 0.4945 \\ 
  & t = 22.7364$^{***}$ & t = 23.0374$^{***}$ & t = 22.7274$^{***}$ & t = 22.7364$^{***}$ & t = 23.0374$^{***}$ & t = 22.8328$^{***}$ & t = 20.0108$^{***}$ & t = 20.2563$^{***}$ & t = 20.0124$^{***}$ & t = 20.0108$^{***}$ & t = 20.2563$^{***}$ & t = 19.9351$^{***}$ \\ 
  & & & & & & & & & & & & \\ 
\hline \\[-1.8ex] 
Observations & 198 & 198 & 198 & 198 & 198 & 198 & 362 & 362 & 362 & 362 & 362 & 362 \\ 
R$^{2}$ & 0.0393 & 0.0680 & 0.0419 & 0.0393 & 0.0680 & 0.0458 & 0.1231 & 0.1442 & 0.1232 & 0.1231 & 0.1442 & 0.1164 \\ 
Adjusted R$^{2}$ & 0.0295 & 0.0585 & 0.0321 & 0.0295 & 0.0585 & 0.0360 & 0.1182 & 0.1394 & 0.1183 & 0.1182 & 0.1394 & 0.1115 \\ 
\hline 
\hline \\[-1.8ex] 
\multicolumn{13}{l} {\parbox[t]{22cm}{\footnotesize \textit{Notes:} OLS regression  of the equation $Y = \alpha + \beta x + \gamma \bar{x} +controls+\epsilon$.The regressions are run using the centrality measures to proxy for persuasiveness of an activist. Centrality is a characteristic of a node that captures the importance of the node position in a network. I use three centrality measures for this analysis. Closeness centrality shows how close each node to any other node. Betweennes centrality captures how well situated a node is in terms of the paths that it lies on. Degree centrality, is defined a the number of links incident to a node. Bonacich centrality is a degree centrality adjusted for the centrality of the neighbours in a network. The centrality measures were computed for both Simple and Spring networks. (By construction, centrality measures for Simple network are identical to the centrality measures of Number of Connections network.) After that I aggregated the centrality measures for each campaign. That is, $act\_simple\_closeness$ is a sum of closeness centralities of every active activist participating in a campaign, and $oth\_simple\_closeness$ is a sum of closeness centralities of every passive activist that invested in the company but does not participate in a campaign. 
Robust standard errors in parenthesis. }} \\
\end{tabular} 
\end{sidewaystable} 

