# rCode
Personal used R code to enhance packages and to provide basic functionality.

Heavily builds on great other packages such as:

- [ggstatsplot](https://github.com/IndrajeetPatil/ggstatsplot)
- [nparLD](https://cran.r-project.org/web/packages/nparLD/nparLD.pdf)
- work by Dr. Haiko Luepsen on [non-parametric variance analyses](http://www.uni-koeln.de/~luepsen/R/)



## Installation

| Type        | Command                                                 |
|-------------|---------------------------------------------------------|                  
| Development | `devtools::source_url("https://raw.githubusercontent.com/M-Colley/rCode/main/r_functionality.R")` |

## Summary of benefits 

-   Significantly reduced code amount

-   Simple copy-paste results to the text editor (i.e. LaTeX)

-   Easy-to-update texts.



## Primary functions

### `checkAssumptionsForAnova_X_Factors`
This function checks Normality and Homogeneity of variance assumption. See https://www.datanovia.com/en/lessons/anova-in-r/#check-assumptions-1 for more information.  
There are dedicated functions for one, two, three, and four factors.

### `ggwithinstatsWithPriorNormalityCheck` and `ggbetweenstatsWithPriorNormalityCheck`
These functions will automatically determine whether the parametric or the non-parametric version of the ggwithinstats/ggbetweenstats version of the [ggstatsplot](https://github.com/IndrajeetPatil/ggstatsplot) package has to be used. Defined with sensible defaults.

### `ggwithinstatsWithPriorNormalityCheckAsterisk` and `ggbetweenstatsWithPriorNormalityCheckAsterisk`
These functions will show the graph with APA-conform asterisks (e.g., *** for p<0.001)
These functions will automatically determine whether the parametric or the non-parametric version of the ggwithinstats/ggbetweenstats version of the [ggstatsplot](https://github.com/IndrajeetPatil/ggstatsplot) package has to be used. Defined with sensible defaults. 

### `generateEffectPlot`
This function defines a plot either showing the main or interaction effect in bold.  

### `reportNPAV`
This function will report the results of the np.anova in the APA-correct way for within-subject designs.   
Necessary commands in LaTeX are:  
\newcommand{\F}[3]{$F({#1},{#2})={#3}$}  
\newcommand{\p}{\textit{p=}}  
\newcommand{\pminor}{\textit{p$<$}}  

### `reportNPAVChi`
This function will report the results of the np.anova in the APA-correct way for between-subject designs.   
Necessary commands in LaTeX are:  
\newcommand{\F}[3]{$F({#1},{#2})={#3}$}  
\newcommand{\p}{\textit{p=}}  
\newcommand{\pminor}{\textit{p$<$}}  

### `reportNparLD`
This function reports the model produced by nparLD. The model provided must be the model generated by the command 'nparLD' \code{\link[nparLD]{nparLD}} (see \url{https://cran.r-project.org/web/packages/nparLD/nparLD.pdf}).  
Necessary commands in LaTeX are:  
\newcommand{\F}{\textit{F=}}  
\newcommand{\p}{\textit{p=}}  
\newcommand{\pminor}{\textit{p$<$}}  
Has been superseded by the np.anova commands of [non-parametric variance analyses](http://www.uni-koeln.de/~luepsen/R/).


### `reportMeanAndSD`
For a given data.frame, this function calculates for all levels of an independent variable, the mean and standard deviation rounded to the 2nd digit.  
Necessary commands in LaTeX are:  
\newcommand{\m}{\textit{M=}}  
\newcommand{\sd}{\textit{SD=}}  

### `reportDunnTest`
This function reports a `FSA::dunnTest` object as text.   
Required commands in Latex:  
\newcommand{\padjminor}{\textit{p$_{adj}<$}}  
\newcommand{\padj}{\textit{p$_{adj}$=}}  

### `reportDunnTestTable`
This function reports post-hoc Dunn tests as a LaTex table. It uses the `dunn.test::dunn.test` function with Bonferroni-correction.








In case of questions or remarks, please contact me via [e-mail](mailto:mark.colley@uni-ulm.de?subject=[GitHub]%20Source%20rCode)
