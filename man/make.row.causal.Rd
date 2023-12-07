\name{make.row.causal}
\alias{make.row.causal}
\title{
Makes a nicely formatted row of results
}
\description{
Makes a nicely formatted row of results. This is a helper function used by causal.calc.all.
}
\usage{
make.row.causal(datas, confounders, outcome, mainpred, mediators, survey = FALSE, values = FALSE, conf = TRUE, survey.diagnose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datas}{
dataset; if survey=TRUE this must be a svydesign object}
  \item{confounders}{
names of confounders, should be provided as a vector of strings that corresponds to the variable names in datas e.g. c("age","sex")
}
  \item{outcome}{
names of outcome, should be provided as a string that corresponds to the variable name in datas }
  \item{mainpred}{
names of main predictor or exposure, should be provided as a string that corresponds to the variable name in datas}
  \item{mediators}{
names of mediators, should be provided as a vector of strings that corresponds to the variable names in datas e.g. c("income","health")}
  \item{survey}{
TRUE or FALSE, if the data is a svydesign object; default is FALSE}
  \item{values}{
TRUE OR FALSE, if values output (which is used for the plot) is wanted; default is FALSE}
  \item{conf}{
TRUE OR FALSE, if confidence intervals are wanted, which are obtained via bootstapping and are computationally intensive; default is TRUE}
  \item{survey.diagnose}{
TRUE OR FALSE, if the user would like a summary of the interim weights printed to diagnose possible extreme weights; default is FALSE
}
}

\value{
if values=FALSE, a string with mediation results; if values = TRUE, a list with rr (string with mediation results) and values (a vector of mediation results)
}

\author{
Layla Parast
}


\keyword{internal}
