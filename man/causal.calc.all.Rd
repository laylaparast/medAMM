\name{causal.calc.all}
\alias{causal.calc.all}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Main function for causal mediation analysis}
\description{
Main function for causal mediation analysis. Calculates the proportion of the exposure effect explained by each given mediator, as well as by all the mediators together. Implements the weighting approach of VanderWeele TJ, Vansteelandt S. Mediation Analysis with Multiple Mediators. Epidemiol Methods. 2014;2(1):95-115. doi:10.1515/em-2012-0010.
}
\usage{
causal.calc.all(datas, confounders, outcome, mainpred, mediator.names, 
survey = FALSE, survey.diagnose = FALSE, conf = TRUE, plot = TRUE, plot.labels 
= NULL, plot.order = NULL)
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
  \item{mediator.names}{
names of mediators, should be provided as a vector of strings that corresponds to the variable names in datas e.g. c("income","health")}
  \item{survey}{
TRUE or FALSE, if the data is a svydesign object; default is FALSE}
  \item{survey.diagnose}{
TRUE OR FALSE, if the user would like a summary of the interim weights printed to diagnose possible extreme weights; default is FALSE
}
 \item{conf}{
TRUE OR FALSE, if confidence intervals are wanted, which are obtained via bootstapping and are computationally intensive; default is TRUE}
  \item{plot}{
TRUE OR FALSE, if plot is wanted, which requires confidence intervals obtained via bootstapping and is computationally intensive; default is TRUE}
  \item{plot.labels}{
a vector of strings for the labels for the mediators in the plot; if not supplied then the variable names are used
}
  \item{plot.order}{
a vector of numbers specifying the order of the plot
}
}

\value{
If plot = TRUE, a plot of returned. In addition, a list is returned:

\item{results}{A formatted table of results}
\item{values}{A table of values used for plotting; will be empty if plot=FALSE}

}
\references{
VanderWeele TJ, Vansteelandt S. Mediation Analysis with Multiple Mediators. Epidemiol Methods. 2014;2(1):95-115. doi:10.1515/em-2012-0010
}
\author{
Layla Parast
}

\examples{
data(datamed)

causal.calc.all(datas = datamed, confounders = c("con1","con2"), outcome = "outcome", 
mainpred = "exposure", mediator.names = c("med1","med2","med3"),conf=FALSE, plot=FALSE)

#computationally intensive
\dontrun{causal.calc.all(datas = datamed, confounders = c("con1","con2"), 
outcome = "outcome", mainpred = "exposure", mediator.names = c("med1","med2",
"med3"),plot=TRUE)}
}