\name{causal.calc}
\alias{causal.calc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Causal mediation analysis function
}
\description{
Causal mediation analysis function. This is a helper function used by causal.calc.all. Implements the weighting approach of: VanderWeele TJ, Vansteelandt S. Mediation Analysis with Multiple Mediators. Epidemiol Methods. 2014;2(1):95-115. doi:10.1515/em-2012-0010.

}
\usage{
causal.calc(datas, confounders, outcome, mainpred, mediators, survey = FALSE, survey.diagnose = FALSE)
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

  \item{survey.diagnose}{
TRUE OR FALSE, if the user would like a summary of the interim weights printed to diagnose possible extreme weights; default is FALSE
}
}

\value{
Returns a list
  \item{direct.effect}{Direct effect}
  \item{indirect.effect}{Indirect effect}
  \item{total}{Total effect}
  \item{prop.mediated}{Proportion mediated}

}
\references{
VanderWeele TJ, Vansteelandt S. Mediation Analysis with Multiple Mediators. Epidemiol Methods. 2014;2(1):95-115. doi:10.1515/em-2012-0010
}
\author{
Layla Parast
}

\keyword{internal}
