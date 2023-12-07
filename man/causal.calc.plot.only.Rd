\name{causal.calc.plot.only}
\alias{causal.calc.plot.only}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to plot causal mediation results}
\description{
Function to plot causal mediation results; must be run after causal.calc.all and use the output from causal.calc.all where plot was set to TRUE so that the values are returned. The purpose of this function is to allow the user to make some edits to the plot, without having to re-run all the mediation analyses.
}
\usage{
causal.calc.plot.only(values, mediator.names, plot.labels = NULL, plot.order = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{values}{
values to be used for plotting which come form causal.calc.all}
  \item{mediator.names}{
names of mediators, should be provided as a vector of strings that corresponds to the variable names in datas e.g. c("income","health")}
  \item{plot.labels}{
a vector of strings for the labels for the mediators in the plot; if not supplied then the variable names are used
}
  \item{plot.order}{
a vector of numbers specifying the order of the plot
}
}

\value{
returns a plot
}

\author{
Layla Parast}
\examples{
data(datamed)

output = causal.calc.all(datas = datamed, confounders = c("con1","con2"), outcome = "outcome", 
mainpred = "exposure", mediator.names = c("med1","med2","med3"),plot=TRUE)

causal.calc.plot.only(output$values, mediator.names = c("med1","med2","med3"), 
plot.labels = c("Employed", "Pollution exposure","Prior arrest"), plot.order = c(2,3,4,1))
}