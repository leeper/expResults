\name{expResults}
\alias{expResults}
\title{expResults}
\description{expResults}
%\usage{}
\arguments{
  \item{dv}{vector containing outcome values}
  \item{tr}{vector containing treatment group indicators}
  \item{labels}{an optional call to a vector (by default, called ".groupNames"), which - if specified - labels resulting table rows}
  \item{object.name}{if specified, stores the results table as an R object (e.g., to be used later for plotting)}
  \item{digits}{rounds all values to specified number of digits (default is 2)}
  \item{NAs}{handles NAs, by default removing them in calculating means and SDs}
}
\details{Function to produce a simple, four-column experimental results table, with treatment group mean, SD, n, and SE.}
%\value{}
%\references{}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
%\examples{}