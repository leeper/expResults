\name{groupImpute}
\alias{groupImpute}
\title{groupImpute}
\description{Impute by groups}
%\usage{}
\arguments{
  \item{dataframe}{A dataframe containing missing data to be imputed.}
  \item{...}{One or more grouping vectors by which \code{dataframe} should be split before imputation.}
  \item{method}{A character string specifying the method by which missing data should be imputed. One of \dQuote{mean} (mean replacement; the default), \dQuote{median} (median replacement), \dQuote{draw1} (sample with replacement), or \dQuote{draw2} (sample without replacement).}
}
\details{Function to impute missing data in a dataframe, based upon grouping factor(s).}
\value{A dataframe}
%\references{}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
\examples{
n <- 100
mydf <- data.frame(t=sample(1:5,n,TRUE), x=rnorm(n), y=rnorm(n), z=rbinom(n,1,.5))
mydf[cbind(sample(1:nrow(mydf),20,TRUE),sample(2:ncol(mydf),20,TRUE))] <- NA
groupImpute(mydf,mydf$t,mydf$z)
}