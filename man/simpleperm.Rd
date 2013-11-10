\name{simpleperm}
\alias{simpleperm}
\title{simpleperm}
\description{Permutation test with imputation}
%\usage{}
\arguments{
  \item{y}{An outcome vector.}
  \item{x}{A vector indicating two groups to compare.}
  \item{iter}{An integer specifying the number of permutations to draw.}
  \item{useNA}{A character string specifying the method of imputation of: \dQuote{complete} (the default), \dQuote{sample.mean.impute}, \dQuote{group.mean.impute}, \dQuote{random.sample}, \dQuote{random.group}, \dQuote{random.sample.permute}, and \dQuote{random.group.permute}.}
  \item{replace}{A logical specifying whether to draw permutations with replacement (i.e., to bootstrap the difference rather than use the permutation distribution). Default is \code{FALSE} (for a permutation test).}
}
\details{Function to estimate a mean difference between two groups, with missing outcome data, using a permutation test (or bootstrap). The \code{useNA} argument specifies how to handle (and possibly impute) missing outcome values.}
\value{A vector representing the permutation (or bootstrap) distribution.}
%\references{}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
\examples{
x <- rbinom(100,1,.5)
y <- rnorm(100)
simpleperm(y,x)
}