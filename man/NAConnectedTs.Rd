\name{NAConnectedTs}
\alias{NAConnectedTs}
\title{Returns a ts with an index of all connected NAs.}
\usage{
  NAConnectedTs(x)
}
\arguments{
  \item{x}{an ts object}
}
\value{
  An ts object
}
\description{
  Could be improved, get rid of the loop
}
\examples{
x <- ts(rnorm(100), start = 1900)
x[c(39:50, 80:85)] <- NA
NAConnectedTs(x)
}

