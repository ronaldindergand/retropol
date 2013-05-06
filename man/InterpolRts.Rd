\name{InterpolRts}
\alias{InterpolRts}
\title{Interpolates missing values in a rts object}
\usage{
  InterpolRts(x)
}
\arguments{
  \item{x}{an rts object}
}
\value{
  An rts object
}
\description{
  The Stineman interpolation restricts the range of the
  interpolant to the nearby range of the points and
  suppresses the well known oscillations characteristic of
  splines and other methods based on polynomials.
}
\examples{
data(rts.with.multiple.na)
InterpolRts(x)
}

