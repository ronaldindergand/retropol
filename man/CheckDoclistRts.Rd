\name{CheckDoclistRts}
\alias{CheckDoclistRts}
\title{check consistency of a doclist in a rts object}
\usage{
  CheckDoclistRts(x)
}
\arguments{
  \item{x}{rts object}
}
\value{
  An error message if the check fails
}
\description{
  the function is applied to an rts object, not to a
  doclist, because it need frequency information from
  x$data.
}
\examples{
data(rts)
CheckDoclistRts(x)
}

