\name{WindowDocAdj}
\alias{WindowDocAdj}
\title{Adjust the time attributes od a doc object to a lower frequency}
\usage{
  WindowDocAdj(doc.i, x, to)
}
\arguments{
  \item{doc.i}{a doc object}

  \item{x}{an rts object}

  \item{to}{scalar, the frequency ot aggregate}
}
\value{
  an adjusted doc object.
}
\description{
  Adjust the time attributes od a doc object to a lower
  frequency
}
\examples{
data(rts.with.multiple.na)
InterpolRts(x)
}

