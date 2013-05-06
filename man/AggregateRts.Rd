\name{AggregateRts}
\alias{AggregateRts}
\title{performs a temporal agregation of an rts object}
\usage{
  AggregateRts(x, conversion = "sum", to = 1)
}
\arguments{
  \item{x}{an rts object}

  \item{conversion}{type of conversion ("sum", "average",
  "first", "last")}

  \item{to}{destination frequency (1, or 4)}
}
\value{
  An rts object
}
\description{
  Calls ta from the tempdisagg package
}

