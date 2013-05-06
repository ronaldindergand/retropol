\name{MergeRts}
\alias{MergeRts}
\title{Retropolates and/or extrapolates a time series with another}
\usage{
  MergeRts(x1, x2, type = "PC")
}
\arguments{
  \item{x1}{rts object with higher priority}

  \item{x2}{rts object with lower priority}

  \item{type}{type of retro: "PC", "SUBST"}
}
\value{
  An rts object
}
\description{
  Retropolates and/or extrapolates a time series with
  another
}

