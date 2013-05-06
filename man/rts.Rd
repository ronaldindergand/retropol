\name{Rts}
\alias{Rts}
\title{Creates a rts object}
\usage{
  Rts(data, id, between = 0, within = 0)
}
\arguments{
  \item{data}{a ts object}

  \item{id}{identification string}

  \item{between}{scalar, the priority between frequencies}

  \item{within}{scalar, the priority within frequencies}
}
\value{
  An rts object, a list containing the elements
  \item{data}{a ts object} \item{doclist}{an empty doclist
  object} \item{between}{scalar, the priority between
  frequencies} \item{within}{scalar, the priority within
  the frequency} \item{id}{identification string}
}
\description{
  Creates a rts object
}

