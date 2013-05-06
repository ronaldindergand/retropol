\name{doc}
\alias{doc}
\title{Creates a doc object}
\usage{
  doc(source, web = NULL, datatype = "orginal", start, end,
    frequency)
}
\arguments{
  \item{source}{character string, source}

  \item{web}{character string}

  \item{datatype}{c("original", "estimated",
  "interpolated")}

  \item{start}{numeric(2), start of the validiy of the doc
  object}

  \item{end}{numeric(2), end of the validiy of the doc
  object}

  \item{frequency}{frequency of the time series, currently
  unsupported.}
}
\value{
  An rts object, a list containing the elements
  \item{source}{character string, source}
  \item{remarks}{character vector, zero, one, or several
  remarks} \item{datatype}{c("original", "estimated",
  "interpolated")} \item{start}{numeric(2), start of the
  validiy of the doc object} \item{end}{numeric(2), end of
  the validiy of the doc object}
}
\description{
  frequency support wuerde doc objekte von den x$data
  unabhaengig machen.
}

