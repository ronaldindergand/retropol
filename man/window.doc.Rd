\name{window.doc}
\alias{window.doc}
\title{window method for doc class}
\usage{
  \method{window}{doc} (x, start = x$start, end = x$end,
    ...)
}
\arguments{
  \item{x}{doc object}

  \item{start}{start of doc}

  \item{end}{end of doc}

  \item{...}{unused, for compatibility with the generic}
}
\value{
  An adjusted doc object, NULL if outside of start and end
}
\description{
  window method for doc class
}

