\name{window.doclist}
\alias{window.doclist}
\title{window method for doclist class}
\usage{
  \method{window}{doclist} (x, start = NULL, end = NULL,
    ...)
}
\arguments{
  \item{x}{doclist object}

  \item{start}{start of doclist}

  \item{end}{end of doclist}

  \item{...}{unused, for compatibility with the generic}
}
\value{
  An adjusted doclist object, NULL if no overlap with any
  doc objects
}
\description{
  window method for doclist class
}
\examples{
data(rts)
x <- x$doclist
a <- window(x, start = 1910, end = 2100)
b <- window(x, start = 1910)
all.equal(a, b)
a <- window(x, start = 1700, end = 2100)
b <- window(x)
all.equal(a, x)
}

