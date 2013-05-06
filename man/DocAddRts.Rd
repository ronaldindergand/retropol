\name{DocAddRts}
\alias{DocAddRts}
\title{adds doc entry to the doclist of an rts object}
\usage{
  DocAddRts(x, source, datatype, web = NULL, start = NULL,
    end = NULL)
}
\arguments{
  \item{x}{rts object}

  \item{source}{char string}

  \item{datatype}{c("original", "estimated",
  "interpolated")}

  \item{web}{character string}

  \item{start}{end of the doc validiy}

  \item{end}{end of the doc validiy}
}
\value{
  rts object
}
\description{
  adds doc entry to the doclist of an rts object
}
\examples{
data(rts)
DocAddRts(x, source = "Test", datatype = "interpolated", start = 1910, end = 1930)
DocAddRts(x, source = "Test", datatype = "interpolated", start = 1910, end = 2100)
DocAddRts(x, source = "Test", datatype = "interpolated", start = 1700, end = 2100)

x <- DocAddRts(x, "eins", "interpolated", web = NULL, start = 1801, end = 1820)
length(x$doclist) # 3
z <- DocAddRts(x, "zwei", "interpolated", web = NULL, start = 1810, end = 1820)
length(z$doclist) # 4

# with empty doclist
x$doclist <- NULL
DocAddRts(x, source = "Test", datatype = "interpolated")
DocAddRts(x, source = "Test", datatype = "interpolated", start = 1910, end = 1930)
}

