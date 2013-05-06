#' Creates a doc object
#' 
#' frequency support wuerde doc objekte von den x$data unabhaengig machen.
#' 
#' @param source      character string, source
#' @param web         character string
#' @param datatype    c("original", "estimated", "interpolated")
#' @param start       numeric(2), start of the validiy of the doc object
#' @param end         numeric(2), end of the validiy of the doc object
#' @param frequency   frequency of the time series, currently unsupported.
#' 
#' @return An rts object, a list containing the elements
#' \item{source}{character string, source}
#' \item{remarks}{character vector, zero, one, or several remarks}
#' \item{datatype}{c("original", "estimated", "interpolated")}
#' \item{start}{numeric(2), start of the validiy of the doc object}
#' \item{end}{numeric(2), end of the validiy of the doc object}
#' @export
doc <- function(source, web = NULL, datatype = "orginal", start, end, frequency){
  z <- list()
  z$source <- source
  z$web <- web
  z$datatype <- datatype
  z$start  <- start
  z$end    <- end
  class(z) <- "doc"
  z
}



#' window method for doc class
#' 
#' @param x          doc object
#' @param start      start of doc
#' @param end        end of doc
#' @param ...        unused, for compatibility with the generic
#' @return           An adjusted doc object, NULL if outside of start and end
#' @export
#' @method window doc
#' @import stats
window.doc <- function(x, start = x$start, end = x$end, ...){  
  if (length(start) > 1) {stop("only use single time")}
  if (length(end) > 1) {stop("only use single time")}
  
  z <- x
  
  # return NULL if window has no overlap with series
  if(start > x$end){
    return(NULL)
  }
  if(x$start > end){
    return(NULL)
  }

  # only adjust if window borders are inside the series
  if(start > x$start){
    z$start <- start
  }
  if(end < x$end){
    z$end <- end
  }
  
  z
}
