#' window method for doclist class
#' 
#' @param x          doclist object
#' @param start      start of doclist
#' @param end        end of doclist
#' @param ...             unused, for compatibility with the generic
#' @return           An adjusted doclist object, NULL if no overlap with any doc objects
#' @method window doclist
#' @import stats
#' @export
#' @examples 
#' data(rts)
#' x <- x$doclist
#' a <- window(x, start = 1910, end = 2100)
#' b <- window(x, start = 1910)
#' all.equal(a, b)
#' a <- window(x, start = 1700, end = 2100)
#' b <- window(x)
#' all.equal(a, x)
#' 
window.doclist <- function(x, start = NULL, end = NULL, ...){
  stopifnot(inherits(x, "doclist"))
  if (is.null(start)){
    start <- x[[1]]$start
  }
  if (is.null(end)){
    end <- x[[length(x)]]$end
  }
  
  if (length(start) > 1) {stop("only use single time")}
  if (length(end) > 1) {stop("only use single time")}
  z <- lapply(x, window.doc, start = start, end = end)
  z <- z[!unlist(lapply(z, is.null))]
  class(z) <- "doclist" 
  z
}


#' summary method for doclist class
#' 
#' @param object          doclist object
#' @param ...             unused, for compatibility with the generic
#' @export
#' @method summary doclist
summary.doclist <- function(object, ...){
  start <- unlist(lapply(object, ..., function(arg) arg$start))
  end <- unlist(lapply(object, ..., function(arg) arg$end))
  source <- unlist(lapply(object, ..., function(arg) arg$source))
  remarks <- unlist(lapply(object, ..., function(arg) paste(arg$remarks, collapse = "; ")))
  data.frame(start, end, source, remarks)
}
