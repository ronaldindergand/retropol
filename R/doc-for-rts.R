
#' adds doc entry to the doclist of an rts object
#' 
#' @param x          rts object
#' @param source     char string
#' @param datatype   c("original", "estimated", "interpolated")
#' @param web         character string
#' @param start      end of the doc validiy
#' @param end        end of the doc validiy
#' @return           rts object
#' @export
#' @examples
#' data(rts)
#' DocAddRts(x, source = "Test", datatype = "interpolated", start = 1910, end = 1930)
#' DocAddRts(x, source = "Test", datatype = "interpolated", start = 1910, end = 2100)
#' DocAddRts(x, source = "Test", datatype = "interpolated", start = 1700, end = 2100)
#' 
#' x <- DocAddRts(x, "eins", "interpolated", web = NULL, start = 1801, end = 1820)
#' length(x$doclist) # 3
#' z <- DocAddRts(x, "zwei", "interpolated", web = NULL, start = 1810, end = 1820)
#' length(z$doclist) # 4
#' 
#' # with empty doclist
#' x$doclist <- NULL
#' DocAddRts(x, source = "Test", datatype = "interpolated")
#' DocAddRts(x, source = "Test", datatype = "interpolated", start = 1910, end = 1930)
#' 
DocAddRts <- function(x, source, datatype, web = NULL,
                      start = NULL, end = NULL){
  stopifnot(inherits(x, "rts"))
  z <- x
  if (is.null(start)) {
    start <- time(x$data)[1]  # time should be a one element vector
  } 
  if (is.null(end)) {
    end <- time(x$data)[length(x$data)]
  } 
  
  newdoc <- doc(source = source, web = web, datatype = datatype, 
                start = start, end = end, frequency = frequency(x$data))
  # if no doclist exists
  if (is.null(z$doclist)){
    z$doclist[[1]] <- newdoc
    class(z$doclist) <- "doclist"
    return(z)
  }
  
  # if a doclist exists
  before <- window.doclist(x$doclist, end = start - 1/frequency(x$data))
  after <- window.doclist(x$doclist, start = end + 1/frequency(x$data))
  
  z$doclist <- before
  z$doclist[[length(z$doclist) + 1]] <- newdoc
  z$doclist <- c(z$doclist, after)
  
  class(z$doclist) <- "doclist"
  CheckDoclistRts(z)
  z
}


#' check consistency of a doclist in a rts object
#' 
#' the function is applied to an rts object, not to a doclist, because it need
#' frequency information from x$data.
#' 
#' @param x          rts object
#' @return           An error message if the check fails
#' @export
#' @examples 
#' data(rts)
#' CheckDoclistRts(x)
CheckDoclistRts <- function(x){
  stopifnot(inherits(x, "rts"))
  stopifnot(inherits(x$doclist, "doclist"))
  if (length(x$doclist) > 1){
    for (i in 1:(length(x$doclist)-1)){
      if (x$doclist[[i]]$end + 1/frequency(x$data) != x$doclist[[i+1]]$start){
        warning("time inconsistency in doclist")
      }
    }
  }
}




#' adds a remark to all doc objects of an rts object
#' 
#' @param x          rts object
#' @param text       char string
#' @param start      numeric
#' @param end        numeric
#' @return           rts object
#' @export
DocRemarkRts <- function(x, text, start = NULL, end = NULL){
  if (is.null(start)) {
    start <- start(x$data)
  } 
  if (is.null(end)) {
    end <- end(x$data)
  }
  
  for (i in 1:length(x$doclist)){
    x$doclist[[i]]$remarks <- c(x$doclist[[i]]$remarks, text)
  }
  x
}

#' time series with the datatypes in a rts
#' 
#' @param x          rts object
#' @return           time series with the datatype
#' @export
DocDatatype <- function(x){
  # sort the doclist by endate
  dl <- x$doclist[order(unlist(lapply(x$doclist, function(x) {x$end})))]
  
  # extracts the datatype for each doc element
  Each <- function(x, frequency){
    if (is.null(x)) {
      return(NULL)
    } else {
      return(ts(x$datatype, start = x$start, 
                end = x$end, frequency = frequency))
    }
  }
  
  # collects it in a list
  ll <- lapply(dl, Each, frequency = frequency(x$data))
  
  if (length(ll) > 1){
    return(merge.ts(ll))
  } else {
    return(ll[[1]])
  }
}
