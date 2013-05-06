
EndTime <- function(x){
  if (is.null(x)){
    return(NULL)
  }
  time(x$data)[length(x$data)]
}
IndexEndTime <- function(data){
  z <- lapply(data, EndTime)
  unlist(z)
}





#' an index of the frequencies in a rts.list
#' 
#' @param rts.list       rts.list
#' @return an index of the frequencies in a rts.list
#' @export
IndexFrequency <- function(rts.list){
  z <- lapply(rts.list, Frequency)
  unlist(z)
}

Frequency <- function(x){
  stopifnot(inherits(x, "rts"))
  if (is.null(x)){
    return(NULL)
  }
  frequency(x$data)
}



BetweenPriority <- function(x){
  stopifnot(inherits(x, "rts"))
  if (is.null(x)){
    return(NULL)
  }
  x$between
}

#' an index of the between.prority in a rts.list
#' 
#' @param rts.list       rts.list
#' @return an index of the frequencies in a rts.list
#' @export
IndexBetweenPriority <- function(rts.list){
  z <- lapply(rts.list, BetweenPriority)
  unlist(z)
}


WithinPriority <- function(x){
  stopifnot(inherits(x, "rts"))
  if (is.null(x$within)){
    return(0)
  }
  x$within
}
#' an index of the within.priority in a rts.list
#' 
#' @param  rts.list       rts.list
#' @return an index of the frequencies in a rts.list
#' @export
IndexWithinPriority <- function(rts.list){
  z <- lapply(rts.list, WithinPriority)
  unlist(z)
}
