# Additional methods for generic functions for ts objects.


# --- rbind method for time series --------------------------

merge.ts <- function(x, ...){
  # vertically combines time series
  #
  # Args:
  #   ...:            ts objects
  #
  # Returns:
  #   a time series
  if (is.list(x)){
    arg <- x
  } else {
    arg <- list(x, ...)
  }
  if (length(arg) < 2){
    stop("needs at least two series for rbind.")
  }
  z <- arg[[1]]
  for (i in 2:length(arg)){
    z <- SubBindTwoSeries(z, arg[[i]])
  }
  z
}


SubBindTwoSeries <- function(x1, x2){
  # vertically combines two time series
  #
  # Args:
  #   x1, x2:            ts object
  #
  # Returns:
  #   a time series
  if (frequency(x1) != frequency(x2)) {
    stop("series are not of the same frequency.")
  }
  z <- x1
  # x2 goes further to the past
  if (time(x1)[1] > time(x2)[1]){
    z <- window(z, start = start(x2), extend = TRUE)
    t.end <- time(x1)[1] - 1/frequency(x1)
    window(z, end = t.end) <- window(x2, end = t.end)
  }
  # x2 goes further to the future
  if (time(x2)[length(x2)] > time(x1)[length(x1)]){
    z <- window(z, end = end(x2), extend = TRUE)
    t.start <- time(x1)[length(x1)] + 1/frequency(x1)
    window(z, start = t.start) <- window(x2, start = t.start)
  } 
  z
}
