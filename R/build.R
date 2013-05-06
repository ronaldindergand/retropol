#' Disaggregates an rts object with a higher frequency rts object
#' 
#' @param agg         a rts object of a lower frequency
#' @param ind         a rts object of a higher frequency indicator
#' @return An rts with the same freq as ind and the same id as agg
#' @export
#' 
Disaggregate <- function(agg, ind){
  ind.series <- ind$data
  agg.series <- window(agg$data, start = start(ta(ind.series)), end = end(ta(ind.series)))
  td.mod <- td(agg.series ~ ind.series)
  z <- Rts(
    predict(td.mod), 
    agg$id
  )
  z <- DocAddRts(z, source = ind$doclist[[1]]$source, 
              web = ind$doclist[[1]]$web[1], datatype = "estimated")
  z <- DocRemarkRts(z, text = paste("The source is used to interpolate the annual values by the Chow-Lin method. Adj. R2 of the regression:", 
                                 format(td.mod$r.squared, digits=2)))
  z
}


#' Sort and merges mts objects of a given frequency according their priority
#' 
#' @param x         a list with rts objects
#' @return An rts with the same freq as ind and the same id as agg
#' @export
#' 
SortMergeRts <- function(x){
  # check whether freq of all rts is identical
  if (length(unique(IndexFrequency(x))) != 1) {
    stop("all rts need to be of the same frequency.")
  }
  
  ordered.by.time <- x[order(IndexEndTime(x), decreasing = TRUE)]
  ordered.by.priority <- ordered.by.time[order(IndexWithinPriority(ordered.by.time), decreasing = TRUE)]
  
  z <- ordered.by.priority[[1]]
  if ((length(ordered.by.priority)) == 1) {
    return(z)
  }
  for (i in 2:length(ordered.by.priority)){
    z <- MergeRts(z, ordered.by.priority[[i]])
  }
  CheckDoclistRts(z)
  z
}
