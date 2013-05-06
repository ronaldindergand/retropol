#' plot method for an rts object
#' 
#' @param x             an rts object
#' @param ...           unused, for compatibility with the generic
#' @return plots the object as a side effect
#' @method plot rts
#' 
#' @export
#' 
plot.rts <- function(x, ...){
  ddt <- DocDatatype(x)
  
  int <- est <- ori <- x$data 
  
  ori[ddt!="original"] <- NA
  est[ddt!="estimated"] <- NA
  int[ddt!="interpolated"] <- NA
  
  plot(x$data , type = "n")
  lines(int, lty = "dotted")
  lines(est, lty = "dashed")
  lines(ori, lty = "solid")
}


#' plot method for an mrts object
#' 
#' @param x             an mrts object
#' @param ...           unused, for compatibility with the generic
#' @return plots the object as a side effect
#' @method plot mrts
#' 
#' @export
#' 
plot.mrts <- function(x, ...){
  par(mfrow=c(3,1))
  plot(x$y)
  plot(x$q)
  plot(x$m)
  par(mfrow=c(1,1))
}
