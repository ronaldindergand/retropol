#' Creates a rts object
#' 
#' @param data          a ts object
#' @param id            identification string
#' @param between       scalar, the priority between frequencies
#' @param within        scalar, the priority within frequencies
#' @return An rts object, a list containing the elements
#' \item{data}{a ts object}
#' \item{doclist}{an empty doclist object}
#' \item{between}{scalar, the priority between frequencies}
#' \item{within}{scalar, the priority within the frequency}
#' \item{id}{identification string}
#' @export
Rts <- function(data, id, between = 0, within = 0){
  z <- list()
  z$data <- data
  z$id   <- id
  z$between <- between
  z$within <- within
  z$doclist <- NULL
  class(z) <- "rts"
  z
}

#' Retropolates and/or extrapolates a time series with another
#' 
#' @param x1          rts object with higher priority
#' @param x2          rts object with lower priority
#' @param type        type of retro: "PC", "SUBST"
#' @return An rts object
#' @import tstools
MergeRts <- function(x1, x2, type = "PC"){
  z <- x1
  
  # Retropolation
  if (time(x1$data)[1] > time(x2$data)[1]){
    anchor <- window(z$data, end = start(z$data))
    extra <- window(Extrapol(x = PC(x2$data), anchor = anchor, type = "PC"), 
                    end = start(z$data))
    z$data <- merge.ts(z$data, extra)
    .x2 <- DocRemarkRts(x2, text = "Percentage change rates from this series were used to retropolate the series.")
    doclist.x2 <- window(.x2$doclist, end = time(x1$data)[1] - 1/frequency(x1$data))
    z$doclist <- c(doclist.x2, z$doclist)
  }
  
  # Extrapolation
  if (time(x2$data)[length(x2$data)] > time(x1$data)[length(x1$data)]){
    anchor <- window(z$data, start = end(z$data))
    extra <- window(Extrapol(x = PC(x2$data), anchor = anchor, type = "PC"),
                    start = end(z$data))
    z$data <- merge.ts(z$data, extra)
    
    .x2 <- DocRemarkRts(x2, text = "Percentage change rates from this source were used to extrapolate the series.")
    doclist.x2 <- window(.x2$doclist, start = time(x1$data)[length(x1$data)] + 1/frequency(x1$data))
    z$doclist <- c(doclist.x2, z$doclist)
  }
  class(z$doclist) <- "doclist"
  CheckDoclistRts(z)
  z
}

#' performs a temporal agregation of an rts object
#' 
#' Calls ta from the tempdisagg package
#' @param x             an rts object
#' @param conversion    type of conversion ("sum", "average", "first", "last")
#' @param to            destination frequency (1, or 4)
#' @return An rts object
#' @export
AggregateRts <- function(x, conversion = "sum", to = 1){
  stopifnot(inherits(x, "rts"))
  if (x$between < 1){
    warning("between priority below 1, something is probably wrong")
  }
  
  require(tempdisagg)
  
  # string 
  high.frequency <- frequency(x$data)
  if (high.frequency == 4){
    from = "quarterly"
  } else if (high.frequency == 12){
    from = "monthly"
  } else {
    stop("frequency not supported.")
  }
  
  z <- x
  z$data <- ta(x$data, conversion = conversion, to = to)
  
  # Adjust the doclist
  z$doclist <- lapply(x$doclist, WindowDocAdj, x = x, to = to)
  class(z$doclist) <- "doclist"

  z <- DocRemarkRts(z, text = paste("Aggregated from", from, "data. Conversion type:", conversion))
  CheckDoclistRts(z)
  z$between <- x$between - 1
  
  z
}



#' Adjust the time attributes od a doc object to a lower frequency
#' 
#' @param doc.i           a doc object
#' @param x              an rts object
#' @param to              scalar, the frequency ot aggregate
#' @return an adjusted doc object.
#' @export
#' @examples
#' data(rts.with.multiple.na)
#' InterpolRts(x)
WindowDocAdj <- function(doc.i, x, to){
  stopifnot(inherits(doc.i, "doc"))
  stopifnot(inherits(x, "rts"))
  tmp <- ta(window(x$data, start = doc.i$start, end = doc.i$end), to = to)
  window.doc(doc.i, start = time(tmp)[1], end = time(tmp)[length(tmp)]) 
}




#' Interpolates missing values in a rts object
#' 
#' The Stineman interpolation restricts the range of the interpolant to the
#' nearby range of the points and suppresses the well known oscillations
#' characteristic of splines and other methods based on polynomials.
#' 
#' @param x             an rts object
#' @return An rts object
#' @export
#' @examples
#' data(rts.with.multiple.na)
#' InterpolRts(x)
InterpolRts <- function(x){
  z <- x
  data.na <- NAConnectedTs(x$data)
  
  if(max(data.na) > 0){
    require(stinepack)
    z$data <- na.stinterp(x$data)
    for (i in 1:max(data.na)){
      na.period.i <- time(data.na)[data.na==i]
      start <- na.period.i[1]
      end   <- na.period.i[length(na.period.i)]
      z <- DocAddRts(z, source = "Interpolated Data", datatype = "interpolated", 
                  start = start, end = end)
    }
  }
  z
}



#' Save an object in ./data under the same name
#' 
#' Overwrite if the object already exists
#' 
#' @param x    an object
#' @export
keep <- function(x){
  file <- paste0("./package/data/", as.character(substitute(x)), ".RData")
  save(x, file = file)
  cat("saved as", file)
}


#' Returns a ts with an index of all connected NAs.
#' 
#' Could be improved, get rid of the loop
#' 
#' @param x             an ts object
#' @return An ts object
#' @export
#' @examples
#' x <- ts(rnorm(100), start = 1900)
#' x[c(39:50, 80:85)] <- NA
#' NAConnectedTs(x)
NAConnectedTs <- function(x){  
  z <- x
  window(z) <- 0
  data.na <- is.na(x)
  index <- 0
  for (i in 1:length(x)){
    if (data.na[i]){
      if(!z[i-1]){
        index <- index + 1
      }
      z[i] <- index
    }
  }
  z
}
  


#' window method for rts class
#' 
#' @param x          rts object
#' @param start      start of rts
#' @param end        end of rts
#' @param ...        unused, for compatibility with the generic
#' @return           rts object
#' @method window rts
#' @import stats
#' @export
window.rts <- function(x, start = NULL, end = NULL, ...){
  if (is.null(start)){
    start <- start(x$data)
  }
  if (is.null(end)){
    end <- end(x$data)
  }
  
  z <- x
  
  # Apply standard window to the ts object in data
  z$data <- window(x$data, start = start, end = end)
  
  # Adjust the doclist
  z$doclist <- lapply(x$doclist, window.doc, start = start, end = end)
  
  # removing NULL elements
  z$doclist <- z$doclist[-(which(sapply(z$doclist, is.null), arr.ind = TRUE))]
  
  z
}
