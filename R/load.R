
#' loads raw data from a specified folder
#' 
#' @param file       file destination
#' @return           rts object
#' @export
LoadFile <- function(file){

  dta <- read.table(file, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  
  z <- Rts(
    ts(dta$data, start = dta$start_series[1], frequency = dta$frequency[1]), 
    id = dta$id[1],
    between = dta$between[1],
    within = dta$within[1]
  )
  
  z$seasonal.adj <- as.logical(dta$seasonal_adj[1])
  
  if (time(z$data)[length(z$data)] != dta$end_series[1]){
    stop (paste0("end of series (", time(z$data)[length(z$data)],
                 ") is different from end_series input."))
  }
  
  # determine the number of doc objects
  # index with source entries
  doc.ind <- which(dta$source != "")
  
  # index with remarks entries
  rem.ind <- which(dta$remarks != "")
  
  doc.counter <- 0
  # add one or more doc indices
  for (i in doc.ind){
    
    if (is.na(dta$start_doc[i])){
      start <- NULL
    } else {
      start <- dta$start_doc[i]
    }
    
    if (is.na(dta$end_doc[i])){
      end <- NULL
    } else {
      end <- dta$end_doc[i]
    }
    
    # this is a bit redundand, i is equal to doc.ind[doc.counter]
    doc.counter <- doc.counter + 1
    z <- DocAddRts(z, source = dta$source[i], web = dta$web[i], 
                start = start, end = end, datatype = "original")
    
    ## add remarks
    # remarks start at i, where do they end:
    if (is.na(doc.ind[i+1])){
      last.rem <- rem.ind[length(rem.ind)]
      
    } else {
      last.rem <- doc.ind[i+1]-1
    }
    
    selected.ind <- which(rem.ind >= i & rem.ind <= last.rem)
    
    for (each in selected.ind){
      z$doclist[[doc.counter]]$remarks <- c(z$doclist[[doc.counter]]$remarks, dta$remarks[each])
    }
  }
  
  z
  
}

