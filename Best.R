best <- function(state, outcome)
  {
  
    data <-read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")## Read outcome data
  
    bestdf <- as.data.frame(cbind(data[,2], data[, 7], data[,11], data[, 17], data[,23]), stringsAsFactors = FALSE)
  
    colnames(bestdf) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  
  ## Check that state and outcome are valid
    if(!state %in% bestdf[, "state"])
      {
      stop('invalid state')
      } 
    else if(!outcome %in% c("heart attack", "heart failure", "pneumonia"))
      {
        stop('invalid outcome')
      } 
    else 
      {
        st <- which(bestdf[, "state"] == state)
        stdata <- bestdf[st, ]    # extracting data for the called state
        otcom <- as.numeric(stdata[, eval(outcome)])
        minval <- min(otcom, na.rm = TRUE)
        results  <- stdata[, "hospital"][which(otcom == minval)]
        output  <- results[order(results)]
      }
  return(output)
  }