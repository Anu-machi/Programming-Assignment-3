rankhospital <- function(state, outcome, rank = "best"){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
  bestdf   <- as.data.frame(cbind(data[, 2],  # hospital
                                  data[, 7],  
                                  data[, 11], 
                                  data[, 17],  
                                  data[, 23]), 
                            stringsAsFactors = FALSE)
  colnames(bestdf) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if (!state %in% bestdf[, "state"]) {
    stop('invalid state')
  } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } 
  else if (is.numeric(rank)) {
    st <- which(bestdf[, "state"] == state)
    stdata <- bestdf[st, ]                     # extracting dataframe for the called state
    stdata[, eval(outcome)] <- as.numeric(stdata[, eval(outcome)])
    stdata <- stdata[order(stdata[, eval(outcome)], stdata[, "hospital"]), ]
    output <- stdata[, "hospital"][rank]
  } 
  else if (!is.numeric(rank)){
    if (rank == "best") {
      output <- best(state, outcome)
    } else if (rank == "worst") {
      st <- which(bestdf[, "state"] == state)
      stdata <- bestdf[st, ]    
      stdata[, eval(outcome)] <- as.numeric(stdata[, eval(outcome)])
      stdata <- stdata[order(stdata[, eval(outcome)], stdata[, "hospital"], decreasing = TRUE), ]
      output <- stdata[, "hospital"][1]
    } else {
      stop('invalid rank')
    }
  }
  return(output)
}
