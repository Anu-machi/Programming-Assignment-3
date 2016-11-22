rankall <- function(outcome, num = "best"){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
  bestdf   <- as.data.frame(cbind(data[, 2], 
                              data[, 7],  
                              data[, 11],  
                              data[, 17],  
                              data[, 23]), 
                        stringsAsFactors = FALSE)
  colnames(bestdf) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  bestdf[, eval(outcome)] <- as.numeric(bestdf[, eval(outcome)])
  
  ## Check that state and outcome are valid
  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else if (is.numeric(num)) {
    stateby <- with(bestdf, split(bestdf, state))
    ordered  <- list()
    for (i in seq_along(stateby)){
      stateby[[i]] <- stateby[[i]][order(stateby[[i]][, eval(outcome)], 
                                         stateby[[i]][, "hospital"]), ]
      ordered[[i]]  <- c(stateby[[i]][num, "hospital"], stateby[[i]][, "state"][1])
    }
    results <- do.call(rbind, ordered)
    output <- as.data.frame(result, row.names = results[, 2], stringsAsFactors = FALSE)
    names(output) <- c("hospital", "state")
  } else if (!is.numeric(num)) {
    if (num == "best") {
      stateby <- with(bestdf, split(bestdf, state))
      ordered  <- list()
      for (i in seq_along(stateby)){
        stateby[[i]] <- stateby[[i]][order(stateby[[i]][, eval(outcome)], 
                                           stateby[[i]][, "hospital"]), ]
        ordered[[i]]  <- c(stateby[[i]][1, c("hospital", "state")])
      }
      results <- do.call(rbind, ordered)
      output <- as.data.frame(results, stringsAsFactors = FALSE)
      rownames(output) <- output[, 2]
    } else if (num == "worst") {
      stateby <- with(bestdf, split(bestdf, state))
      ordered  <- list()
      for (i in seq_along(stateby)){
        stateby[[i]] <- stateby[[i]][order(stateby[[i]][, eval(outcome)], 
                                           stateby[[i]][, "hospital"], 
                                             decreasing = TRUE), ]
        ordered[[i]]  <- c(stateby[[i]][1, c("hospital", "state")])
      }
      results <- do.call(rbind, ordered)
      output <- as.data.frame(results, stringsAsFactors = FALSE)
      rownames(output) <- output[, 2]
    } else {
      stop('invalid num')
    }
  }
  return(output)
}
