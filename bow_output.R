library(data.table)

bow_output <- function(dt, na=TRUE, tf_idf=TRUE) {
  if (tf_idf==TRUE & na==TRUE) {
    return(dt[, lapply(.SD, function(x) x * log(length(x)/sum(!is.na(x))))])
  } else if (tf_idf==TRUE & na==FALSE) {
    return(dt[, lapply(.SD, function(x) x * log(length(x)/sum(ifelse(x!=0, 1, 0))))])
  } else if (tf_idf==FALSE & na==TRUE) {
    return(dt[, lapply(.SD, function(x) ifelse(!is.na(x), 1, NA))])
  } else if (tf_idf==FALSE & na==FALSE) {
    return(dt[, lapply(.SD, function(x) ifelse(x!=0, 1, 0))])
  } else {
    return(NULL)
  }
}
