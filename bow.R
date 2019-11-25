library(data.table) 

bow <- function(text_list, n_gram=1, all_preceding_n_grams=TRUE, data.table_output=FALSE) {
  x <- text_list
  x <- strsplit(x=x, split=" ")
  x <- unlist(x=x)
  x <- x[x!=""]
  x_vec <- c()
  if (length(x)==0) {
    x_vec <- NULL # remove error of no values
  } else {
    if (all_preceding_n_grams==TRUE) { # all n-grams up to between unigram to n-gram
      for (i in 1:n_gram) {
        for (j in 1:max(1, length(x)-i+1)) {
          x_vec <- c(x_vec, paste(x[j:(j+i-1)], collapse=" "))
        }
      }
    } else {
      if (length(x)<n_gram) { # remove error of undefined n-gram
        x_vec <- NULL
      } else {
        for (k in 1:(length(x)-n_gram+1)) { # strictly select only n-gram
          x_vec <- c(x_vec, paste(x[k:(k+n_gram-1)], collapse=" "))
        }
      }
    }
  }
  x_tab <- table(x_vec)
  if (data.table_output==FALSE) { # only taken counts
    return(x_tab)
  } else { # return data.table output
    if (length(x_tab)==0) {
      x_dt <- data.table(`__empty__`=NA)
    } else {
      x_dt <- data.table(x_tab)
      colnames(x_dt) <- c("ind","val")
      x_nam <- x_dt[, ind]
      x_val <- x_dt[, val]
      x_dt <- data.table(t(x_dt[, val]))
      colnames(x_dt) <- x_nam
    }
    return(x_dt)
  }
}
