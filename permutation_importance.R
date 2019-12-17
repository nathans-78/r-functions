library(data.table)

permutation_importance <- function(dt, y, model, model_name, pred_func="predict(object=model, newdata=dt, type=\"response\")", loss="logloss", class_0_1_prob=TRUE, n_perm=20, newdata_command_update=NULL) {
  # names of predictor variables
  dt_x_var <- names(dt[, .SD, .SDcols=-y])
  
  # distribution of predictions based on randomising each variable
  x_out1 <- lapply(dt_x_var,
                   function(x1) {
                     if (loss=="logloss") { # log-loss output
                       if (class_0_1_prob==TRUE) { # for binary classification with only (0,1) set values
                         # log-loss for model
                         dt_temp <- dt # dummy temp table to replaced by original
                         x_char <- sapply(dt, is.character) # transform character variables to factor, maintain order in model.matrix pivot
                         x_char <- names(x_char[x_char==TRUE])
                         suppressWarnings(dt[, c(x_char):=lapply(.SD, as.factor), .SDcols=x_char])
                         dt <- data.table(dt[, ..y], model.matrix(~., data=dt[, .SD, .SDcols=-y])[, -1])
                         y_prob <- eval(parse(text=pred_func)) # output of probabilities
                         y_prob <- data.table(actual=dt[, get(y)], y_prob)
                         y_loss_temp <- sapply(1:dt[, .N],
                                               function(i) {
                                                 return(max(1e-04, y_prob[i, ifelse(actual==1, y_prob, 1-y_prob)]))
                                               })
                         # y_loss_temp <- y_prob[, ifelse(actual==1, y_prob, 1-y_prob)]
                         y_loss <- -sum(log(y_loss_temp))
                         dt <- dt_temp # return original dataset
                         
                         # log-loss based on each randomised permutation for selected variable
                         x_out2 <- lapply(1:n_perm,
                                          function(x2) {
                                            dt_2 <- dt
                                            x_2_char <- sapply(dt_2, is.character) # transform character variables to factor, maintain order in model.matrix pivot
                                            x_2_char <- names(x_2_char[x_2_char==TRUE])
                                            suppressWarnings(dt_2[, c(x_2_char):=lapply(.SD, as.factor), .SDcols=x_2_char])
                                            dt_2[, (x1):=copy(dt_2[sample(x=1:dt_2[, .N], size=dt_2[, .N], replace=FALSE), get(x1)])]
                                            dt_2 <- data.table(dt_2[, ..y], model.matrix(~., data=dt_2[, .SD, .SDcols=-y])[, -1])
                                            if (is.null(newdata_command_update)) {
                                              y_2_prob <- eval(parse(text=gsub("newdata=dt", "newdata=dt_2", pred_func)))
                                            } else {
                                              y_2_prob <- eval(parse(text=gsub(paste0(newdata_command_update, "=dt", newdata_command_update, "=dt_2", pred_func))))
                                            }
                                            y_2_prob <- data.table(actual=dt_2[, get(y)], y_2_prob)
                                            y_2_loss_temp <- sapply(1:dt_2[, .N],
                                                                    function(i) {
                                                                      return(max(1e-04, y_2_prob[i, ifelse(actual==1, y_2_prob, 1-y_2_prob)]))
                                                                    })
                                            # y_2_loss_temp <- y_2_prob[, ifelse(actual==1, y_2_prob, 1-y_2_prob)]
                                            y_2_loss <- -sum(log(y_2_loss_temp))
                                            dt_2 <- data.table(model_name=model_name, variable=x1, iteration=x2, loss_func_diff=y_2_loss-y_loss)
                                            return(dt_2)
                                          })
                         x_out2 <- rbindlist(l=x_out2, use.names=TRUE, fill=TRUE)
                         return(x_out2)
                       } else if (class_0_1_prob==FALSE) { # for multi-class classification
                         # log-loss for model
                         dt_temp <- dt # dummy temp table to replaced by original
                         x_char <- sapply(dt, is.character) # transform character variables to factor, maintain order in model.matrix pivot
                         x_char <- names(x_char[x_char==TRUE])
                         suppressWarnings(dt[, c(x_char):=lapply(.SD, as.factor), .SDcols=x_char])
                         dt <- data.table(dt[, ..y], model.matrix(~., data=dt[, .SD, .SDcols=-y])[, -1])
                         y_prob <- eval(parse(text=pred_func)) # output of probabilities per class
                         y_loss_temp <- cbind(data.table(y_prob), data.table(y_act=dt[, as.character(get(y))])) # combine matrix of probabilities for each class
                         y_loss_temp <- sapply(1:dt[, .N], # iterate through each prediction and select only the output with their respective actual class
                                               function(i) {
                                                 return(max(1e-04, y_loss_temp[i, get(y_act)]))
                                               })
                         y_loss <- -sum(log(y_loss_temp))
                         dt <- dt_temp # return original dataset
                         
                         # log-loss based on each randomised permutation for selected variable
                         x_out2 <- lapply(1:n_perm,
                                          function(x2) {
                                            dt_2 <- dt
                                            x_2_char <- sapply(dt_2, is.character) # transform character variables to factor, maintain order in model.matrix pivot
                                            x_2_char <- names(x_2_char[x_2_char==TRUE])
                                            suppressWarnings(dt_2[, c(x_2_char):=lapply(.SD, as.factor), .SDcols=x_2_char])
                                            dt_2[, (x1):=dt_2[sample(x=1:dt_2[, .N], size=dt_2[, .N], replace=FALSE), get(x1)]]
                                            dt_2 <- data.table(dt_2[, ..y], model.matrix(~., data=dt_2[, .SD, .SDcols=-y])[, -1])
                                            if (is.null(newdata_command_update)) {
                                              y_2_prob <- eval(parse(text=gsub("newdata=dt", "newdata=dt_2", pred_func)))
                                            } else {
                                              y_2_prob <- eval(parse(text=gsub(paste0(newdata_command_update, "=dt", newdata_command_update, "=dt_2", pred_func))))
                                            }
                                            y_2_loss_temp <- cbind(data.table(y_2_prob), data.table(y_act=dt_2[, as.character(get(y))])) # output of probabilities per class
                                            y_2_loss_temp <- sapply(1:dt_2[, .N], # iterate through each prediction and select only the output with their respective actual class
                                                                    function(i) {
                                                                      return(max(1e-04, y_2_loss_temp[i, get(y_act)]))
                                                                    })
                                            y_2_loss <- -sum(log(y_2_loss_temp))
                                            dt_2_out <- data.table(model_name=model_name, variable=x1, iteration=x2, loss_func_diff=y_2_loss-y_loss)
                                            return(dt_2_out)
                                          })
                         x_out2 <- rbindlist(l=x_out2, use.names=TRUE, fill=TRUE)
                         return(x_out2)
                       } else {
                         return(NULL)
                       }
                     } else if (loss=="sse") { # sums of squares output
                       # sse for model
                       dt_temp <- dt # dummy temp table to replaced by original
                       x_char <- sapply(dt, is.character) # transform character variables to factor, maintain order in model.matrix pivot
                       x_char <- names(x_char[x_char==TRUE])
                       suppressWarnings(dt[, c(x_char):=lapply(.SD, as.factor), .SDcols=x_char])
                       dt <- data.table(dt[, ..y], model.matrix(~., data=dt[, .SD, .SDcols=-y])[, -1])
                       y_pred <- eval(parse(text=pred_func)) # regression predictions
                       y_loss <- sum((dt[, get(y)]-y_pred)^2) # sse
                       dt <- dt_temp # return original dataset
                       
                       # see based on each randomised permutation for each variable
                       x_out2 <- lapply(1:n_perm,
                                        function(x2) {
                                          dt_2 <- dt
                                          x_2_char <- sapply(dt_2, is.character) # transform character variables to factor, maintain order in model.matrix pivot
                                          x_2_char <- names(x_2_char[x_2_char==TRUE])
                                          suppressWarnings(dt_2[, c(x_2_char):=lapply(.SD, as.factor), .SDcols=x_2_char])
                                          dt_2[, (x1):=copy(dt_2[sample(x=1:dt_2[, .N], size=dt_2[, .N], replace=FALSE), get(x1)])]
                                          dt_2 <- data.table(dt_2[, ..y], model.matrix(~., data=dt_2[, .SD, .SDcols=-y])[, -1])
                                          if (is.null(newdata_command_update)) {
                                            y_2_pred <- eval(parse(text=gsub("newdata=dt", "newdata=dt_2", pred_func)))
                                          } else {
                                            y_2_pred <- eval(parse(text=gsub(paste0(newdata_command_update, "=dt", newdata_command_update, "=dt_2", pred_func))))
                                          }
                                          y_2_loss <- sum((dt_2[, get(y)]-y_2_pred)^2)
                                          dt_2_out <- data.table(model_name=model_name, variable=x1, iteration=x2, loss_func_diff=y_2_loss-y_loss)
                                          return(dt_2_out)
                                        })
                       x_out2 <- rbindlist(l=x_out2, use.names=TRUE, fill=TRUE)
                       return(x_out2)
                     } else {
                       return(NULL)
                     }
                   })
  
  # combine and return results
  x_out <- rbindlist(l=x_out1, use.names=TRUE, fill=TRUE)
  return(x_out)
}
