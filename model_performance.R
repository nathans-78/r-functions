library(data.table)

model_performance <- function(model, predict, actual) {
  y_class <- unique(sort(c(predict, actual)))
  X <- data.table(predict=predict, actual=actual)
  accuracy <- X[predict==actual, .N]/X[, .N]
  confusion_matrix <- table(X[, predict], X[, actual])
  results <- lapply(y_class,
                    function(y) {
                      results <- data.table(model=model,
                                            class=y,
                                            precision=X[predict==y & actual==y, .N]/X[predict==y, .N],
                                            recall=X[predict==y & actual==y, .N]/X[actual==y, .N])
                      results[, f1_score:=(2*precision*recall)/(precision+recall)]
                      results[, weight:=X[actual==y, .N]/X[, .N]]
                      return(results)
                    })
  results <- rbindlist(l=results, use.names=TRUE, fill=TRUE)
  results[, names(results[, !c("model","class")]):=lapply(.SD, function(x) ifelse(is.nan(x) | is.na(x), 0, x)), .SDcols=names(results[, !c("model","class")])]
  results_consolidated <- results[, .(model,
                                      accuracy=accuracy,
                                      precision,
                                      weighted_precision=precision*weight,
                                      recall,
                                      weighted_recall=recall*weight,
                                      f1_score,
                                      weighted_f1_score=f1_score*weight)]
  results_consolidated <- results_consolidated[, .(accuracy=mean(accuracy),
                                                   mean_precision=mean(precision),
                                                   weighted_precision=sum(weighted_precision),
                                                   mean_recall=mean(recall),
                                                   weighted_recall=sum(weighted_recall),
                                                   mean_f1_score=mean(f1_score),
                                                   weighted_f1_score=sum(weighted_f1_score)),
                                               by=.(model)]
  return(list("confusion_matrix"=confusion_matrix,
              "performance_results"=results,
              "results_consolidated"=results_consolidated))
}
