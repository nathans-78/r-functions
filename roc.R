roc <- function(prob_values, labels, prob_thresh=seq(from=0, to=1, by=0.01), no_cores=1) {
  library(data.table)
  library(parallel)
  
  x_out <- mclapply(prob_thresh,
                    function(x) {
                      x_pred <- ifelse(prob_values>=x, 1, 0)
                      x_dt <- data.table(pred=x_pred, actual=labels)
                      tnr <- x_dt[pred==0 & actual==0, .N]/x_dt[actual==0, .N]
                      fpr <- x_dt[pred==1 & actual==0, .N]/x_dt[actual==0, .N]
                      fnr <- x_dt[pred==0 & actual==1, .N]/x_dt[actual==1, .N]
                      tpr <- x_dt[pred==1 & actual==1, .N]/x_dt[actual==1, .N]
                      precision <- x_dt[pred==1 & actual==1, .N]/x_dt[pred==1, .N]
                      recall <- x_dt[pred==1 & actual==1, .N]/x_dt[actual==1, .N]
                      f1_score <- (2*precision*recall)/(precision+recall)
                      x_res <- data.table(model=model_name, prob_threshold=x, tnr=tnr, fpr=fpr, fnr=fnr, tpr=tpr, precision=precision, recall=recall, f1_score=f1_score)
                      return(x_res)
                      },
                    mc.cores=no_cores)
  x_out <- rbindlist(l=x_out, use.names=TRUE, fill=FALSE)
  
  auc <- x_out[, .N, by=.(fpr, tpr)][, !c("N")][order(fpr, tpr)]
  auc[, fpr_prev:=c(NA, auc[1:(auc[, .N]-1), fpr])]
  auc[, tpr_prev:=c(NA, auc[1:(auc[, .N]-1), tpr])]
  auc[, tpr_min:=ifelse(abs(tpr_prev)<abs(tpr), tpr_prev, tpr)]
  auc[, tpr_max:=ifelse(abs(tpr_prev)>=abs(tpr), tpr_prev, tpr)]
  auc[, area:=(fpr-fpr_prev)*(tpr_min + 0.5*(tpr_max-tpr_min))]
  auc <- auc[!is.na(area), sum(area)]
  
  return(list("roc_dt"=x_out,
              "auc"=auc))
}
