library(data.table)

roc <- function(dt, dt_act, model, model_name, pred_func="predict(object=model, newdata=dt, type=\"response\")", prob_thresh=seq(from=0, to=1, by=0.01)) {
  x_out <- lapply(prob_thresh,
                  function(x) {
                    x_prob <- eval(parse(text=pred_func))
                    x_pred <- ifelse(x_prob>=x, 1, 0)
                    x_dt <- data.table(pred=x_pred, actual=dt_act)
                    tnr <- x_dt[pred==0 & actual==0, .N]/x_dt[actual==0, .N]
                    fpr <- x_dt[pred==1 & actual==0, .N]/x_dt[actual==0, .N]
                    fnr <- x_dt[pred==0 & actual==1, .N]/x_dt[actual==1, .N]
                    tpr <- x_dt[pred==1 & actual==1, .N]/x_dt[actual==1, .N]
                    precision <- x_dt[pred==1 & actual==1, .N]/x_dt[pred==1, .N]
                    recall <- x_dt[pred==1 & actual==1, .N]/x_dt[actual==1, .N]
                    f1_score <- (2*precision*recall)/(precision+recall)
                    x_res <- data.table(model=model_name, prob_threshold=x, tnr=tnr, fpr=fpr, fnr=fnr, tpr=tpr, precision=precision, recall=recall, f1_score=f1_score)
                    return(x_res)
                  })
  
  x_out <- rbindlist(l=x_out, use.names=TRUE, fill=FALSE)
  return(x_out)
}
