### Set profiler function ###
## Profiler.
profiler <- function(dt_population_1, dt_population_2, id, variables, no_cores=1) {
  # load libraries
  library(data.table)
  library(parallel)
  
  # convert to data.table
  setDT(dt_population_1)
  setDT(dt_population_2)
  
  # loop through each variable and aggregate stats
  dt_1 <- mclapply(variables,
                   function(x1) {
                     # print attribute
                     print(paste0("attribute ", x1))
                     
                     # start time
                     start_time <- proc.time()
                     
                     # population distribution
                     dt_2 <- dt_population_1[!is.na(get(x1)), uniqueN(get(id)), get(x1)]
                     colnames(dt_2) <- c("value","population_1_count")
                     dt_2[, attribute:=x1]
                     dt_2[, population_1_percent:=population_1_count/sum(population_1_count), by=.(attribute)]
                     
                     # sample distribution
                     dt_3 <- dt_population_2[!is.na(get(x1)), uniqueN(get(id)), get(x1)]
                     colnames(dt_3) <- c("value","population_2_count")
                     dt_3[, attribute:=x1]
                     dt_3[, population_2_percent:=population_2_count/sum(population_2_count), by=.(attribute)]
                     
                     # end time
                     end_time <- proc.time()
                     
                     # print time results
                     print(paste0(round((end_time-start_time)[3], 0), " seconds"))
                     
                     # combine stats
                     dt_4 <- merge(dt_2, dt_3, by.x=c("attribute","value"), by.y=c("attribute","value"), all=TRUE)
                     dt_4[, c("population_1_count","population_1_percent","population_2_count","population_2_percent"):=lapply(.SD, function(x) ifelse(is.na(x), 0, x)), .SDcols=c("population_1_count","population_1_percent","population_2_count","population_2_percent")]
                     
                     # Evaluate z-scores per value proportion and information value
                     # Note we add dummy value to replace 0 counts
                     dt_4[, c("population_1_count","population_2_count"):=lapply(.SD, function(x) ifelse(is.na(x), 0, x)), .SDcols=c("population_1_count","population_2_count")]
                     dt_4[, population_1_count_dummy:=ifelse(population_1_count==0, 1, population_1_count)]
                     dt_4[, population_1_percent_dummy:=population_1_count_dummy/sum(population_1_count_dummy), by=.(attribute)]
                     dt_4[, population_2_count_dummy:=ifelse(population_2_count==0, 1, population_2_count)]
                     dt_4[, population_2_percent_dummy:=population_2_count_dummy/sum(population_2_count_dummy), by=.(attribute)]
                     z_score <- sapply(1:nrow(dt_4),
                                       function(x2) {
                                         z_val <- prop.test(x=c(dt_4[x2, population_1_count_dummy], dt_4[x2, population_2_count_dummy]),
                                                            n=c(dt_4[, sum(population_1_count_dummy)], dt_4[, sum(population_2_count_dummy)]),
                                                            alternative="two.sided")
                                         z_val <- sign(z_val$estimate[1]-z_val$estimate[2])*sqrt(z_val$statistic)
                                         return(z_val)
                                       })
                     dt_4[, z_score:=z_score]
                     dt_4[, information_value:=(population_1_percent_dummy-population_2_percent_dummy)*log(x=(population_1_percent_dummy/population_2_percent_dummy), base=exp(1))]
                     dt_4[, population_1_count_dummy:=NULL]
                     dt_4[, population_1_percent_dummy:=NULL]
                     dt_4[, population_2_count_dummy:=NULL]
                     dt_4[, population_2_percent_dummy:=NULL]
                     
                     return(dt_4)
                   },
                   mc.cores=no_cores)
  
  # combine and return results
  dt_1 <- rbindlist(l=dt_1, use.names=TRUE, fill=TRUE)
  return(dt_1)
}
