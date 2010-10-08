summary.rmas <-
function(object, stage=NULL,...){
  # genera una tabla sumario semejante al "show Numbers" de Ramas
       cosa <- object
       nl <- length (cosa)
       time <- dim(cosa[[1]])[2]
    if (nl>1){
      abundances <-sapply(cosa, function(x) apply(x,2,sum))
      if (!is.null(stage)) abundances <- sapply(cosa, function(x) x[stage,])
      summary <- apply(abundances,1, function(x) c(min(x), mean(x)-sd(x), 
                                                   mean(x),mean(x)+sd(x),
                                                   max(x))) 
      summary <- t(summary)
      summary <- data.frame(cbind(0:(time-1), round(summary,2)),
                             row.names=0:(time-1))
      names(summary) <- c("Time", "Minimum", "-1 S.D.", "Average",
                          "+1 S.D.", "Maximum")
    }
    if(nl==1){
      abundances <-apply(cosa[[1]], 2,sum)
      if (!is.null(stage)) abundances <- apply(cosa,function(x) x[stage,])
                        summary <- cbind(0:(time-1),abundances)
      colnames(summary) <- c("Time", "Abundance")
    }
    ##
    summary <- data.frame(summary,row.names=NULL,check.names=FALSE)
    ##
  class(summary) <- c("summary.rmas",class(summary))
        plot(summary)
  return(summary)
  }

