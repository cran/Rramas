estambi <-
function(mat, matsd, equalsign){
   if(equalsign!=TRUE){
     mat <- matrix(rnorm(length(mat), mean=mat, sd=matsd),nrow=dim(mat)[1], ncol=dim(mat)[2])
    }
   if(equalsign==TRUE){
     mat <- mat + (matsd*rnorm(1,mean=0,sd=1))
    }
    #check/correct negative values and values >1
    mat[mat<0] <- 0
    mat[mat>1] <-1
    return(mat)
}

