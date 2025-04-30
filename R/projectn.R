projectn <-
function (v0, mat, matsd = NULL, estamb = FALSE, estdem = FALSE, 
    equalsign = TRUE, stmat = NULL, fecundity1 = TRUE, nrep = 1, 
    time = 10, management = NULL, round = TRUE) 
{
    if (sum(class(mat) == "tmatrix") == 0) 
        mat <- as.tmatrix(mat)
    vn <- NULL
    vm <- NULL

    # control the number of management actions--------------------
     if (!is.null(management)) {

       #1) if management is a matrix
	if(!is.null(dim(management))){
	  if(nrow(management) != nrow(mat)) stop("You should provide management actions for each stage\n i.e., nrow(management) == nrow(mat) \n\n")
            # if there are less management columns than time steps, add extra 0 columns
	  if(ncol(management) < time) {
             warning("You provided less management actions than time steps\n i.e., ncol(management) < time\n\n")
              extra <- matrix (0, nrow =nrow(mat), ncol=time - ncol(management))
              management <- cbind(management, extra)
	  }
           # if there are more management columns than time steps, warn about that
	  if(ncol(management) > time) {
             warning("You provided more management actions than time steps\n i.e., ncol(management) > time\n\n")       
	  }
        }

        # 2) if management  is a vector: turn it into a matrix
         if(is.null(dim(management))) {
             if(length(management) < nrow(mat)) stop("You should provide a management action for each stage\n   i.e., length(management) == nrow(mat) \n\n")
             # management matrix with just one column
 	     management <- matrix(management, nrow = nrow(mat))
           }
    } # end management control------------------------------------------------

    for (i in 1:nrep) {
        vn[[i]] <- cbind(v0, v0)
        vm[[i]] <- cbind(v0 * NA, v0 * NA)
    }
    for (i in 1:time) {
        for (ii in 1:nrep) {
            v <- project1(v0 = vn[[ii]][, i + 1], mat = mat, 
                matsd = matsd, estamb = estamb, estdem = estdem, 
                equalsign = equalsign, stmat = stmat, fecundity1 = fecundity1)
            if (round == TRUE) 
                v <- round(v)
            v.0 <- v
            if (!is.null(management)) {
                # management actions for this time step
                 # if(ncol(management) < 2) management.i <- management else  management.i <- management[,i]
                 if(ncol(management) < 2) management.i <- as.numeric(management) else  management.i <- as.numeric(management[,i])

                 # control if some management action referes to proportion of individuals rather than absolute number of individuals
                 # i.e., if  the values in management are < 1
                 proportions <- abs(management.i) < 1  
                 management.i [proportions] <-  (round(v * management.i))[proportions]
                 v <- v + management.i
		  v[v < 0] <- 0
            }
            v.m <- v.0 - v
            harvest <- v.m
            harvest[v.m < 0] <- 0
            #} # nuevo

            vn[[ii]] <- cbind(vn[[ii]], v)
            vm[[ii]] <- cbind(vm[[ii]], harvest)
        }
    }
    vn <- lapply(vn, function(x) x[, -1])
    vm <- lapply(vm, function(x) x[, -1])
    vnm <- list(vn = vn, harvest = vm, mat = mat, management = management)
    class(vnm) <- c("rmas", class(vnm))
    return(vnm)
}
