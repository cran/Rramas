projectn <-
function(v0, mat, matsd=NULL, estamb=FALSE,
         estdem=FALSE, equalsign=TRUE,
         stmat=NULL, fecundity1=TRUE,
         nrep=1, time=10){
  
  #check names of stages, etc
  if(sum(class(mat)=="tmatrix")==0) mat <- as.tmatrix(mat)
  # st.names <-dimnames(mat)[[1]]
    
  vn <- NULL
  for (i in 1:nrep){
    vn[[i]]<- cbind(v0,v0) #repeat v0 to facilitate loop computations
  }
  for (i in 1:time){
    vn <- lapply(vn, function(x) cbind(x,
                      project1(v0=x[,i+1], mat=mat, matsd=matsd,
                               estamb=estamb, estdem=estdem,
                               equalsign=equalsign,stmat=stmat,
                               fecundity1=fecundity1)))
  }
  vn <- lapply(vn,function(x) x[,-1]) #get rid of the repeated v0
  #rownames(vn) <- st.names
  class(vn) <- c("rmas", class(vn))  
  return(vn)
}

