
BatWing1 <- function(x,scale=FALSE,method=c("BM-1988","A-1988","SS-1979","P-1977")){
  
  ## Bats measures are taken in mm and gr, if scale id true, all measures be in meters and kilograms
  
  if(scale==TRUE){
    
    x1 <- as.data.frame(mapply(function(x){x/1000},x[,2:length(colnames(x))]))
    x <- cbind(x[,1],x1)
    
  }
  
  ## Create outcome matrix
  
  r <- matrix(NA, 
              nrow= length(x[,1]),
              ncol=5)
  
  ## Assing colum names
  
  colnames(r) <- c("ID","BM-1988","A-1988","SS-1979","P-1977")
  
  for (i in 1:length(r[,1])){
    
    ## Blood & MacFarlane 1998
    
    BM <- 2*((x[i,2]* x[i,3]) + (0.5*(x[i,3]* x[i,5])))
    r[i,2] <- as.numeric(BM)
    
    ## Aldrige 1988
    
    A <- (2.85*(10^-3)) * x[i,6]^2/3
    r[i,3] <- as.numeric(A)
    
    ## Smith & Starrett 1979
    
    alpha <- atan((x[i,3]/x[i,5]))
    
    SS <- 2*( (x[i,2] * x[i,3]) + ((1/2) * (cos(alpha) * x[i,5] * x[i,4])) + ((1/2) * (sin(alpha) * x[i,4] * x[i,3])) )
    r[i,4] <- as.numeric(SS)
    
    ## Pirlot 1977
    
    P <- 0.735 * x[i,3] * x[i,7]
    r[i,5] <- as.numeric(P)
    
  }
  
  r <- as.data.frame(r)
  
  ## Assing ID
  
  r$ID <- x[,1]
  
  # Find the requested methods
  
  meth <- which(colnames(r)%in%method)
  
  n.names <- c("ID",colnames(r)[meth])
  
  r <- cbind(r$ID, r[,meth])
  
  r <- as.data.frame(r)
  
  colnames(r) <- n.names
  
  r$ID <- as.character(x[,1])
  
  ## END
  
  return(r)
  
}
