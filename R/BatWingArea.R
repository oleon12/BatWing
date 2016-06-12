
BatWingArea <- function(x,scale=FALSE,method=c("BM","A","SS","P")){
  
  ## Bats measures are taken in mm and gr, if scale id true, all measures be in meters and kilograms
  
  if(scale==TRUE){
    
    x1 <- as.data.frame(mapply(function(x){x/1000},x[,2:length(colnames(x))]))
    x <- cbind(x[,1],x1)
    
  }
  
  ## Assing colum names
  
    ## Blood & MacFarlane 1998
    
    BM <- 2*((x[,2]* x[,3]) + (0.5*(x[,3]* x[,5])))
    
    ## Aldrige 1988
    
    A <- (2.85*(10^-3)) * x[,6]^2/3
    
    ## Smith & Starrett 1979
    
    alpha <- atan((x[,3]/x[,5]))
    
    SS <- 2*( (x[,2] * x[,3]) + ((1/2) * (cos(alpha) * x[,5] * x[,4])) + ((1/2) * (sin(alpha) * x[,4] * x[,3])) )
    
    ## Pirlot 1977
    
    P <- 0.735 * x[,3] * x[,7]
    
  r <- cbind(BM,A,SS,P)
  
  quote <- c("BM","A","SS","P")
  
  r <- r[, which(quote%in%method)]
  
  colnames(r) <- quote[which(quote%in%method)]
  
  rownames(r) <- x[,1]

  mean1 <- apply(x[,-1], 2, mean)
  mean2 <- apply(r, 2, mean)
  sd1 <- apply(x[,-1], 2, sd)
  sd2 <- apply(r, 2, sd)
  var1 <- apply(x[,-1], 2, var)
  var2 <- apply(r, 2, var)
    
  s1 <- rbind(mean1,sd1,var1)
  s2 <- rbind(mean2,sd2,var2)
  
  s <- cbind(s1,s2)

  rownames(s) <- c("mean","sd","var")
  
  l <- list(r,s)
  
  names(l) <- c("Area","Stats")
  
  ## END
  
  return(l)
  
}

