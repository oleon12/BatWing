BatWingDynamics <- function(x,scale=FALSE,method=c("AR","WL","RWL","MPS")){

  if(scale==T){
    
    x[ , 3] <- x[,3]/1000 ## Transform
    
  }
  
  ## Aspect Ratio
  
  AR <- (x[,3]^2)/x[,4] ## Squared wingspan divided by the Area (LSA) in m2
  
  ## Wingl Loading
  
  WL <- (x[,2]*9.81)/x[,4] ## Mass multiplied by gravity constant, divided by Area (LSA) in m2
  
  ## Relative Wing Loading
  # Make the Wing Loading independent of size for morphometrically similar species
  
  RWL <- WL/(x[,2]^(1/3))
  
  ## Minimum Power Speed
  
  MPS <- 6.58* (x[,2]^0.422)* (x[,3]^-0.479)* (x[,4]^-0.148)
  
  
  r1 <- cbind(AR,WL,RWL,MPS)
  
  quote <- c("AR","WL","RWL","MPS")
  
  r <- r1[,which(quote%in%method)]
  
  colnames(r) <- quote[which(quote%in%method)]
  
  rownames(r) <- x[,1]
  
  return(r)
  
}



