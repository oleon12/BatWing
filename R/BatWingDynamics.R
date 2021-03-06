#'
#' @title Wing's dynamic measures. 
#' 
#' @description Calculate the Dynamics measures for a Bat wing.
#' 
#' @return The function return four Dynamics meaures.
#'
#' @param x is the input matrix. This matrix is very specific, check the example.
#' 
#' @param scale if you want to transform milimeters and grames to meters and kilograms.
#' 
#' @param method is the LSA calculation method ("AR","WL","RWL","MPS").
#' 
#' @return The function returns a S3 object with two matrices. First one
#' is the Dynamics measures, and second one have basic statistics for the input
#' variables and the Dynamic measures values.
#' 
#' @examples
#' library(BatWing)
#' data(dynamics)
#'
#' BatWingDynamics(x = wing, scale = FALSE)
#'
#'@seealso \url{https://github.com/oleon12/BatWing/tree/master/README.Rmd}
#'
#'@author Leon-Alvarado Omar Daniel.
#'
#'@keyword Bat LSA, Bat Aspect Ratio, Bat Wing Loading
#'
#'@export


BatWingDynamics <- function(x,scale=FALSE,method=c("AR","WL","RWL","MPS")){
  
  if(scale==T){
    
    x[ , c(2:3)] <- x[,c(2,3)]/1000 ## Transform
    
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
  
  names(l) <- c("Dynamics","Stats")
  
  return(l)
  
}
