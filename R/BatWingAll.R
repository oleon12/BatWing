#'
#' @title LSA + Dynamics measures for Bat wings. 
#' 
#' @description Calculate the LSA and Dynamics measures for a Bat wing using different wings measures.
#' 
#' @return The function return four LSA calculations and four Dynamics measures. 
#'
#' @param x is the input matrix. This matrix is very specific, check the example.
#' 
#' @param scale if you want to transform milimeters and grames to meters and kilograms
#' for the  LSA and Dynamics measures calculate.
#' 
#' @return The function returns a S3 object with two list objects. First one
#' is the LSA and Dynamics measures, and second one is the Statistics.
#' 
#' @examples
#' library(BatWing)
#' data(wing)
#'
#' BatWingAll(x = wing, scale=FALSE)
#'
#'
#'@author Leon-Alvarado Omar Daniel.
#'
#'

BatWingAll <- function(x, scale=F){
  
  
  if(scale==T){
    
    Area.r <- BatWingArea(x,scale=T)
    
  }else{
    
    Area.r <- BatWingArea(x,scale=T)
    
  }
  
  out.list <- list()
  
  if(scale==F){
    
    warning("scale is FALSE. Remember, for the Dynamic formulas the LSA or Area must be in squared meters")
    
  }
  
  for (i in 1:length(colnames(Area.r))){
    
    r1 <- data.frame(x[,1],x[,6],x[,7],Area.r[,i])
    
    
    if(scale==T){
      
      Dyn.r <- BatWingDynamics(r1,scale=T)
      
      Dyn.r <- cbind(Area=Area.r[,i],Dyn.r)
      
      out.list[[i]] <- Dyn.r
      
    }else{
      
      Dyn.r <- BatWingDynamics(r1,scale=F)
      
      Dyn.r <- cbind(Area=Area.r[,i],Dyn.r)
      
      out.list[[i]] <- Dyn.r
    }
    
  }
  
  names(out.list) <- colnames(Area.r)
  
  return(out.list)
  
}



