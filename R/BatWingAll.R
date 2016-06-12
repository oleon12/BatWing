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
#'@seealso \url{https://github.com/oleon12/BatWing/tree/master/README.Rmd}
#'
#'@author Leon-Alvarado Omar Daniel.
#'
#'@keyword Bat LSA, Bat Aspect Ratio, Bat Wing Loading
#'
#'@export


BatWingAll <- function(x, scale=F){
  
  
  if(scale==T){
    
    Area.r <- BatWingArea(x,scale=T)
    
  }else{
    
    Area.r <- BatWingArea(x,scale=T)
    
  }
  
  out.list <- list()
  
  stat.l <- list()
  
  if(scale==F){
    
    warning("scaleArea is FALSE, remember, for the Dynamic formulas the LSA or Area must be in squared meters")
    
  }
  
  for (i in 1:length(colnames(Area.r[[1]]))){
    
    r1 <- data.frame(x[,1],x[,6],x[,7],Area.r[[1]][,i])
    
    colnames(r1)[2:4] <- c("Mass","Wingspan","Area")
    
    if(scale==T){
      
      Dyn.r <- BatWingDynamics(r1,scale=T)
      
      stat.l[[i]] <- Dyn.r[[2]]
      
      Dyn.r <- cbind(Area=Area.r[[1]][,i],Dyn.r[[1]])
      
      out.list[[i]] <- Dyn.r
      
    }else{
      
      Dyn.r <- BatWingDynamics(r1,scale=F)
      
      stat.l[[i]] <- Dyn.r[[2]]
      
      Dyn.r <- cbind(Area=Area.r[[1]][,i],Dyn.r[[1]])
      
      out.list[[i]] <- Dyn.r
    }
    
  }
  
  names(out.list) <- c("BloodMcFarlane","Aldrige","SmithStarrett","Pirlot")
  
  names(stat.l) <-  c("BloodMcFarlane","Aldrige","SmithStarrett","Pirlot")
  
  out <- list(out.list,stat.l)
  names(out) <-c("Dynamics","Stats")
  
  return(out)
  
}

