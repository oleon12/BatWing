

BatWingAll <- function(x, scaleArea=F, scaleDynamics=F){
  
  
  if(scaleArea==T){
    
    Area.r <- BatWingArea(x,scale=T)
    
  }else{
    
    Area.r <- BatWingArea(x,scale=T)
    
  }
  
  out.list <- list()
  
  if(scaleArea==F){
    
    warning("scaleArea is FALSE, remember, for the Dynamic formulas the LSA or Area must be in squared meters")
    
  }
  
  for (i in 1:length(colnames(Area.r))){
    
    r1 <- data.frame(x[,1],x[,6],x[,7],Area.r[,i])
    
    
    if(scaleDynamics==T){
      
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



