#' @title Spatial Pattern measure: Fractal Geometry
#' @description MAIN function to calculate fractal geometries. Also calculates patch sizes (same as patch area).
#' @details Dependencies include FGcore.R , lbl.R (both included in the repository)

#' @param data is a list of binary matrices of length >=1 (OR) a single binary matrix
#' @return A list of lists (of area and perimeter values for each patch) in every matrix of input list (OR) a single list (of area and perimeter values for each patch in the input marix) if input is a single matrix
fracgeo<-function(data){
  if (sum(is.list(data))==0){
    return(FGcore(data))
  } 
  else{return(lapply(data,function(x){FGcore(x)}))
    }
}
