# 
# 
# Documentation for the serengeti dataset (objects serengeti and serengeti.rain)
# 
# See also R/forestgap.R for pointers on how to document a dataset. 
# 
#' @title Serengeti dataset
#' 
#' @description Vegetation data along a rainfall gradient in Serengeti national park. 
#' The data-set consists of a recatangular area of size 7.5 km x 90 km. This data is represented as a list of matrices.
#' Each matrix is a moving window of 7.5 km x 7.5 km which moves my 2.5 km. Each entry in the matrix is vegetation data classified into binary units with 0(grass) and 1(wood).
#' 
#' @details The resolution of vegetation data is 30m and that of rainfall data is 2.5km.    
#' 
#' @format A list of logical matrices 
#' 
#' @source Extraceted from Eby's et al (2017) supplementary material 
#'   \url{https://github.com/tee-lab/spacetime-csd/}
#' 
#' @references 
#' 
#' Eby, S., Agrawal, A., Majumder, S., Dobson, A.P. & Guttal, V. (2017). 
#' Alternative stable states and spatial indicators of critical slowing down 
#' along a spatial gradient in a savanna ecosystem: Global Ecology 
#' and Biogeography, 26, 638-649

#' Reed, D. N., Anderson, T. M., Dempewolf, J., Metzger, K., & Serneels, S. (2009). 
#' The spatial distribution of vegetation types in the Serengeti ecosystem: 
#' the influence of rainfall and topographic relief on vegetation patch characteristics. 
#' Journal of Biogeography, 36(4), 770-782.
# 
# The strings below have no effect except forcing R to read this file. 
"serengeti" 

#' @rdname serengeti
#' 
#' @format The annual rainfall corresponding to the matrices in the serengeti 
#'   dataset, in the corresponding order. 
#' 
"serengeti.rain"

