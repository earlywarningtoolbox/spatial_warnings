# 
# This is what is going to run on package attachment
# 
# 
# 
.onAttach <- function(libname, pkgname){
  
  packageStartupMessage({ 
    paste0("This is spatialwarnings ", 
           utils::packageDescription("spatialwarnings", fields = "Version"), 
           "\n", 
           "Use plan(multiprocess) to set up parallel processing")
    }, appendLF = TRUE)
  
}

