# 
# This is what is going to run 
# 
# 
# 
.onLoad <- function(libname, pkgname){
  if ( is.null( getOption("spw.threads") ) ) { 
    options(spw.threads = 1)
  }
  
  packageStartupMessage("This is spatialwarnings ", 
                        packageDescription("spatialwarnings", 
                                           fields = "Version"), 
                        appendLF = TRUE)
  packageStartupMessage("Use options(spw.threads = <n>) to set up multi-core processing")
}


.onUnload <- function(libpath) { 
  options(spw.threads = NULL)
}
