# 
# This is what is going to run 
# 
# 
# 
.onAttach <- function(libname, pkgname){
  if ( is.null( getOption("spw.threads") ) ) { 
    options(spw.threads = 1)
  }
  
  packageStartupMessage("This is spatialwarnings ", 
                        utils::packageDescription("spatialwarnings", 
                                                  fields = "Version"), 
                        appendLF = TRUE)
  if ( .Platform$OS.type == "unix" ) { 
    packageStartupMessage("Use options(mc.cores = <n>) to set up multi-core processing")
  }
}


.onUnload <- function(libpath) { 
  options(spw.threads = NULL)
}

