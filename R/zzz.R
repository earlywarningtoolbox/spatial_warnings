# 
# This is what is going to run 
# 
# 
# 
.onAttach <- function(libname, pkgname){
  
  packageStartupMessage("This is spatialwarnings ", 
                        utils::packageDescription("spatialwarnings", 
                                                  fields = "Version"), 
                        appendLF = TRUE)

  if ( .Platform$OS.type == "unix" ) { 
    packageStartupMessage("Use options(mc.cores = <n>) to set up multi-core processing")
  }

}

