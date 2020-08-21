# 
# 
# Textual output options 
DIGITS <- 2

# This is going to run when the user loads the package
# 
.onAttach <- function(libname, pkgname){
  
  parallelism_message <- "Use plan(multiprocess) to set up parallel processing"
  
  package_ver <- utils::packageDescription("spatialwarnings", 
                                           fields = "Version")
  
  devel_message <- ""
  if ( grepl("99$", package_ver, perl = TRUE) ) { 
    devel_message <- " (development version, use at your own risk!)"
  }
  
  packageStartupMessage({ 
    paste0("This is spatialwarnings ", 
           package_ver, devel_message, 
           "\n", parallelism_message)
    }, appendLF = TRUE)
  
}



# A function returning a theme that gives visual homogenity to all the plots 
# produced by the spatialwarnings package. 
theme_spwarnings <- function() { 
  lightgray <- "#DCDAD5"
  lightgray_darker <- "#B6B5B1"
  lightgray_lighter <- "#F1EFE9"
  theme_bw() + 
    theme(panel.grid.major  = element_line(color = lightgray_darker, 
                                           linetype = 2, 
                                           size = .3), 
          panel.grid.minor  = element_blank(), 
          strip.background = element_rect(fill = NA, color = NA), 
          panel.border = element_rect(color = lightgray))
}

# Line color scale
linescale_spwarnings <- function() { 
  scale_color_manual(values = c('#EAD710', '#EA4510', '#6E10EA', '#10EA12'))
}

fillscale_spwarnings <- function(...) { 
#   scale_fill_manual(values = c('#F2C84A', "#F24A4A", "#734AF2", "#4AF24B"))
  scale_fill_manual(..., values = c("#F7DC8D", "#F78D8D", "#A78DF7","#8DF78E"))
}

