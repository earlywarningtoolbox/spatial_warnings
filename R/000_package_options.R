# 
# 
# Textual output options 
DIGITS <- 3

OPTIMWARNINGS <- FALSE

# A theme that gives visual homogenity to all the plots 
# produced by the spatialwarnings package. It is implemented as a function so 
# that some things are tweakable on the fly
# 
theme_spwarnings <- function() { 
  lightgray <- "#DCDAD5"
  lightgray_darker <- "#B6B5B1"
  lightgray_lighter <- "#F1EFE9"
  theme_bw() + 
    theme(panel.grid.major  = element_line(color = lightgray_darker, 
                                             linetype = 2), 
          panel.grid.minor  = element_blank(), 
          strip.background = element_rect(fill = lightgray, color = lightgray), 
          panel.border = element_rect(color = lightgray)
          ) 
}
    
# Line color scale
linescale_spwarnings <- function() { 
  scale_color_manual(values = c('#EAD710', '#EA4510', '#6E10EA', '#10EA12'))
}

fillscale_spwarnings <- function(...) { 
#   scale_fill_manual(values = c('#F2C84A', "#F24A4A", "#734AF2", "#4AF24B"))
  scale_fill_manual(..., values = c("#F7DC8D", "#F78D8D", "#A78DF7","#8DF78E"))
}

