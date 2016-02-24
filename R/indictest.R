# 
# 
# Indictest is a method for testing the significance of spatial indicators. 
# 
# This file only defines the global method and maybe some helpers. For each 
#   method, please refer to the task_* files. 

# Define global method
#'@export
indictest <- function(obj, null_replicates = 999, ...) { 
  UseMethod('indictest')
}

