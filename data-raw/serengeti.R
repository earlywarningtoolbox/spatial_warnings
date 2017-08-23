# 
# 
# This file reads the raw data for the serengeti dataset, and convert it 
# to a form that is compatible with ditributing it within and R package. 
# 

options(mc.cores = 4)

datdir <- './data-raw/serengeti/'
files  <- dir(paste0(datdir, "matrices"), full = TRUE)

# Read matrices
matrices <- lapply(files, read.csv, sep = ",", header = FALSE)
names(matrices) <- files
matrices <- lapply(matrices, function(x) as.matrix(x)>0)

# Number of points to keep
minn <- 3 # 2
maxn <- length(matrices) - 4 # 11

matrices <- matrices[minn:maxn]

ics <- generic_spews(matrices, 
                     subsize = 5, 
                     detrend = FALSE, 
                     abs_skewness = FALSE, 
                     moranI_coarse_grain = TRUE)
ics.test <- indictest(ics)

# Read rain data 
rain <- read.csv(paste0(datdir, 'serengeti_rain.dat'), header = FALSE)
rain <- rain[minn:maxn, "V1"]

plot(ics.test, rain) + 
  geom_vline(xintercept = 590)

# Compute indicators for testing
# spcs <- spectral_spews(matrices, 
#                        sdr_low_range = c(0, .2), 
#                        sdr_high_range = c(.8, 1))
# spcs.test <- indictest(spcs)
# 
# plot(spcs, along = rain)
# plot(spcs.test, along = rain) + 
#   geom_vline(xintercept = 590, color = "red", linetype = "dashed") 


# Save datasets 
serengeti <- matrices 
names(serengeti) <- NULL 
serengeti.rain <- rain 

use_data(serengeti, overwrite = TRUE)
use_data(serengeti.rain, overwrite = TRUE)

# 
# # Do moran with z-score (does not change anything)
# moranz <- unlist( lapply(matrices, function(o) { 
#   o <- coarse_grain(o, subsize = 5)
#   o[] <- (o - mean(o)) / sd(o)
#   raw_moran(o)
# }) )
# 
# plot( subset(ics.test, indicator == "moran")[ ,"value"], moranz)
