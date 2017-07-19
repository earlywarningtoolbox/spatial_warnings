# 
# This file will download and prepare the Serengeti data. See also: 
#   https://github.com/tee-lab/spacetime-csd
# 

# Download and prepare data
serengeti.dat <- read.csv("https://raw.githubusercontent.com/tee-lab/spacetime-csd/master/tran5_veg_30m.csv")
serengeti.dat <- as.matrix(serengeti.dat)
dimnames(serengeti.dat) <- NULL

# We set -9999 to one
for (j in seq.int(ncol(serengeti.dat))) { 
  for (i in seq.int(nrow(serengeti.dat))) { 
    if ( abs(serengeti.dat[i,j] - -9999) < .01 ) { 
      serengeti.dat[i,j] <- rbinom(1, 1, .5) # random to 1 or 0
    }
  }
}

# lattice::levelplot(serengeti.dat)

# Download and prepare rain data
raindat <- read.csv('https://raw.githubusercontent.com/tee-lab/spacetime-csd/master/tran5_rain_2500m.csv')
raindat <- as.matrix(raindat)
dimnames(raindat) <- NULL

# Interpolate rain data 
rain_interp <- sapply(seq.int(ncol(raindat)), function(col) { 
  approx(x = seq(1, nrow(serengeti.dat), length.out = nrow(raindat)), 
                      y = raindat[ ,col], # this is the mean (?)
                      xout = seq(1, nrow(serengeti.dat)))$y
  })


# Now splice up the data
serengeti <- list()
serengeti.rain <- numeric(0)
for (n in seq(0, floor(nrow(serengeti.dat)/250)-1, by = 0.5)) { 
# for (perc in seq(0, 1, by = .1)) { 
#   print(n)
#     rainlvl <- mean(raindat[ (1+perc*nrow(rainlvl)):(perc*nrow(rainlvl)), ])
    nrow_min <- (1+80+n*250)
    nrow_max <- (80+(n+1)*250)
    rain_min <- floor( nrow_min / nrow(serengeti.dat) * nrow(raindat)) 
    rain_max <- floor( nrow_max / nrow(serengeti.dat) * nrow(raindat)) 
    rainlvl <- mean(raindat[rain_min:rain_max, ])
    if (rainlvl <= 730 && rainlvl >= 591) { 
      serengeti.rain <- append(serengeti.rain, rainlvl)
      serengeti <- c(serengeti, list(serengeti.dat[nrow_min:nrow_max, 1:250] > 0))
    }
}

# library(spatialwarnings)

plop <- indictest( generic_spews(serengeti, subsize = 5, 
                                 # Sabiha's code uses coarse graining
                                 moranI_coarse_grain = FALSE) )

plot(plop)
plot(plop, along = serengeti.rain)


# Recompute moran's values with z-score
serengeti.moran <- unlist( lapply(serengeti, function(m) { 
    m <- coarse_grain(m, 5)
    m <- (m - mean(m)) / sd(m)
    raw_moran(m)
  }) )

plopdf <- as.data.frame(plop)
plopdf[with(plopdf, indicator == "moran"), "value"] <- serengeti.moran

ggplot(plopdf) + 
  geom_point(aes(x = rep(serengeti.rain, 4), y = value)) + 
  facet_grid( indicator ~ ., scales = "free_y")

plot(plop)

# Save datasets
use_data(serengeti)
use_data(serengeti.rain)

