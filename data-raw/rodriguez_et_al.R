# 
# 

# Read binarized images from Rodriguez et al. (2017). The images are binarized
# using the graythresh() function from Octave, then exported as csvs containing 
# 1/0s 
picdir <- "./data-raw/rodriguez_etal"
pics <- dir(picdir, full = TRUE, pattern = "*.csv")

# Read the pictures 
imgs <- lapply(pics, function(f) { 
  a <- as.matrix(read.csv(f, header = FALSE)) > 0
  dimnames(a) <- NULL
  a
  })
names(imgs) <- gsub(".csv", "", basename(pics))

# Compute a test of indicators  
ics <- flowlength_sews(imgs) 
ics <- indictest(ics, nperm = 19)
plot(ics, along = names(psds))

# Store dataset now
arizona <- imgs
use_data(arizona, overwrite = TRUE, compress = TRUE)




