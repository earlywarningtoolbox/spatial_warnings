# 
# 
# First tests to include a kolmogorov complexity indicator
# 

library(acss)
library(stringr)
library(magrittr)
library(ggplot2)

# Open temporary directory
tmpdir <- tempdir()
setwd(tmpdir)

# Download and extract files
supfiles <- "https://ars.els-cdn.com/content/image/1-s2.0-S1476945X16300691-mmc1.zip"
download.file(supfiles, destfile = "supdata.zip")
unzip("supdata.zip")
unzip("./mmc1/supplementaryDataAndCode.zip")

# Irregular patterns 
# ------------------

# Load matrices 
irr_allmats <- dir("supplementaryDataAndCode/irregular-patterns/", 
                   full.names = TRUE)
# Read all matrices 
irr_mats <- lapply(irr_allmats, 
                   function(f) { 
                     a <- as.matrix(read.table(f))
                     attr(a, "dimnames") <- NULL
                     a
                    })

# Read parameter values 
irr_pars <- str_extract(irr_allmats, "homall[0-9]+.*.txt") %>% 
  sub('homall', '', .) %>% 
  sub('.txt', '', .) %>% 
  as.numeric()



# Compute generic indicators as a reference
irr_mats_bin <- lapply(irr_mats_bin, function(mat) mat > mean(mat))
irr_genindics <- generic_sews(irr_mats_bin, subsize = 4) %>% 
                   indictest(nulln = 199)
plot(irr_genindics, along = irr_pars) + 
  labs(x = 'Rainfall') # Stress increases as rainfall decreases




# Compute KBDM. This function returns a single value with a matrix 
# as input. 
get_kbdm <- function(mat, subsize) { 
  # Split matrix 
  xs <- seq(1, nrow(mat), by = subsize)
  ys <- seq(1, ncol(mat), by = subsize)
  allblockns <- expand.grid(seq.int(length(xs)-1), 
                            seq.int(length(ys)-1))
  all_substr <- Map(function(xblockn, yblockn) { 
                         as.vector(mat[xs[xblockn]:(xs[xblockn+1]-1), 
                                       ys[yblockn]:(ys[yblockn+1]-1)]) %>% 
                          as.integer() %>% 
                          paste(collapse = "")
                         }, 
                    allblockns[ ,1], allblockns[ ,2]) %>% unlist
  # Summarize the substrings 
  counts <- table(all_substr)
  counts <- data.frame(string = names(counts), 
                       multip = as.vector(counts), 
                       kctm = acss(names(counts), alphabet = 2)[ ,1])
  
  # Compute Kbdm 
  return( with(counts, sum(log2(multip) + kctm)) )
}

# Create the indicator function
indic_kbdm <- create_indicator(get_kbdm)

# Here, subsize cannot be above 3 so that individual blocks do not have 
# a length above 12 (which is the max string length that acss can handle)
# Somehow in Dakos 2016 they manage to use string lengths of 16 (4x4 
# submatrices) ? (or there is a misunderstanding ?)
all_kbdms <- indic_kbdm(irr_mats_bin, subsize = 3) %>% 
               indictest(nulln = 19)

plot(all_kbdms, along = irr_pars)
