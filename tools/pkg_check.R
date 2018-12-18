#!/usr/bin/R -f  
# This piece of code will test the package on main platforms 
#

# Clone then 
pkgdir <- tempdir()
git_address <- system(paste0("git clone https://github.com/spatial-ews/spatialwarnings ", 
                             pkgdir))

# Check on windows
# devtools::check_win_devel(pkg = pkgdir)
# devtools::check_win_release(pkg = pkgdir)
# devtools::check_win_oldrelease(pkg = pkgdir)

# Check on Solaris
rhub::check(pkgdir, platform = "solaris-x86-patched")

# Check on linux 
rhub::check_for_cran(pkgdir)

# Results are sent by email 

# Cleanup
file.remove(dir(pkgdir, full.name = TRUE))
