#!/usr/bin/R -f  
# This piece of code will test the package on main platforms 
#

# Clone then 
pkgdir <- tempdir()
git_address <- system(paste0("git clone https://github.com/spatial-ews/spatialwarnings ", 
                             pkgdir))

# archs <- c("debian-gcc-devel", "debian-gcc-patched", "debian-gcc-release", 
#            "fedora-clang-devel", "fedora-gcc-devel", "macos-elcapitan-release", 
#            "macos-mavericks-oldrel", "solaris-x86-patched", "ubuntu-gcc-devel", 
#            "ubuntu-gcc-release", "windows-x86_64-devel", 
#            "windows-x86_64-patched", "windows-x86_64-release")
archs <- c("solaris-x86-patched")

# Check on windows
devtools::check_win_devel(pkg = pkgdir)
devtools::check_win_release(pkg = pkgdir)
devtools::check_win_oldrelease(pkg = pkgdir)

# Check on Solaris
rhub::check(pkgdir, platform = "solaris-x86-patched")

# Check on linux 
rhub::check(pkgdir, platform = "debian-gcc-release")
rhub::check(pkgdir, platform = "debian-gcc-patched")
rhub::check(pkgdir, platform = "debian-gcc-devel")

# Results are sent by email 

# Cleanup
file.remove(dir(pkgdir, full.name = TRUE))
