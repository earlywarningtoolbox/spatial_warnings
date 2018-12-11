# 
# This piece of code will test the package on main platforms 
#

# Clone then 
pkgdir <- tempdir()
git_address <- system(paste0("git clone https://github.com/spatial-ews/spatialwarnings ", 
                             pkgdir))

archs <- c("debian-gcc-devel", "debian-gcc-patched", "debian-gcc-release", 
           "fedora-clang-devel", "fedora-gcc-devel", "macos-elcapitan-release", 
           "macos-mavericks-oldrel", "solaris-x86-patched", "ubuntu-gcc-devel", 
           "ubuntu-gcc-release", "ubuntu-rchk", "windows-x86_64-devel", 
           "windows-x86_64-patched", "windows-x86_64-release")

rhub::check(pkgdir, platform = archs)

