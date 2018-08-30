# Option 1: ok ------------------------------------------------------------
# From SWATmodel package: it does not work
# OBS. Change the version$arch for R_ARCH since are diferentes
# Orig
# libarch = if (nzchar(version$arch)) paste("libs", version$arch, sep = "/") else "libs"
# dest <- file.path(R_PACKAGE_DIR, libarch)
# swats<- c("rswim2018.exe")
# for(filename in swats) {
#   dir.create(dest, recursive = TRUE, showWarnings = FALSE)
#   file.copy(filename, dest, overwrite = TRUE)
# }

# Modify: ok
libarch = if (nzchar(R_ARCH)) paste("libs", R_ARCH, sep = "/") else "libs"
dest <- file.path(R_PACKAGE_DIR, libarch)
swats<- c("rswim2018.exe")
for(filename in swats) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  file.copy(filename, dest, overwrite = TRUE)
}


# Option 2: ok ------------------------------------------------------------
#
# copy lib:
# libfile <- paste("rswim2018.exe", sep="")
# dest <- file.path(R_PACKAGE_DIR, paste("libs", R_ARCH, sep=""))
# dir.create(dest, recursive=TRUE, showWarnings=FALSE)
# file.copy(libfile, dest, overwrite=TRUE)


# Option 3: ok ------------------------------------------------------------
# https://rdrr.io/cran/BayesXsrc/src/src/install.libs.R

# mydebug <- TRUE
# if (mydebug) {
#   cat("R_ARCH=", R_ARCH,"\n")
#   cat("R_PACKAGE_DIR=", R_PACKAGE_DIR, "\n")
#   cat("R_PACKAGE_NAME=", R_PACKAGE_NAME, "\n")
#   cat("R_PACKAGE_SOURCE=", R_PACKAGE_SOURCE, "\n")
#   cat("SHLIB_EXT=", SHLIB_EXT, "\n")
#   cat("WINDOWS=", WINDOWS, "\n")
# }
# binary <- if(WINDOWS) "rswim2018.exe" else "rswim2018"
# if ( file.exists(binary) ) {
#   libarch <- if (nzchar(R_ARCH)) paste('libs', R_ARCH, sep='') else 'libs'
#   dest <- file.path(R_PACKAGE_DIR, libarch)
#   dir.create(dest, recursive = TRUE, showWarnings = FALSE)
#   file.copy(binary, dest, overwrite = TRUE)
#   file.remove(binary)
# }
