#
# lpSolveS4 package startup file
#

r_profile <- path.expand("~/.Rprofile")
if(file.exists(r_profile)){
  source(r_profile)
  #cat("\n")
  #cat("Default ~/.Rprofile sourced\n")
}

# LibPath <- c("/Users/tshott/Library/R/3.3")
# .libPaths(LibPath)

# Make a utility function
required_packages <- c("lpSolveAPI", "devtools", "testthat")
install_packages  <- NULL

for(i in 1:length(required_packages)){
  if (length(find.package(required_packages[i], quiet=TRUE)) == 0){
    install_packages <- c(install_packages, required_packages[i])
  }
}

if(length(install_packages) > 0){
  cat("\n")
  cat("WARNING: Some packages required to test and build are not installed\n")
  cat("         Need to install: ", paste(install_packages, collapse=", "), "\n")
  cat("         Use RStudio Menu Tools->Install Packages...\n\n")
  cat("         OR - paste following command into Console\n\n")

  string <- paste(sprintf('"%s"', c(install_packages)), collapse = ", ")
  cat("         install.packages( c(",string ,"), verbose=TRUE) \n\n")
}

library(devtools)

# If repo not set - setup
if (is.null(getOption("repos"))){
  options(repos="http://cran.rstudio.com")
}

#cat("\n")
#cat(".Rprofile sourced\n")


