#' Library
#'
#' @param Packages character vector with ur packages to install or load
#' @importFrom utils install.packages installed.packages
<<<<<<< HEAD
#' @export
=======
>>>>>>> 523a6f5283cb384f5f933b9f1f680b558c16395d
Library <- function(Packages) {
  InstallAndLoad <- function(Package) {
    if (!Package %in% installed.packages()[, 1]) {install.packages(Package, repos="https://cran.rstudio.com/")}
    require(Package, character.only = TRUE)
  }
  invisible(sapply(Packages, InstallAndLoad))
}
