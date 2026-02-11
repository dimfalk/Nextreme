.onAttach <- function(libname, pkgname) {

  pkg <- "Nextreme"

  packageStartupMessage(utils::packageVersion(pkg))
}
