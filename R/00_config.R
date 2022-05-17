install_missing_pkgs <- function(packages, bib_file = NULL) {
  new.packages <-
    packages[!(packages %in% utils::installed.packages()[, "Package"])]

  if (length(new.packages)) {
    install.packages(new.packages)
  }
  return(NULL)
}
