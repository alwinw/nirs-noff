library(targets)
library(tarchetypes)

lapply(list.files("R", full.names = TRUE), source)

packages <- c("dplyr", "ggplot2", "haven", "gtsummary")

tar_option_set(
  packages = packages
)

list(
  tar_files(
    csv_paths, list.files("data", "[0-9]{1,2}_*.csv", full.names = TRUE)
  )
)
