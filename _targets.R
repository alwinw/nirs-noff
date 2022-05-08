library(targets)
library(tarchetypes)

lapply(list.files(here::here("R"), pattern = ".R$", full.names = TRUE), source)

packages <- c("dplyr", "tidyr", "ggplot2", "RColorBrewer", "haven", "gtsummary", "here")

tar_option_set(
  packages = packages
)

list(
  tar_files(
    csv_paths, list.files("data", "[0-9]{1,2}_*.csv", full.names = TRUE)
  )
)
