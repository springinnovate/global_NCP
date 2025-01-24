library(usethis)

# Create a new package
create_package("RestorationES")

# Add dependencies
use_package("terra")
use_package("sf")
use_package("dplyr")
use_package("ggplot2")
use_package("yaml")
use_package("tidyr")
use_package("purrr")

# Create core directories
use_r("data_prep")  # For data preparation functions
use_r("model")      # For ES modeling functions
use_r("visualize")  # For visualization functions
use_testthat()      # For unit testing



