##CLEAR ALL
rm(list = ls(all = TRUE)) 

## install required packages
# Package names
packages <- c("tidyverse", "xtable", "stargazer", "gsynth", "purrr")

# load packages
invisible(lapply(packages, library, character.only = TRUE))