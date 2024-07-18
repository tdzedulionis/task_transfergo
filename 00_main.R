# R @4.1.2 is used

rm(list = ls())

# For reproducability, renv is used.
# renv::activate()
# renv::restore()

# Load libraries ----------------------------------------------------------

packages <- c('tidyverse',
              'lares',
              'mgcv',
              'data.table',
              'MASS',
              'broom')

lapply(packages, function(x){
  if(!requireNamespace(x, quietly = TRUE)) install.packages(x); 
  suppressPackageStartupMessages(library(x, character.only = T))}
)

# Source ------------------------------------------------------------------

# functions
source("000_functions.R")

# set constants
source("01_set_constants.R")

# Input
source("02_input.R")

# Exploratory data analysis and ANOVA tests
source("03_eda_tests.R")

# Modelling without splitting by user activity groups
source("04_modelling_whole.R")

# Modelling with splitting by user activity groups
source("05_modelling_groups.R")
