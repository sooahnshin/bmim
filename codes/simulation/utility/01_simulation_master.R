##########################################################################
## This R script is used for the simulation studies in Appendices L and M.
## It runs the following R scripts: 01_simulation*_*.R, where the first * 
## represents the simulation number (1-3) and the second * represents the 
## utility function used in the simulation. The utility functions are: 
## gaussian and quadratic. For the non-uniform yea/nay positions used in 
## Appendix M, the R scripts are named 01_simulation*_nonuniform.R.
## (Figures 11, 14, 17, and 20)
##########################################################################
## Data: Synthetic data (generated using the `generate.data.ext` function 
## in `codes/simulation/helper.R`)
##########################################################################
## Instructions:
## 1. Part 1 is for fitting the model. Since this process will take time and 
##    involves random data generation, it is recommended to skip this part 
##    and load the saved results in Part 2.
## 2. Part 2 is for generating the figures.
##########################################################################

# Create folder for saving the results
library(here)
setwd(here("codes", "simulation"))

##########################################################################
# Part 1 ------------------------------------------------------------------
##########################################################################
## Skip this part and load the saved results in Part 2

## In total, there are 3 x 2 x 2 = 12 simulations.
## You may want to consider running these R scripts in parallel to save time.

## Figure 11
source("01_simulation1_gaussian.R")
source("01_simulation2_gaussian.R")
source("01_simulation3_gaussian.R")

## Figure 14
source("01_simulation1_quadratic.R")
source("01_simulation2_quadratic.R")
source("01_simulation3_quadratic.R")

## Figure 17
source("01_simulation1_gaussian_nonuniform.R")
source("01_simulation2_gaussian_nonuniform.R")
source("01_simulation3_gaussian_nonuniform.R")

## Figure 20
source("01_simulation1_quadratic_nonuniform.R")
source("01_simulation2_quadratic_nonuniform.R")
source("01_simulation3_quadratic_nonuniform.R")

##########################################################################
# Part 2 ------------------------------------------------------------------
##########################################################################

## Figure 11
source("01_simulation_results_gaussian.R")

## Figure 14
source("01_simulation_results_quadratic.R")

## Figure 17
source("01_simulation_results_gaussian_nonuniform.R")

## Figure 20
source("01_simulation_results_quadratic_nonuniform.R")
