##########################################################################
## This R script is used for the simulation studies in Appendix I 
## for the large tuning parameter c. It runs the simulation with c = 8.
##########################################################################
## Data: Synthetic data
##########################################################################
## Instructions: Since this process will take time and involves random data 
##    generation, it is recommended to skip this R script and load the saved 
##    results in 03_simulation_results.R.
##########################################################################

## arguments
# args <- commandArgs(trailingOnly = TRUE)
args <- c(2, 50000, 10, 20000, 1)
n_chain <- as.numeric(args[1])
mcmc_length <- as.numeric(args[2])
thin_length <- as.numeric(args[3])
burnin_length <- as.numeric(args[4])
c_param <- 8

cat("n_chain:", n_chain, "\n",
    "mcmc_length:", mcmc_length, "\n",
    "thin_length:", thin_length, "\n",
    "burnin_length:", burnin_length, "\n",
    "window_size:", c_param, "\n")

install.packages("l1ideal_1.5.tar.gz", repos = NULL, type = "source")
if(packageVersion("l1ideal") != "1.5") {
  stop("Please install the correct version of l1ideal package")
  ## Version 1.5 is same as 1.2 except line 14 in l1ideal.c (#define SLICE_W 8)
}
library(l1ideal)
library(posterior)
library(coda)
library(purrr)
library(future)
library(furrr)
plan(multisession)
library(here)
setwd(here("codes", "simulation"))

set.seed(1)
seed_list <- sample(1:1000, n_chain)

dat <- generate.data(dimensions = 2, 
                     n1 = 100, 
                     n2 = 0, 
                     n3 = 0, 
                     n4 = 0,
                     m = 1000,
                     mu1 = c(0,0), 
                     sigma1 = 0.7, 
                     theta = 1, 
                     seed = 1)
rc <- pscl::rollcall(dat$votes,
                     yea = 1, nay = 0, missing = 2, notInLegis = 3,
                     legis.names = dat$legis_data$name, legis.data = dat$legis_data,
                     vote.names = dat$votes_data$name, vote.data = dat$votes_data)
## run l1ideal with n chains
res_c <- future_map(1:n_chain, ~l1ideal(rc, dimensions = 2, mcmc = mcmc_length, thin = thin_length,
                                           burnin = burnin_length, minvotes = 20, lop = 0,
                                           verbose = 100, seed = seed_list[[.x]]), .options = furrr_options(seed = NULL))
saveRDS(dat, "../../data/simulation/tuning_param/sim_c_large_dat.rds")
saveRDS(res_c, "../../data/simulation/tuning_param/sim_c_large.rds")

## return to the original package
install.packages("../../l1ideal_1.2.tar.gz", repos = NULL, type = "source")
