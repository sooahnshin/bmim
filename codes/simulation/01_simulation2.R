## See 01_simulation_master.R for the full script
## arguments
# args <- commandArgs(trailingOnly = TRUE)
args <- c(4, 50000, 10, 20000)
n_chain <- as.numeric(args[1])
mcmc_length <- as.numeric(args[2])
thin_length <- as.numeric(args[3])
burnin_length <- as.numeric(args[4])

cat("n_chain:", n_chain, "\n",
    "mcmc_length:", mcmc_length, "\n",
    "thin_length:", thin_length, "\n",
    "burnin_length:", burnin_length, "\n")

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

### Simulation 2
dat <- generate.data(dimensions = 2, 
                     n1 = 50, 
                     n2 = 50, 
                     n3 = 0, 
                     n4 = 0,
                     m = 1000,
                     mu1 = c(0.7,0.7), 
                     mu2 = c(-0.7,-0.7), 
                     sigma1 = 0.5, 
                     sigma2 = 0.5, 
                     theta = 1, 
                     seed = 2)
rc <- pscl::rollcall(dat$votes,
                     yea = 1, nay = 0, missing = 2, notInLegis = 3,
                     legis.names = dat$legis_data$name, legis.data = dat$legis_data,
                     vote.names = dat$votes_data$name, vote.data = dat$votes_data)
## run l1ideal with n chains
res_list <- future_map(1:n_chain, ~l1ideal(rc, dimensions = 2, mcmc = mcmc_length, thin = thin_length,
                                           burnin = burnin_length, minvotes = 20, lop = 0,
                                           verbose = 100, seed = seed_list[[.x]]))

saveRDS(res_list, "../../data/simulation/sim2_res_list.rds")
saveRDS(dat, file = "../../data/simulation/sim2_data.rds")