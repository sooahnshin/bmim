##########################################################################
## Additional Analysis: US House of Representatives (52nd to 55th within a single model) 
## Fit BMIM for Dynamic Analysis (running the entire period)
## See Appendix P for details.
##########################################################################
## Instructions:
## The following code fits the BMIM model to the pre-processed data.
## Since this process involves running MCMC, it may take a while to complete.
## You may want to skip this code and use the results saved in the 
## data/congress/results folder in the next sections.
##########################################################################

library(tidyverse)
library(l1ideal)
library(future)
library(furrr)
plan(multisession)
library(here)
setwd(here("codes", "congress"))
source("funs.R")

## arguments
# args <- commandArgs(trailingOnly = TRUE)
args <- c(1, 50000, 10, 20000)
n_chain <- as.numeric(args[1])
mcmc_length <- as.numeric(args[2])
thin_length <- as.numeric(args[3])
burnin_length <- as.numeric(args[4])

southern <- c("MD", "DE", "VA", "WV", "KY", "NC", "SC", "TN", "GA", "AL", "FL", "MS", "LA", "AR", "MO", "OK", "TX")
load("../../data/congress/Rdata/H52_55.RData")

## rollcall
rc <- pscl::rollcall(H_votes,
                       yea = 1, nay = 0, missing = c(NA, 2), notInLegis = 3,
                       legis.names = H_legis$bioname, legis.data = H_legis,
                       vote.names = H_bills$congress_rollnumber, vote.data = H_bills)
filtered <- filter_votes(rc, 0, 20)
bills <- H_bills[filtered$bills,]
votes <- rc$votes[filtered$legis, filtered$bills]
legis <- H_legis[filtered$legis,] %>%
    mutate(group = ifelse(state_abbrev %in% southern & party_name == "Democrat", "Southern Democrat", party_name))

## run l1ideal
res <- l1ideal(rc, dimensions = 2, mcmc  = mcmc_length, thin = thin_length, burnin = burnin_length, 
                 minvotes = 20,
                 lop = 0, verbose = 1000, seed = 123)

saveRDS(votes, file = "../../data/congress/results/H52_55_votes.rds")
saveRDS(legis, file = "../../data/congress/results/H52_55_legis.rds")
saveRDS(bills, file = "../../data/congress/results/H52_55_bills.rds")
## since res is a large object, we save each component separately
saveRDS(res$legislators, file = "../../data/congress/results/H52_55_res_legislators.rds")
saveRDS(res$yea_positions, file = "../../data/congress/results/H52_55_res_yea_positions.rds")
saveRDS(res$nay_positions, file = "../../data/congress/results/H52_55_res_nay_positions.rds")
res$legislators <- res$yea_positions <- res$nay_positions <- NULL
saveRDS(res, file = "../../data/congress/results/H52_55_res.rds")