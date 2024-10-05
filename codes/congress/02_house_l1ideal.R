##########################################################################
## Main Analysis: US House of Representatives (52nd to 55th) 
## Fit BMIM
##########################################################################
## Instructions:
## The following code fits the BMIM model to the pre-processed data.
## Since this code involves running MCMC, it may take a while to run.
## You may want to skip this code and use the results saved in the 
## data/congress/results folder in the next parts.
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
args <- c(1, 100000, 10, 50000)
n_chain <- as.numeric(args[1])
mcmc_length <- as.numeric(args[2])
thin_length <- as.numeric(args[3])
burnin_length <- as.numeric(args[4])

house_num <- c("52", "53", "54", "55")
southern <- c("MD", "DE", "VA", "WV", "KY", "NC", "SC", "TN", "GA", "AL", "FL", "MS", "LA", "AR", "MO", "OK", "TX")

## function for running l1ideal
run_l1ideal <- function(i, mcmc = 50000, thin = 10, burnin = 20000){
  load(paste0("../../data/congress/Rdata/H", house_num[i], ".RData"))
  ## make it as rollcall object
  rc <- pscl::rollcall(H_votes,
                       yea = 1, nay = 0, missing = c(NA, 2), notInLegis = 3,
                       legis.names = H_legis$bioname, legis.data = H_legis,
                       vote.names = H_bills$rollnumber, vote.data = H_bills)
  ## filter votes: drop unanimous votes, and save legislators voted minimum 20 rollcalls
  filtered <- filter_votes(rc, lop = 0, minvotes = 20)
  bills <- H_bills[filtered$bills,]
  votes <- rc$votes[filtered$legis, filtered$bills]
  legis <- H_legis[filtered$legis,] %>%
    ## group based on party label & southern democrat
    mutate(group = ifelse(state_abbrev %in% southern & party_name == "Democrat", "Southern Democrat", party_name))
  ## run l1ideal
  res <- l1ideal(rc, dimensions = 2, mcmc  = mcmc, thin = thin, burnin = burnin, 
                 minvotes = 20,
                 lop = 0, verbose = 1000, seed = 123)
  ## save
  save(votes, legis, bills, res,
       file = paste0("../../data/congress/results/H", house_num[i], ".RData"))
}

## run l1ideal for each congress
future_map(1:length(house_num), run_l1ideal, mcmc = mcmc_length, thin = thin_length, burnin = burnin_length)

