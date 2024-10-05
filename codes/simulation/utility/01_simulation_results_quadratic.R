## See 01_simulation_master.R for the full script
library(l1ideal)
library(posterior)
library(coda)
library(tidyverse)
library(patchwork)
library(here)
setwd(here("codes", "simulation"))

#  n_chain: 4 
#  mcmc_length: 50000 
#  thin_length: 10 
#  burnin_length: 20000 

##########
## Simulation 1
##########
source("helper.R")
dat <- read_rds("../../data/simulation/utility/sim1_quadratic_dat.rds")
res_list <- read_rds("../../data/simulation/utility/sim1_quadratic_res_list.rds")
n_chain <- length(res_list)

## check whether the coordinates are flipped
p_ls <- map(1:n_chain, function(m) {
  p <- plot.simulation(dat, res_list[[m]], coord_flip = TRUE)
  return(p[[3]] + p[[4]])
})
p_ls[[1]] / p_ls[[2]] / p_ls[[3]] / p_ls[[4]]

## fix the coordinates
coord_flip_ls <- c(FALSE, FALSE, TRUE, TRUE)
p_ls <- plot_multichain(n_chain, dat, res_list, coord_flip_ls, rep(TRUE, 4), rep(TRUE, 4))
p_chains <- wrap_plots(p_ls, ncol = 2)

# it takes a while to run this code (to compute the convergence diagnostics)
res <- get_summary_chains(res_list, coord_flip_ls, rep(TRUE, 4), rep(TRUE, 4))
saveRDS(res, file = "../../data/simulation/utility/sim1_quadratic_res_summary.rds")

p_res <- plot_legislator_chains(dat, res)
p_res[[1]] + p_res[[2]] + p_res[[3]] + p_res[[4]]
p_res_combined <- ((p_res[[1]] + theme(aspect.ratio=1)) + 
          (p_res[[2]] + theme(aspect.ratio=1)) + 
          (p_res[[3]] + theme(aspect.ratio=1)) + 
          (p_res[[4]] + theme(aspect.ratio=1))) + 
  plot_layout(ncol = 4) +
  plot_annotation(title = "(a) Non-partisan")
ggsave("../../figure/sim_quadratic_synthetic1.pdf", p_res_combined, width = 14, height = 4)

## running time
for(i in 1:n_chain) {
  cat("Chain ", i, " Running time: ", convert_seconds_to_hours(res_list[[i]]$running_time), "\n")
}
# Chain  1  Running time:  3.241 hours 
# Chain  2  Running time:  3.234 hours 
# Chain  3  Running time:  3.217 hours 
# Chain  4  Running time:  3.26 hours 


##########
## Simulation 2
##########
rm(list = ls())
source("helper.R")
dat <- read_rds("../../data/simulation/utility/sim2_quadratic_dat.rds")
res_list <- read_rds("../../data/simulation/utility/sim2_quadratic_res_list.rds")
n_chain <- length(res_list)

## check whether the coordinates are flipped
p_ls <- map(1:n_chain, function(m) {
  p <- plot.simulation(dat, res_list[[m]], coord_flip = TRUE)
  return(p[[3]] + p[[4]])
})
p_ls[[1]] / p_ls[[2]] / p_ls[[3]] / p_ls[[4]]

## fix the coordinates
coord_flip_ls <- c(TRUE, TRUE, TRUE, TRUE)
p_ls <- plot_multichain(n_chain, dat, res_list, coord_flip_ls)
p_chains <- wrap_plots(p_ls, ncol = 2)

# it takes a while to run this code (to compute the convergence diagnostics)
res <- get_summary_chains(res_list, coord_flip_ls, rep(FALSE, 4), rep(FALSE, 4))
saveRDS(res, file = "../../data/simulation/utility/sim2_quadratic_res_summary.rds")

p_res <- plot_legislator_chains(dat, res)
p_res[[1]] + p_res[[2]] + p_res[[3]] + p_res[[4]]
p_res_combined <- ((p_res[[1]] + theme(aspect.ratio=1)) + 
          (p_res[[2]] + theme(aspect.ratio=1)) + 
          (p_res[[3]] + theme(aspect.ratio=1)) + 
          (p_res[[4]] + theme(aspect.ratio=1))) + 
  plot_layout(ncol = 4) +
  plot_annotation(title = "(b) Two-party")
ggsave("../../figure/sim_quadratic_synthetic2.pdf", p_res_combined, width = 14, height = 4)

## running time
for(i in 1:n_chain) {
  cat("Chain ", i, " Running time: ", convert_seconds_to_hours(res_list[[i]]$running_time), "\n")
}
# Chain  1  Running time:  3.293 hours 
# Chain  2  Running time:  3.276 hours 
# Chain  3  Running time:  3.278 hours 
# Chain  4  Running time:  3.311 hours 


##########
## Simulation 3
##########
rm(list = ls())
source("helper.R")
dat <- read_rds("../../data/simulation/utility/sim3_quadratic_dat.rds")
res_list <- read_rds("../../data/simulation/utility/sim3_quadratic_res_list.rds")
n_chain <- length(res_list)

## check whether the coordinates are flipped
p_ls <- map(1:n_chain, function(m) {
  p <- plot.simulation(dat, res_list[[m]], coord_flip = TRUE)
  return(p[[1]] + p[[2]] + p[[3]] + p[[4]])
})
p_ls[[1]] / p_ls[[2]] / p_ls[[3]] / p_ls[[4]]

## fix the coordinates
coord_flip_ls <- c(FALSE, FALSE, TRUE, TRUE)
p_ls <- plot_multichain(n_chain, dat, res_list, coord_flip_ls)
p_chains <- wrap_plots(p_ls, ncol = 2)

# it takes a while to run this code (to compute the convergence diagnostics)
res <- get_summary_chains(res_list, coord_flip_ls, rep(FALSE, 4), rep(FALSE, 4))
saveRDS(res, file = "../../data/simulation/utility/sim3_quadratic_res_summary.rds")

p_res <- plot_legislator_chains(dat, res)
p_res[[1]] + p_res[[2]] + p_res[[3]] + p_res[[4]]
p_res_combined <- ((p_res[[1]] + theme(aspect.ratio=1)) + 
          (p_res[[2]] + theme(aspect.ratio=1)) + 
          (p_res[[3]] + theme(aspect.ratio=1)) + 
          (p_res[[4]] + theme(aspect.ratio=1))) + 
  plot_layout(ncol = 4) +
  plot_annotation(title = "(c) Multi-party")
ggsave("../../figure/sim_quadratic_synthetic3.pdf", p_res_combined, width = 14, height = 4)

## running time
for(i in 1:n_chain) {
  cat("Chain ", i, " Running time: ", convert_seconds_to_hours(res_list[[i]]$running_time), "\n")
}
# Chain  1  Running time:  3.282 hours 
# Chain  2  Running time:  3.296 hours 
# Chain  3  Running time:  3.307 hours 
# Chain  4  Running time:  3.318 hours 

