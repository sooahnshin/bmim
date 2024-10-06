##########################################################################
## This R script is used for the simulation studies in the paper -- results with BMIM.
##########################################################################
## First, it runs the following R scripts: 01_simulation1.R, 01_simulation2.R, and 01_simulation3.R.
## Then, it reports the results using the helper functions in the helper.R script.
## Primarily, it generates Figure 6 in the paper.
## Additionally, it generates Figures 4, 6 (first panel), and 8 in the Appendix.
##########################################################################
## Data: Synthetic data (generated using the `l1ideal::generate.data` function).
##########################################################################
## Instructions:
## 1. Part 1 is for fitting the model. Since this process will take time and 
##    involves random data generation, it is recommended to skip this part 
##    and load the saved results in Part 2.
## 2. Part 2 is for generating the figures.
##########################################################################

##########################################################################
# Part 1 ------------------------------------------------------------------
##########################################################################
## Skip this part and load the saved results in Part 2

## Simulation 1: (a) non-partisan
source("01_simulation1.R")

## Simulation 2: (b) two-party
source("01_simulation2.R")

## Simulation 3: (c) multi-party
source("01_simulation3.R")

##########################################################################
# Part 2 ------------------------------------------------------------------
##########################################################################

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
## Simulation 1: non-partisan
##########
source("helper.R")
res_list <- list()
for(i in 1:4) {
  res_list[[i]] <- read_rds(paste0("../../data/simulation/sim1_res", i, ".rds"))
  res_list[[i]]$legislators <- read_rds(paste0("../../data/simulation/sim1_res", i, "_legislators.rds"))
  res_list[[i]]$yea_positions <- read_rds(paste0("../../data/simulation/sim1_res", i, "_yea_positions.rds"))
  res_list[[i]]$nay_positions <- read_rds(paste0("../../data/simulation/sim1_res", i, "_nay_positions.rds"))
}
n_chain <- length(res_list)
dat <- read_rds("../../data/simulation/sim1_data.rds")

## check whether the coordinates are flipped
p_ls <- map(1:n_chain, function(m) {
  p <- plot.simulation(dat, res_list[[m]], coord_flip = TRUE)
  return(p[[3]] + p[[4]])
})
p_ls[[1]] / p_ls[[2]] / p_ls[[3]] / p_ls[[4]]

## fix the coordinates
coord_flip_ls <- c(FALSE, TRUE, TRUE, TRUE)
p_ls <- map(1:n_chain, function(m) {
  p <- plot.simulation(dat, res_list[[m]], coord_flip = coord_flip_ls[m])
  return(p[[3]] + p[[4]])
})
## check whether the coordinates are correctly flipped
p_ls[[1]] / p_ls[[2]] / p_ls[[3]] / p_ls[[4]]

## it takes a while to run this code (to compute the convergence diagnostics)
## you may want to skip this line and load the saved results in the next line
res <- get_summary_chains(res_list, coord_flip_ls, rep(TRUE, 4), rep(TRUE, 4))
saveRDS(res, file = "../../data/simulation/sim1_res_summary.rds")
res <- read_rds("../../data/simulation/sim1_res_summary.rds")

p_res <- plot_legislator_chains(dat, res)
p_res[[1]] + p_res[[2]] + p_res[[3]] + p_res[[4]]
p_res_combined <- ((p_res[[1]] + theme(aspect.ratio=1)) + 
          (p_res[[2]] + theme(aspect.ratio=1)) + 
          (p_res[[3]] + theme(aspect.ratio=1)) + 
          (p_res[[4]] + theme(aspect.ratio=1))) + 
  plot_layout(ncol = 4) +
  plot_annotation(title = "(a) Non-partisan")
## Figure 6
ggsave("../../figure/sim_synthetic1.pdf", p_res_combined, width = 14, height = 4)
## Figure 6 in the Appendix
ggsave("../../figure/sim_mid_c.pdf", p_res_combined + plot_annotation(title = "Moderate window size (c = 4)"), width = 14, height = 4)

## running time
for(i in 1:n_chain) {
  cat("Chain ", i, " Running time: ", convert_seconds_to_hours(res_list[[i]]$running_time), "\n")
}
# Chain  1  Running time:  3.257 hours 
# Chain  2  Running time:  3.24 hours 
# Chain  3  Running time:  3.234 hours 
# Chain  4  Running time:  3.272 hours 

## rhat
rhat_p <- ggplot(res, aes(x = factor(dimension), y = rhat, fill = type)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.75)) +
  labs(x = "Dimension", y = expression(hat(R)), title = NULL) +
  theme_minimal() +
  ylim(0.999, 1.1) +
  geom_hline(yintercept = 1.1, linetype = "dashed", color = "red") +
  scale_fill_brewer(name = "Type", labels = c("Ideal Points", "Nay Positions", "Yea Positions"), palette = "Paired")
## Figure 4 in the Appendix
ggsave("../../figure/sim_synthetic1_rhat.pdf", rhat_p, width = 8, height = 4)

##########
## Simulation 2: two-party
##########
rm(list = ls())
source("helper.R")
res_list <- list()
for(i in 1:4) {
  res_list[[i]] <- read_rds(paste0("../../data/simulation/sim2_res", i, ".rds"))
  res_list[[i]]$legislators <- read_rds(paste0("../../data/simulation/sim2_res", i, "_legislators.rds"))
  res_list[[i]]$yea_positions <- read_rds(paste0("../../data/simulation/sim2_res", i, "_yea_positions.rds"))
  res_list[[i]]$nay_positions <- read_rds(paste0("../../data/simulation/sim2_res", i, "_nay_positions.rds"))
}
n_chain <- length(res_list)
dat <- read_rds("../../data/simulation/sim2_data.rds")

## check whether the coordinates are flipped
p_ls <- map(1:n_chain, function(m) {
  p <- plot.simulation(dat, res_list[[m]], coord_flip = TRUE)
  return(p[[3]] + p[[4]])
})
p_ls[[1]] / p_ls[[2]] / p_ls[[3]] / p_ls[[4]]

## fix the coordinates
coord_flip_ls <- c(FALSE, FALSE, TRUE, TRUE)
p_ls <- map(1:n_chain, function(m) {
  p <- plot.simulation(dat, res_list[[m]], coord_flip = coord_flip_ls[m])
  return(p[[3]] + p[[4]])
})
## check whether the coordinates are correctly flipped
p_ls[[1]] / p_ls[[2]] / p_ls[[3]] / p_ls[[4]]

## plot the chains
p_ls <- map(1:n_chain, function(m) {
  p <- plot.simulation(dat, res_list[[m]], coord_flip = FALSE)
  return(p[[2]] + scale_color_brewer("", type = "qual", palette = "Set1") + ggtitle(paste("Chain", m)))
})
p_chains <- wrap_plots(p_ls, ncol = 2)
## Figure 8 in the Appendix
ggsave("../../figure/sim_synthetic2_chains.pdf", p_chains, width = 7, height = 7)

## it takes a while to run this code (to compute the convergence diagnostics)
## you may want to skip this line and load the saved results in the next line
res <- get_summary_chains(res_list, coord_flip_ls, rep(TRUE, 4), rep(FALSE, 4))
saveRDS(res, file = "../../data/simulation/sim2_res_summary.rds")
res <- read_rds("../../data/simulation/sim2_res_summary.rds")

p_res <- plot_legislator_chains(dat, res)
p_res[[1]] + p_res[[2]] + p_res[[3]] + p_res[[4]]
p_res_combined <- ((p_res[[1]] + theme(aspect.ratio=1)) + 
          (p_res[[2]] + theme(aspect.ratio=1)) + 
          (p_res[[3]] + theme(aspect.ratio=1)) + 
          (p_res[[4]] + theme(aspect.ratio=1))) + 
  plot_layout(ncol = 4) +
  plot_annotation(title = "(b) Two-party")
## Figure 6
ggsave("../../figure/sim_synthetic2.pdf", p_res_combined, width = 14, height = 4)

## running time
for(i in 1:n_chain) {
  cat("Chain ", i, " Running time: ", convert_seconds_to_hours(res_list[[i]]$running_time), "\n")
}
# Chain  1  Running time:  3.32 hours 
# Chain  2  Running time:  3.283 hours 
# Chain  3  Running time:  3.303 hours 
# Chain  4  Running time:  3.286 hours 

## rhat
rhat_p <- ggplot(res, aes(x = factor(dimension), y = rhat, fill = type)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.75)) +
  labs(x = "Dimension", y = expression(hat(R)), title = NULL) +
  theme_minimal() +
  ylim(0.999, 1.1) +
  geom_hline(yintercept = 1.1, linetype = "dashed", color = "red") +
  scale_fill_brewer(name = "Type", labels = c("Ideal Points", "Nay Positions", "Yea Positions"), palette = "Paired")
## Figure 4 in the Appendix
ggsave("../../figure/sim_synthetic2_rhat.pdf", rhat_p, width = 8, height = 4)

##########
## Simulation 3: multi-party
##########
rm(list = ls())
source("helper.R")
res_list <- list()
for(i in 1:4) {
  res_list[[i]] <- read_rds(paste0("../../data/simulation/sim3_res", i, ".rds"))
  res_list[[i]]$legislators <- read_rds(paste0("../../data/simulation/sim3_res", i, "_legislators.rds"))
  res_list[[i]]$yea_positions <- read_rds(paste0("../../data/simulation/sim3_res", i, "_yea_positions.rds"))
  res_list[[i]]$nay_positions <- read_rds(paste0("../../data/simulation/sim3_res", i, "_nay_positions.rds"))
}
n_chain <- length(res_list)
dat <- read_rds("../../data/simulation/sim3_data.rds")

## check whether the coordinates are flipped
p_ls <- map(1:n_chain, function(m) {
  p <- plot.simulation(dat, res_list[[m]], coord_flip = TRUE)
  return(p[[3]] + p[[4]])
})
p_ls[[1]] / p_ls[[2]] / p_ls[[3]] / p_ls[[4]]

## fix the coordinates
coord_flip_ls <- c(TRUE, FALSE, FALSE, TRUE)
p_ls <- map(1:n_chain, function(m) {
  p <- plot.simulation(dat, res_list[[m]], coord_flip = coord_flip_ls[m])
  return(p[[3]] + p[[4]])
})
## check whether the coordinates are correctly flipped
p_ls[[1]] / p_ls[[2]] / p_ls[[3]] / p_ls[[4]]

## plot the chains
p_ls <- map(1:n_chain, function(m) {
  p <- plot.simulation(dat, res_list[[m]], coord_flip = FALSE)
  return(p[[2]] + scale_color_brewer("", type = "qual", palette = "Set1") + ggtitle(paste("Chain", m)))
})
p_chains <- wrap_plots(p_ls, ncol = 2)

## it takes a while to run this code (to compute the convergence diagnostics)
## you may want to skip this line and load the saved results in the next line
res <- get_summary_chains(res_list, coord_flip_ls, rep(FALSE, 4), rep(FALSE, 4))
saveRDS(res, file = "../../data/simulation/sim3_res_summary.rds")
res <- read_rds("../../data/simulation/sim3_res_summary.rds")

p_res <- plot_legislator_chains(dat, res)
p_res[[1]] + p_res[[2]] + p_res[[3]] + p_res[[4]]
p_res_combined <- ((p_res[[1]] + theme(aspect.ratio=1)) + 
          (p_res[[2]] + theme(aspect.ratio=1)) + 
          (p_res[[3]] + theme(aspect.ratio=1)) + 
          (p_res[[4]] + theme(aspect.ratio=1))) + 
  plot_layout(ncol = 4) +
  plot_annotation(title = "(c) Multi-party")
## Figure 6
ggsave("../../figure/sim_synthetic3.pdf", p_res_combined, width = 14, height = 4)

## running time
for(i in 1:n_chain) {
  cat("Chain ", i, " Running time: ", convert_seconds_to_hours(res_list[[i]]$running_time), "\n")
}
# Chain  1  Running time:  3.213 hours 
# Chain  2  Running time:  3.112 hours 
# Chain  3  Running time:  3.207 hours 
# Chain  4  Running time:  3.151 hours 

## rhat
rhat_p <- ggplot(res, aes(x = factor(dimension), y = rhat, fill = type)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.75)) +
  labs(x = "Dimension", y = expression(hat(R)), title = NULL) +
  theme_minimal() +
  ylim(0.999, 1.1) +
  geom_hline(yintercept = 1.1, linetype = "dashed", color = "red") +
  scale_fill_brewer(name = "Type", labels = c("Ideal Points", "Nay Positions", "Yea Positions"), palette = "Paired")
## Figure 4 in the Appendix
ggsave("../../figure/sim_synthetic3_rhat.pdf", rhat_p, width = 8, height = 4)
