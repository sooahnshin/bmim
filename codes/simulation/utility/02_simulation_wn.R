##########################################################################
## This R script is used for the simulation studies in Appendices L and M.
## It runs simulations with different data generating processes 
## (gaussian, gaussian_nonuniform, quadratic, quadratic_nonuniform) 
## using W-NOMINATE.
## (Figures 12, 15, 18, and 21)
##########################################################################
## Data: Synthetic data (generated using the `generate.data.ext` function 
## in `codes/simulation/helper.R`).
## See 01_simulation*.R for how each dataset is generated.
##########################################################################
## Instructions:
## 1. Part 1 is for fitting the model. Since this process will take time and 
##    involves random data generation, it is recommended to skip this part 
##    and load the saved results in Part 2.
## 2. Part 2 is for generating the figures.
##########################################################################

library(l1ideal)
library(tidyverse)
setwd(here("codes", "simulation"))
source("helper.R")

##########################################################################
# Part 1 ------------------------------------------------------------------
##########################################################################
## Skip this part and load the saved results in Part 2

run_wn <- function(simulation_id, filename) {
    # read data
    dat <- read_rds(paste0("../../data/simulation/utility/sim", simulation_id, "_", filename, "_dat.rds"))
    # make rollcall object
    rc <- pscl::rollcall(dat$votes,
        yea = 1, nay = 0, missing = 2, notInLegis = 3,
        legis.names = dat$legis_data$name, legis.data = dat$legis_data,
        vote.names = dat$votes_data$name, vote.data = dat$votes_data
    )
    # anchors
    ind <- rep(NA, 2)
    ind[1] <- which.max(dat$legis_data$true_ideal_point_1d)
    ind[2] <- which.max(dat$legis_data$true_ideal_point_2d)
    # fit the model
    set.seed(222)
    wn <- wnominate::wnominate(rc, ubeta=15, uweights=0.5, dims=2, minvotes=20,
                           lop=0.025,trials=3, polarity=c(ind[1],ind[2]), verbose=T)
    # save results
    save(dat, rc, wn, file = paste0("../../data/simulation/utility/sim", simulation_id, "_", filename, "_wn.RData"))
}

expand.grid(simulation_id = 1:3, filename = c("gaussian", "gaussian_nonuniform", "quadratic", "quadratic_nonuniform")) %>%
    pmap(run_wn)


##########################################################################
# Part 2 ------------------------------------------------------------------
##########################################################################

## gaussian nonuniform

p1 <- make_wn_figure(1, "gaussian_nonuniform", coord_flip = TRUE, sign_flip_1d = TRUE, sign_flip_2d = FALSE)
p2 <- make_wn_figure(2, "gaussian_nonuniform", coord_flip = FALSE, sign_flip_1d = FALSE, sign_flip_2d = FALSE)
p3 <- make_wn_figure(3, "gaussian_nonuniform", coord_flip = FALSE, sign_flip_1d = FALSE, sign_flip_2d = FALSE)

p1s <- ((p1[[1]] + theme(aspect.ratio=1)) + 
          (p1[[2]] + theme(aspect.ratio=1)) + 
          (p1[[3]] + theme(aspect.ratio=1)) + 
          (p1[[4]] + theme(aspect.ratio=1))) + 
  plot_layout(ncol = 4) +
  plot_annotation(title = "(a) Non-partisan")
p2s <- ((p2[[1]] + theme(aspect.ratio=1)) + 
          (p2[[2]] + theme(aspect.ratio=1)) + 
          (p2[[3]] + theme(aspect.ratio=1)) + 
          (p2[[4]] + theme(aspect.ratio=1))) + 
  plot_layout(ncol = 4) +
  plot_annotation(title = "(b) Two-party")
p3s <- ((p3[[1]] + theme(aspect.ratio=1)) + 
          (p3[[2]] + theme(aspect.ratio=1)) + 
          (p3[[3]] + theme(aspect.ratio=1)) + 
          (p3[[4]] + theme(aspect.ratio=1))) + 
  plot_layout(ncol = 4) +
  plot_annotation(title = "(c) Multi-party")

## Figure 18
ggsave("../../figure/sim_gaussian_nonuniform_synthetic1_wn.pdf", p1s, width = 14, height = 4)
ggsave("../../figure/sim_gaussian_nonuniform_synthetic2_wn.pdf", p2s, width = 14, height = 4)
ggsave("../../figure/sim_gaussian_nonuniform_synthetic3_wn.pdf", p3s, width = 14, height = 4)

## gaussian

p1 <- make_wn_figure(1, "gaussian", coord_flip = TRUE, sign_flip_1d = TRUE, sign_flip_2d = FALSE)
p2 <- make_wn_figure(2, "gaussian", coord_flip = FALSE, sign_flip_1d = FALSE, sign_flip_2d = FALSE)
p3 <- make_wn_figure(3, "gaussian", coord_flip = FALSE, sign_flip_1d = FALSE, sign_flip_2d = FALSE)

p1s <- ((p1[[1]] + theme(aspect.ratio=1)) + 
          (p1[[2]] + theme(aspect.ratio=1)) + 
          (p1[[3]] + theme(aspect.ratio=1)) + 
          (p1[[4]] + theme(aspect.ratio=1))) + 
  plot_layout(ncol = 4) +
  plot_annotation(title = "(a) Non-partisan")
p2s <- ((p2[[1]] + theme(aspect.ratio=1)) + 
          (p2[[2]] + theme(aspect.ratio=1)) + 
          (p2[[3]] + theme(aspect.ratio=1)) + 
          (p2[[4]] + theme(aspect.ratio=1))) + 
  plot_layout(ncol = 4) +
  plot_annotation(title = "(b) Two-party")
p3s <- ((p3[[1]] + theme(aspect.ratio=1)) + 
          (p3[[2]] + theme(aspect.ratio=1)) + 
          (p3[[3]] + theme(aspect.ratio=1)) + 
          (p3[[4]] + theme(aspect.ratio=1))) + 
  plot_layout(ncol = 4) +
  plot_annotation(title = "(c) Multi-party")

## Figure 12
ggsave("../../figure/sim_gaussian_synthetic1_wn.pdf", p1s, width = 14, height = 4)
ggsave("../../figure/sim_gaussian_synthetic2_wn.pdf", p2s, width = 14, height = 4)
ggsave("../../figure/sim_gaussian_synthetic3_wn.pdf", p3s, width = 14, height = 4)

## quadratic nonuniform

p1 <- make_wn_figure(1, "quadratic_nonuniform", coord_flip = TRUE, sign_flip_1d = TRUE, sign_flip_2d = FALSE)
p2 <- make_wn_figure(2, "quadratic_nonuniform", coord_flip = FALSE, sign_flip_1d = FALSE, sign_flip_2d = FALSE)
p3 <- make_wn_figure(3, "quadratic_nonuniform", coord_flip = FALSE, sign_flip_1d = FALSE, sign_flip_2d = FALSE)

p1s <- ((p1[[1]] + theme(aspect.ratio=1)) + 
          (p1[[2]] + theme(aspect.ratio=1)) + 
          (p1[[3]] + theme(aspect.ratio=1)) + 
          (p1[[4]] + theme(aspect.ratio=1))) + 
  plot_layout(ncol = 4) +
  plot_annotation(title = "(a) Non-partisan")
p2s <- ((p2[[1]] + theme(aspect.ratio=1)) + 
          (p2[[2]] + theme(aspect.ratio=1)) + 
          (p2[[3]] + theme(aspect.ratio=1)) + 
          (p2[[4]] + theme(aspect.ratio=1))) + 
  plot_layout(ncol = 4) +
  plot_annotation(title = "(b) Two-party")
p3s <- ((p3[[1]] + theme(aspect.ratio=1)) + 
          (p3[[2]] + theme(aspect.ratio=1)) + 
          (p3[[3]] + theme(aspect.ratio=1)) + 
          (p3[[4]] + theme(aspect.ratio=1))) + 
  plot_layout(ncol = 4) +
  plot_annotation(title = "(c) Multi-party")

## Figure 21
ggsave("../../figure/sim_quadratic_nonuniform_synthetic1_wn.pdf", p1s, width = 14, height = 4)
ggsave("../../figure/sim_quadratic_nonuniform_synthetic2_wn.pdf", p2s, width = 14, height = 4)
ggsave("../../figure/sim_quadratic_nonuniform_synthetic3_wn.pdf", p3s, width = 14, height = 4)

## quadratic

p1 <- make_wn_figure(1, "quadratic", coord_flip = TRUE, sign_flip_1d = TRUE, sign_flip_2d = FALSE)
p2 <- make_wn_figure(2, "quadratic", coord_flip = FALSE, sign_flip_1d = FALSE, sign_flip_2d = FALSE)
p3 <- make_wn_figure(3, "quadratic", coord_flip = FALSE, sign_flip_1d = TRUE, sign_flip_2d = FALSE)

p1s <- ((p1[[1]] + theme(aspect.ratio=1)) + 
          (p1[[2]] + theme(aspect.ratio=1)) + 
          (p1[[3]] + theme(aspect.ratio=1)) + 
          (p1[[4]] + theme(aspect.ratio=1))) + 
  plot_layout(ncol = 4) +
  plot_annotation(title = "(a) Non-partisan")
p2s <- ((p2[[1]] + theme(aspect.ratio=1)) + 
          (p2[[2]] + theme(aspect.ratio=1)) + 
          (p2[[3]] + theme(aspect.ratio=1)) + 
          (p2[[4]] + theme(aspect.ratio=1))) + 
  plot_layout(ncol = 4) +
  plot_annotation(title = "(b) Two-party")
p3s <- ((p3[[1]] + theme(aspect.ratio=1)) + 
          (p3[[2]] + theme(aspect.ratio=1)) + 
          (p3[[3]] + theme(aspect.ratio=1)) + 
          (p3[[4]] + theme(aspect.ratio=1))) + 
  plot_layout(ncol = 4) +
  plot_annotation(title = "(c) Multi-party")

## Figure 15
ggsave("../../figure/sim_quadratic_synthetic1_wn.pdf", p1s, width = 14, height = 4)
ggsave("../../figure/sim_quadratic_synthetic2_wn.pdf", p2s, width = 14, height = 4)
ggsave("../../figure/sim_quadratic_synthetic3_wn.pdf", p3s, width = 14, height = 4)