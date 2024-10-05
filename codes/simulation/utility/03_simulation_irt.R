##########################################################################
## This R script is used for the simulation studies in Appendices L and M.
## It runs simulations with different data generating processes 
## (gaussian, gaussian_nonuniform, quadratic, quadratic_nonuniform) 
## using Bayesian IRT models.
## (Figures 13, 16, 19, and 22)
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
library(patchwork)
library(here)
setwd(here("codes", "simulation"))
source("helper.R")

##########################################################################
# Part 1 ------------------------------------------------------------------
##########################################################################
## Skip this part and load the saved results in Part 2

run_irt <- function(simulation_id, filename) {
    # read data
    dat <- read_rds(paste0("../../data/simulation/utility/sim", simulation_id, "_", filename, "_dat.rds"))
    # make rollcall object
    rc <- pscl::rollcall(dat$votes,
        yea = 1, nay = 0, missing = 2, notInLegis = 3,
        legis.names = dat$legis_data$name, legis.data = dat$legis_data,
        vote.names = dat$votes_data$name, vote.data = dat$votes_data
    )
    # fit the model
    set.seed(222)
    idl <- pscl::ideal(rc,
        dropList = list(lop = 0, legisMin = 10),
        priors = NULL, startvals = "eigen",
        d = 2, maxiter = 10000, thin = 10, burnin = 1000,
        impute = FALSE, normalize = FALSE,
        store.item = TRUE, file = NULL, verbose = TRUE
    )
    # save results
    save(dat, rc, idl, file = paste0("../../data/simulation/utility/sim", simulation_id, "_", filename, "_irt.RData"))
}

expand.grid(simulation_id = 1:3, filename = c("gaussian", "gaussian_nonuniform", "quadratic", "quadratic_nonuniform")) %>%
    pmap(run_irt)


##########################################################################
# Part 2 ------------------------------------------------------------------
##########################################################################

## gaussian nonuniform

p1 <- make_irt_figure(1, "gaussian_nonuniform", coord_flip = TRUE, sign_flip_1d = TRUE, sign_flip_2d = TRUE)
p2 <- make_irt_figure(2, "gaussian_nonuniform", coord_flip = FALSE, sign_flip_1d = FALSE, sign_flip_2d = TRUE)
p3 <- make_irt_figure(3, "gaussian_nonuniform", coord_flip = FALSE, sign_flip_1d = TRUE, sign_flip_2d = TRUE)

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

## Figure 19
ggsave("../../figure/sim_gaussian_nonuniform_synthetic1_irt.pdf", p1s, width = 14, height = 4)
ggsave("../../figure/sim_gaussian_nonuniform_synthetic2_irt.pdf", p2s, width = 14, height = 4)
ggsave("../../figure/sim_gaussian_nonuniform_synthetic3_irt.pdf", p3s, width = 14, height = 4)

## gaussian

p1 <- make_irt_figure(1, "gaussian", coord_flip = TRUE, sign_flip_1d = TRUE, sign_flip_2d = TRUE)
p2 <- make_irt_figure(2, "gaussian", coord_flip = FALSE, sign_flip_1d = TRUE, sign_flip_2d = FALSE)
p3 <- make_irt_figure(3, "gaussian", coord_flip = FALSE, sign_flip_1d = TRUE, sign_flip_2d = TRUE)

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

## Figure 13
ggsave("../../figure/sim_gaussian_synthetic1_irt.pdf", p1s, width = 14, height = 4)
ggsave("../../figure/sim_gaussian_synthetic2_irt.pdf", p2s, width = 14, height = 4)
ggsave("../../figure/sim_gaussian_synthetic3_irt.pdf", p3s, width = 14, height = 4)

## quadratic nonuniform

p1 <- make_irt_figure(1, "quadratic_nonuniform", coord_flip = TRUE, sign_flip_1d = TRUE, sign_flip_2d = TRUE)
p2 <- make_irt_figure(2, "quadratic_nonuniform", coord_flip = FALSE, sign_flip_1d = TRUE, sign_flip_2d = TRUE)
p3 <- make_irt_figure(3, "quadratic_nonuniform", coord_flip = FALSE, sign_flip_1d = TRUE, sign_flip_2d = TRUE)

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

## Figure 22
ggsave("../../figure/sim_quadratic_nonuniform_synthetic1_irt.pdf", p1s, width = 14, height = 4)
ggsave("../../figure/sim_quadratic_nonuniform_synthetic2_irt.pdf", p2s, width = 14, height = 4)
ggsave("../../figure/sim_quadratic_nonuniform_synthetic3_irt.pdf", p3s, width = 14, height = 4)

## quadratic

p1 <- make_irt_figure(1, "quadratic", coord_flip = TRUE, sign_flip_1d = TRUE, sign_flip_2d = TRUE)
p2 <- make_irt_figure(2, "quadratic", coord_flip = FALSE, sign_flip_1d = TRUE, sign_flip_2d = TRUE)
p3 <- make_irt_figure(3, "quadratic", coord_flip = FALSE, sign_flip_1d = TRUE, sign_flip_2d = TRUE)

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

## Figure 16
ggsave("../../figure/sim_quadratic_synthetic1_irt.pdf", p1s, width = 14, height = 4)
ggsave("../../figure/sim_quadratic_synthetic2_irt.pdf", p2s, width = 14, height = 4)
ggsave("../../figure/sim_quadratic_synthetic3_irt.pdf", p3s, width = 14, height = 4)
