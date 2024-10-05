##########################################################################
## This R script is used for the simulation studies in the Appendix -- 
## results with W-NOMINATE.
## As before, we generate synthetic data with three different settings, 
## and then run the W-NOMINATE model on the data. We compare the true 
## ideal points with the estimated ideal points. 
## (Figure 9)
##########################################################################
## Instructions:
## 1. Part 1 is for fitting the model. Since this process will take time and 
##    involves random data generation, it is recommended to skip this part 
##    and load the saved results in Part 2.
## 2. Part 2 is for generating the figures.
##########################################################################

library(here)
setwd(here("codes", "simulation"))

##########################################################################
# Part 1 ------------------------------------------------------------------
##########################################################################
## Skip this part and load the saved results in Part 2

library(l1ideal)
library(tidyverse)
proj <- function(u, v) {
  return(as.vector( (u %*% v) / (v %*% v) ) * v)
}

# Scenario 1 --------------------------------------------------------------
# synthetic data
dat <- readRDS("../../data/simulation/sim1_data.rds") # See 01_simulation_master.R

# make roll call object
rc <- pscl::rollcall(dat$votes,
                     yea = 1, nay = 0, missing = 2, notInLegis = 3,
                     legis.names = dat$legis_data$name, legis.data = dat$legis_data,
                     vote.names = dat$votes_data$name, vote.data = dat$votes_data)
# anchors
ind <- rep(NA, 2)
ind[1] <- which.max(dat$legis_data$true_ideal_point_1d)
ind[2] <- which.max(dat$legis_data$true_ideal_point_2d)

# fit the model
set.seed(222)
wn <- wnominate::wnominate(rc, ubeta=15, uweights=0.5, dims=2, minvotes=20,
                               lop=0.025,trials=3, polarity=c(ind[1],ind[2]), verbose=T)
# save
save(dat, rc, wn, file = "../../data/simulation/sim1_wn.RData")

# Scenario 2 --------------------------------------------------------------
rm(list = ls())

# synthetic data
dat <- readRDS("../../data/simulation/sim2_data.rds") # See 01_simulation_master.R

# make roll call object
rc <- pscl::rollcall(dat$votes,
                     yea = 1, nay = 0, missing = 2, notInLegis = 3,
                     legis.names = dat$legis_data$name, legis.data = dat$legis_data,
                     vote.names = dat$votes_data$name, vote.data = dat$votes_data)
# anchors
ind <- rep(NA, 2)
ind[1] <- which.max(dat$legis_data$true_ideal_point_1d)
ind[2] <- which.max(dat$legis_data$true_ideal_point_2d)

# fit the model
set.seed(222)
wn <- wnominate::wnominate(rc, ubeta=15, uweights=0.5, dims=2, minvotes=20,
                           lop=0.025,trials=3, polarity=c(ind[1],ind[2]), verbose=T)
# save
save(dat, rc, wn, file = "../../data/simulation/sim2_wn.RData")


# Scenario 3 --------------------------------------------------------------
rm(list = ls())

# synthetic data
dat <- readRDS("../../data/simulation/sim3_data.rds") # See 01_simulation_master.R

# make roll call object
rc <- pscl::rollcall(dat$votes,
                     yea = 1, nay = 0, missing = 2, notInLegis = 3,
                     legis.names = dat$legis_data$name, legis.data = dat$legis_data,
                     vote.names = dat$votes_data$name, vote.data = dat$votes_data)
# anchors
ind <- rep(NA, 2)
ind[1] <- which.max(dat$legis_data$true_ideal_point_1d)
ind[2] <- which.max(dat$legis_data$true_ideal_point_2d)

# fit the model
set.seed(222)
wn <- wnominate::wnominate(rc, ubeta=15, uweights=0.5, dims=2, minvotes=20,
                           lop=0.025,trials=3, polarity=c(ind[1],ind[2]), verbose=T)
# save
save(dat, rc, wn, file = "../../data/simulation/sim3_wn.RData")


##########################################################################
# Part 2 ------------------------------------------------------------------
##########################################################################

# Plot --------------------------------------------------------------------
rm(list = ls())
source("helper.R")

## Simulation 1
res <- read_rds("../../data/simulation/sim1_res_summary.rds")
load("../../data/simulation/sim1_wn.RData")
p1 <- plot_legislator_chains(dat, res)

# make data frame for visualization
ideal_df <- dat$legis_data
ideal_df <- ideal_df[ideal_df$name %in% res$variable,]
# ideal point results
ideal_df$ideal_point_1d = wn$legislators$coord1D
ideal_df$ideal_point_2d = wn$legislators$coord2D

# W-NOMINATE
p1[[2]] = ggplot() + 
  geom_point(aes(x = ideal_point_1d, 
                 y = ideal_point_2d, 
                 col = group, shape = group), 
             size = 2, data = ideal_df) +
  theme_classic() +
  theme(legend.position="none") +
  scale_color_brewer("", type = "qual", palette = "Set1") +
  labs(x="Dimension 1",y="Dimension 2",title="W-NOMINATE")

# Comparing true and estimated ideal points
p1[[3]] = ggplot() +
  geom_point(aes(x = true_ideal_point_1d, y = 
                   ideal_point_1d, col = group, shape = group), 
             size = 2, data = ideal_df) +
  theme_classic() +
  theme(legend.position="none") +
  scale_color_brewer("", type = "qual", palette = "Set1") +
  labs(x="true",y="estimate",title="True v. Estimate (Dimension 1)")
p1[[4]] = ggplot() +
  geom_point(aes(x = true_ideal_point_2d, y = 
                   ideal_point_2d, col = group, shape = group), 
             size = 2, data = ideal_df) +
  theme_classic() +
  theme(legend.position="none") +
  scale_color_brewer("", type = "qual", palette = "Set1") +
  labs(x="true",y="estimate",title="True v. Estimate (Dimension 2)")

rm(list=setdiff(ls(), "p1"))
source("helper.R")

## Simulation 2
res <- read_rds("../../data/simulation/sim2_res_summary.rds")
load("../../data/simulation/sim2_wn.RData")
p2 <- plot_legislator_chains(dat, res)

# make data frame for visualization
ideal_df <- dat$legis_data
ideal_df <- ideal_df[ideal_df$name %in% res$variable,]
# ideal point results
ideal_df$ideal_point_1d = wn$legislators$coord1D
ideal_df$ideal_point_2d = wn$legislators$coord2D

# W-NOMINATE
p2[[2]] = ggplot() + 
  geom_point(aes(x = ideal_point_1d, 
                 y = ideal_point_2d, 
                 col = group, shape = group), 
             size = 2, data = ideal_df) +
  theme_classic() +
  theme(legend.position="none") +
  scale_color_brewer("", type = "qual", palette = "Set1") +
  labs(x="Dimension 1",y="Dimension 2",title="W-NOMINATE")

# Comparing true and estimated ideal points
p2[[3]] = ggplot() +
  geom_point(aes(x = true_ideal_point_1d, y = 
                   ideal_point_1d, col = group, shape = group), 
             size = 2, data = ideal_df) +
  theme_classic() +
  theme(legend.position="none") +
  scale_color_brewer("", type = "qual", palette = "Set1") +
  labs(x="true",y="estimate",title="True v. Estimate (Dimension 1)")
p2[[4]] = ggplot() +
  geom_point(aes(x = true_ideal_point_2d, y = 
                   ideal_point_2d, col = group, shape = group), 
             size = 2, data = ideal_df) +
  theme_classic() +
  theme(legend.position="none") +
  scale_color_brewer("", type = "qual", palette = "Set1") +
  labs(x="true",y="estimate",title="True v. Estimate (Dimension 2)")

rm(list=setdiff(ls(), c("p1", "p2")))
source("helper.R")

## Simulation 3
res <- read_rds("../../data/simulation/sim3_res_summary.rds")
load("../../data/simulation/sim3_wn.RData")
p3 <- plot_legislator_chains(dat, res)

# make data frame for visualization
ideal_df <- dat$legis_data
ideal_df <- ideal_df[ideal_df$name %in% res$variable,]
# ideal point results
ideal_df$ideal_point_1d = wn$legislators$coord1D
ideal_df$ideal_point_2d = wn$legislators$coord2D

# W-NOMINATE
p3[[2]] = ggplot() + 
  geom_point(aes(x = -ideal_point_1d, 
                 y = ideal_point_2d, 
                 col = group, shape = group), 
             size = 2, data = ideal_df) +
  theme_classic() +
  theme(legend.position="none") +
  scale_color_brewer("", type = "qual", palette = "Set1") +
  labs(x="Dimension 1",y="Dimension 2",title="W-NOMINATE")

# Comparing true and estimated ideal points
p3[[3]] = ggplot() +
  geom_point(aes(x = true_ideal_point_1d, y = 
                   -ideal_point_1d, col = group, shape = group), 
             size = 2, data = ideal_df) +
  theme_classic() +
  theme(legend.position="none") +
  scale_color_brewer("", type = "qual", palette = "Set1") +
  labs(x="true",y="estimate",title="True v. Estimate (Dimension 1)")
p3[[4]] = ggplot() +
  geom_point(aes(x = true_ideal_point_2d, y = 
                   ideal_point_2d, col = group, shape = group), 
             size = 2, data = ideal_df) +
  theme_classic() +
  theme(legend.position="none") +
  scale_color_brewer("", type = "qual", palette = "Set1") +
  labs(x="true",y="estimate",title="True v. Estimate (Dimension 2)")

## combine the plots
library(patchwork)
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

## Figure 9
pdf(file="../../figure/sim_synthetic_wn.pdf", width=18, height=14)
multiplot(p1s, p2s, p3s, cols = 1)
dev.off()

# ggsave("../../figure/sim_synthetic1_wn.pdf", p1s, width = 14, height = 4)
# ggsave("../../figure/sim_synthetic2_wn.pdf", p2s, width = 14, height = 4)
# ggsave("../../figure/sim_synthetic3_wn.pdf", p3s, width = 14, height = 4)

