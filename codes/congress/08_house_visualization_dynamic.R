##########################################################################
## Additional Analysis: US House of Representatives (52nd to 55th within a single model) 
## Pre-process Votes Data for Dynamic Analysis (running the entire period)
## See Appendix P for details (Figure 29).
##########################################################################
## Instructions: Run the following code to generate and save the figures.
##########################################################################

library(tidyverse)
library(l1ideal)
library(here)
setwd(here("codes", "congress"))
source("funs.R")

## read data
load("../../data/congress/results/H52_55.RData")
south <- c("MD", "DE", "VA", "WV", "KY", "NC", "SC", "TN", "GA", "AL", "FL", "MS", "LA", "AR", "MO", "TX")
west <- c("IA", "MN", "KS", "NE", "SD", "ND", "CO", "CA", "WY", "WA", "OR", "MT", "ID", "NV")
north <- unique(legis$state_abbrev[!legis$state_abbrev %in% c(south, west)])
group <- c("Southern Democrat", "Democrat", "Ind. Democrat", "Republican", "National Greenbacker", "Readjuster Democrat", "Independent",
          "Readjuster", "Ind. Republican", "Union", "Union Labor", "Populist", "Silver", "Silver Republican", "Progressive Republican",
          "Socialist", "Progressive", "Prohibitionist", "Farmer-Labor", "American Labor")
group.col <- c("#e36934", "#1405BD", "#635FBD", "#DE0100", "#006400", "#0004A1", "#ABABAB", 
              "#000382", "#DE7E78", "#7f3300", "#5550c8", "#62bd6d", "#777777", "#ff4444", "#912929", 
              "#cd3700", "#b63434", "#ff00ff", "#EAEB13", "#ff0000")

res <- postprocess.l1ideal(res)
l1object <- res
color.group <- legis$group
shape.group <- legis$group
dim <- length(l1object$legislators)
df <- summary.l1ideal(l1object)

## BMIM ideal points
ideal_df <- data.frame(ideal_point_1d = -df[df$parameter=="ideal_point_1d","mean"],
                       ideal_point_2d = df[df$parameter=="ideal_point_2d","mean"],
                       name = legis$bioname,
                       stringsAsFactors = F)
ideal_df$color.group <- color.group
ideal_df$shape.group <- shape.group
ideal_df <- ideal_df %>%
  mutate(state = legis$state_abbrev) %>%
  mutate(section = case_when(state %in% south ~ "S",
                             state %in% west ~ "W",
                             state %in% north ~ "N"))

## read legislators data 
load("../../data/congress/results/H52.RData")
legis52 <- legis
load("../../data/congress/results/H53.RData")
legis53 <- legis
load("../../data/congress/results/H54.RData")
legis54 <- legis
load("../../data/congress/results/H55.RData")
legis55 <- legis

ideal_df <- ideal_df |>
    mutate(H52 = if_else(name %in% legis52$bioname, 1, 0),
           H53 = if_else(name %in% legis53$bioname, 1, 0),
           H54 = if_else(name %in% legis54$bioname, 1, 0),
           H55 = if_else(name %in% legis55$bioname, 1, 0))

## legislators served in H52
l1.52 <- ggplot() +
  geom_point(aes(x = ideal_point_1d, y = ideal_point_2d, 
                 shape = section), col = "grey", 
             data = ideal_df |> filter(H52 == 0), 
             alpha = 0.3, size = 1.5) +
  geom_point(aes(x = ideal_point_1d, y = ideal_point_2d, 
                 shape = section, col = color.group), 
             data = ideal_df |> filter(H52 == 1), 
             alpha = 0.7, size = 1.5) +
  theme_classic() +
  labs(x="Dimension 1", y="Dimension 2",
       title="(a) The 52nd US H.R. (1891-1893)") +
  coord_equal(xlim = c(-3,3), ylim = c(-3,3)) +
  scale_shape_manual(name = "State", 
                     breaks = c("N", "S", "W"), 
                     values = c(19, 17, 15),
                     labels = c("Northeast", "South", "West")) +
  scale_color_manual(name = "Group", breaks = group, values = group.col) +
  theme(legend.position = "bottom")

## legislators served in H53
l1.53 <- ggplot() +
  geom_point(aes(x = ideal_point_1d, y = ideal_point_2d, 
                 shape = section), col = "grey", 
             data = ideal_df |> filter(H53 == 0), 
             alpha = 0.3, size = 1.5) +
  geom_point(aes(x = ideal_point_1d, y = ideal_point_2d, 
                 shape = section, col = color.group), 
             data = ideal_df |> filter(H53 == 1), 
             alpha = 0.7, size = 1.5) +
  theme_classic() +
  labs(x="Dimension 1", y="Dimension 2",
       title="(b) The 53rd US H.R. (1893-1895)") +
  coord_equal(xlim = c(-3,3), ylim = c(-3,3)) +
  scale_shape_manual(name = "State", 
                     breaks = c("N", "S", "W"), 
                     values = c(19, 17, 15),
                     labels = c("Northeast", "South", "West")) +
  scale_color_manual(name = "Group", breaks = group, values = group.col) +
  theme(legend.position = "bottom")

## legislators served in H54
l1.54 <- ggplot() +
  geom_point(aes(x = ideal_point_1d, y = ideal_point_2d, 
                 shape = section), col = "grey", 
             data = ideal_df |> filter(H54 == 0), 
             alpha = 0.3, size = 1.5) +
  geom_point(aes(x = ideal_point_1d, y = ideal_point_2d, 
                 shape = section, col = color.group), 
             data = ideal_df |> filter(H54 == 1), 
             alpha = 0.7, size = 1.5) +
  theme_classic() +
  labs(x="Dimension 1", y="Dimension 2",
       title="(c) The 54th US H.R. (1895-1897)") +
  coord_equal(xlim = c(-3,3), ylim = c(-3,3)) +
  scale_shape_manual(name = "State", 
                     breaks = c("N", "S", "W"), 
                     values = c(19, 17, 15),
                     labels = c("Northeast", "South", "West")) +
  scale_color_manual(name = "Group", breaks = group, values = group.col) +
  theme(legend.position = "bottom")

## legislators served in H55
l1.55 <- ggplot() +
  geom_point(aes(x = ideal_point_1d, y = ideal_point_2d, 
                 shape = section), col = "grey", 
             data = ideal_df |> filter(H55 == 0), 
             alpha = 0.3, size = 1.5) +
  geom_point(aes(x = ideal_point_1d, y = ideal_point_2d, 
                 shape = section, col = color.group), 
             data = ideal_df |> filter(H55 == 1), 
             alpha = 0.7, size = 1.5) +
  theme_classic() +
  labs(x="Dimension 1", y="Dimension 2",
       title="(d) The 55th US H.R. (1897-1899)") +
  coord_equal(xlim = c(-3,3), ylim = c(-3,3)) +
  scale_shape_manual(name = "State", 
                     breaks = c("N", "S", "W"), 
                     values = c(19, 17, 15),
                     labels = c("Northeast", "South", "West")) +
  scale_color_manual(name = "Group", breaks = group, values = group.col) +
  theme(legend.position = "bottom")

# layout
lay <- rbind(c(1,1,1,1,1,1,1,1,1,1),
             c(2,2,2,2,2,3,3,3,3,3),
             c(2,2,2,2,2,3,3,3,3,3),
             c(2,2,2,2,2,3,3,3,3,3),
             c(2,2,2,2,2,3,3,3,3,3),
             c(2,2,2,2,2,3,3,3,3,3),
             c(4,4,4,4,4,5,5,5,5,5),
             c(4,4,4,4,4,5,5,5,5,5),
             c(4,4,4,4,4,5,5,5,5,5),
             c(4,4,4,4,4,5,5,5,5,5),
             c(4,4,4,4,4,5,5,5,5,5),
             c(6,6,6,6,6,6,6,6,6,6))

# legend
leg <- ggplot() + 
  geom_point(aes(x = ideal_point_1d, y = ideal_point_2d, 
                 shape = section, col = color.group), 
             data = ideal_df, 
             alpha = 0.7, size = 1.5) +
  theme_classic() +
  labs(x="Dimension 1: Partisan", y="Dimension 2: Gold Standard",
       title="(a) L1 Norm Ideal Point Estimation") +
  coord_equal(xlim = c(-1,1), ylim = c(-1,1)) +
  # coord_equal(ratio = 1) +
  scale_shape_manual(name = "State", 
                     breaks = c("N", "S", "W"), 
                     values = c(19, 17, 15),
                     labels = c("Northeast", "South", "West")) +
  scale_color_manual(name = "Group", breaks = group, values = group.col) +
  theme(legend.position = "bottom")
# title
l1.title = ggplot() + 
  annotate("text", x = 1, y = 1, size=5, label = "L1 Norm Ideal Point Estimation") + 
  theme_void()
mylegend <- g_legend(leg)

## Figure 29
pdf(file="../../figure/l1ideal_H52_H55.pdf", width=9, height=10)
grid.arrange(l1.title,
             l1.52 + theme(legend.position = "none"), 
             l1.53 + theme(legend.position = "none"),
             l1.54 + theme(legend.position = "none"),
             l1.55 + theme(legend.position = "none"),
             mylegend, layout_matrix = lay)
dev.off()
