##########################################################################
## This R script is used to visualize the illustrative example of the 
## cutting line in the proposed method (Figure 1 of Appendix F) 
## and in the Bayesian IRT model (Figure 2 of Appendix G).
##########################################################################
## This is purely a visualization script that uses a stylized example 
## and does not involve any data analysis.
##########################################################################

library(tidyverse)
library(RColorBrewer)
library(patchwork)

theme_set(theme_void(base_size = 15) + theme(plot.title = element_text(hjust = 0.5), legend.position = "none"))
colors_set <- c("#F9B3B3", "#BDD4F3")

## Figure 1
dat <- data.frame(
    label = c("N", "Y"),
    x = c(-1, -1),
    y = c(-0.8, 1.2)
)

## case (a) horizontal cutting line
p1 <- ggplot(dat, aes(x, y)) +
    geom_rect(aes(xmin = -3, xmax = 3, ymin = sum(y)/2, ymax = 3), fill = colors_set[2]) +
    geom_rect(aes(xmin = -3, xmax = 3, ymin = -3, ymax = sum(y)/2), fill = colors_set[1]) +
    geom_point(aes(color = label), size = 5) +
    geom_text(aes(label = label, color = label), nudge_x = 0.2, nudge_y = 0.2, size = 10) +
    scale_color_brewer(palette = "Set1") +
    geom_segment(aes(x = -3, y = sum(y)/2, xend = 3, yend = sum(y)/2), color = "grey50", linewidth = 3, lty = "dashed") +
    geom_segment(aes(x = -3, y = 0, xend = 3, yend = 0), linewidth = 1.5, arrow = arrow(length = unit(0.3, "cm"), ends = "both", type = "closed")) +
    geom_segment(aes(x = 0, y = -3, xend = 0, yend = 3), linewidth = 1.5, arrow = arrow(length = unit(0.3, "cm"), ends = "both", type = "closed")) +
    coord_fixed()

dat <- data.frame(
    label = c("N", "Y"),
    x = c(0.6, -1),
    y = c(1.2, 1.2)
)

## case (b) vertical cutting line
p2 <- ggplot(dat, aes(x, y)) +
    geom_rect(aes(xmin = -3, xmax = sum(x)/2, ymin = -3, ymax = 3), fill = colors_set[2]) +
    geom_rect(aes(xmin = sum(x)/2, xmax = 3, ymin = -3, ymax = 3), fill = colors_set[1]) +
    geom_point(aes(color = label), size = 5) +
    geom_text(aes(label = label, color = label), nudge_x = 0.2, nudge_y = 0.2, size = 10) +
    scale_color_brewer(palette = "Set1") +
    geom_segment(aes(x = sum(x)/2, y = -3, xend = sum(x)/2, yend = 3), color = "grey50", linewidth = 3, lty = "dashed") +
    geom_segment(aes(x = -3, y = 0, xend = 3, yend = 0), linewidth = 1.5, arrow = arrow(length = unit(0.3, "cm"), ends = "both", type = "closed")) +
    geom_segment(aes(x = 0, y = -3, xend = 0, yend = 3), linewidth = 1.5, arrow = arrow(length = unit(0.3, "cm"), ends = "both", type = "closed")) +
    coord_fixed()

dat <- data.frame(
    label = c("N", "Y"),
    x = c(1.6, -1),
    y = c(-0.8, 1.2)
)

temp <- (abs(diff(dat$x)) + abs(diff(dat$y)))/2

shape_y <- data.frame(
  x = c(-3, dat$x[2] + temp, dat$x[2] + temp, dat$x[1] - temp, dat$x[1] - temp, -3),
  y = c(3, 3, dat$y[2], dat$y[1], -3, -3)
)

shape_n <- data.frame(
  x = c(dat$x[2] + temp, dat$x[2] + temp, dat$x[1] - temp, dat$x[1] - temp, 3, 3),
  y = c(3, dat$y[2], dat$y[1], -3, -3, 3)
)

## case (c) mix of straight lines 1
p3 <- ggplot(dat, aes(x, y)) +
  geom_polygon(data = shape_n, aes(x, y), fill = colors_set[1]) +
  geom_polygon(data = shape_y, aes(x, y), fill = colors_set[2]) +
  geom_point(aes(color = label), size = 5) +
    geom_text(aes(label = label, color = label), nudge_x = 0.2, nudge_y = 0.2, size = 10) +
    scale_color_brewer(palette = "Set1") +
    geom_segment(aes(x = x[2] + temp, y = y[2], xend = x[2] + temp, yend = 3), color = "grey50", linewidth = 3, lty = "dashed") +
    geom_segment(aes(x = x[1] - temp, y = -3, xend = x[1] - temp, yend = y[1]), color = "grey50", linewidth = 3, lty = "dashed") +
    geom_segment(aes(x = x[1] - temp, y = y[1], xend = x[2] + temp, yend = y[2]), color = "grey50", linewidth = 3, lty = "dashed") +
    geom_segment(aes(x = -3, y = 0, xend = 3, yend = 0), linewidth = 1.5, arrow = arrow(length = unit(0.3, "cm"), ends = "both", type = "closed")) +
    geom_segment(aes(x = 0, y = -3, xend = 0, yend = 3), linewidth = 1.5, arrow = arrow(length = unit(0.3, "cm"), ends = "both", type = "closed")) +
    coord_fixed()

dat <- data.frame(
    label = c("N", "Y"),
    x = c(1.2, -0.8),
    y = c(-1, 1.6)
)

temp <- (abs(diff(dat$x)) + abs(diff(dat$y)))/2

shape_y <- data.frame(
  x = c(-3, -3, dat$x[2], dat$x[1], 3, 3),
  y = c(3, dat$y[2] - temp, dat$y[2] - temp, dat$y[1] + temp, dat$y[1] + temp, 3)
)

shape_n <- data.frame(
  x = c(-3, dat$x[2], dat$x[1], 3, 3, -3),
  y = c(dat$y[2] - temp, dat$y[2] - temp, dat$y[1] + temp, dat$y[1] + temp, -3, -3)
)

## case (d) mix of straight lines 2
p4 <- ggplot(dat, aes(x, y)) +
  geom_polygon(data = shape_n, aes(x, y), fill = colors_set[1]) +
  geom_polygon(data = shape_y, aes(x, y), fill = colors_set[2]) +
  geom_point(aes(color = label), size = 5) +
    geom_text(aes(label = label, color = label), nudge_x = 0.2, nudge_y = 0.2, size = 10) +
    scale_color_brewer(palette = "Set1") +
    geom_segment(aes(x = x[1], y = y[1] + temp, xend = 3, yend = y[1] + temp), color = "grey50", linewidth = 3, lty = "dashed") +
    geom_segment(aes(x = x[2], y = y[2] - temp, xend = -3, yend = y[2] - temp), color = "grey50", linewidth = 3, lty = "dashed") +
    geom_segment(aes(x = x[2], y = y[2] - temp, xend = x[1], yend = y[1] + temp), color = "grey50", linewidth = 3, lty = "dashed") +
    geom_segment(aes(x = -3, y = 0, xend = 3, yend = 0), linewidth = 1.5, arrow = arrow(length = unit(0.3, "cm"), ends = "both", type = "closed")) +
    geom_segment(aes(x = 0, y = -3, xend = 0, yend = 3), linewidth = 1.5, arrow = arrow(length = unit(0.3, "cm"), ends = "both", type = "closed")) +
    coord_fixed()

dat <- data.frame(
    label = c("N", "Y"),
    x = c(1.2, -1.2),
    y = c(-1.2, 1.2)
)

shape_y <- data.frame(
  x = c(-3, -3, dat$x[2], dat$x[1], dat$x[1]),
  y = c(3, dat$y[1], dat$y[1], dat$y[2], 3)
)

sshape_y <- data.frame(
  x = c(3, dat$x[2], dat$x[2], dat$x[1], 3),
  y = c(-3, -3, dat$y[1], dat$y[2], dat$y[2])
)

## case (d) mix of straight lines 3
p5 <- ggplot(dat, aes(x, y)) +
  geom_polygon(data = shape_n, aes(x, y), fill = colors_set[1]) +
  geom_polygon(data = shape_y, aes(x, y), fill = colors_set[2]) +
  geom_point(aes(color = label), size = 5) +
    geom_rect(aes(xmin = x[1], xmax = 3, ymin = y[2], ymax = 3), fill = "grey80") +
    geom_rect(aes(xmin = -3, xmax = x[2], ymin = y[1], ymax = -3), fill = "grey80") +
    geom_text(aes(label = label, color = label), nudge_x = 0.2, nudge_y = 0.2, size = 10) +
    scale_color_brewer(palette = "Set1") +
    geom_segment(aes(x = x[1], y = y[2], xend = 3, yend = y[2]), color = "grey50", linewidth = 3, lty = "dashed") +
    geom_segment(aes(x = x[1], y = y[2], xend = x[1], yend = 3), color = "grey50", linewidth = 3, lty = "dashed") +
    geom_segment(aes(x = x[2], y = y[1], xend = -3, yend = y[1]), color = "grey50", linewidth = 3, lty = "dashed") +
    geom_segment(aes(x = x[2], y = y[1], xend = x[2], yend = -3), color = "grey50", linewidth = 3, lty = "dashed") +
    geom_segment(aes(x = x[2], y = y[1], xend = x[1], yend = y[2]), color = "grey50", linewidth = 3, lty = "dashed") +
    geom_segment(aes(x = -3, y = 0, xend = 3, yend = 0), linewidth = 1.5, arrow = arrow(length = unit(0.3, "cm"), ends = "both", type = "closed")) +
    geom_segment(aes(x = 0, y = -3, xend = 0, yend = 3), linewidth = 1.5, arrow = arrow(length = unit(0.3, "cm"), ends = "both", type = "closed")) +
    coord_fixed()

p <- p1 + p2 + p3 + p4 + p5 + plot_layout(ncol = 2) + plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")

dat <- data.frame(
    label = c("O[yj]", "O[nj]", "O*plain(\"'\")[yj]", "O*plain(\"'\")[nj]", "Ideal~Point"),
    type = c("bY", "aN", "bY", "aN", "cX"),
    x = c(-1, 1, -2, 0, 1),
    y = c(1, -1, 0, -2, 1)
)

## Figure 2
## BIRT cutting line
p2 <- ggplot(dat, aes(x, y)) +
    scale_color_brewer(palette = "Set1") +
    geom_segment(aes(x = 1, y = -1, xend = 0, yend = 0), color = colors_set[1], linewidth = 5, alpha = 0.5) +
    geom_segment(aes(x = 0, y = -2, xend = -1, yend = -1), color = colors_set[1], linewidth = 5, alpha = 0.5) +
    geom_segment(aes(x = -1, y = 1, xend = 0, yend = 0), color = colors_set[2], linewidth = 5, alpha = 0.5) +
    geom_segment(aes(x = -2, y = 0, xend = -1, yend = -1), color = colors_set[2], linewidth = 5, alpha = 0.5) +
    geom_segment(aes(x = 1, y = 1, xend = -1, yend = -1), color = "grey50", linewidth = 3, lty = "dotted") +
    geom_segment(aes(x = 1, y = -1, xend = -1, yend = 1), color = "grey50", linewidth = 3, lty = "dotted") +
    geom_segment(aes(x = 0, y = -2, xend = -2, yend = 0), color = "grey50", linewidth = 3, lty = "dotted") +
    geom_segment(aes(x = -3, y = 0, xend = 3, yend = 0), linewidth = 1.5, arrow = arrow(length = unit(0.3, "cm"), ends = "both", type = "closed")) +
    geom_segment(aes(x = 0, y = -3, xend = 0, yend = 3), linewidth = 1.5, arrow = arrow(length = unit(0.3, "cm"), ends = "both", type = "closed")) +
    geom_point(aes(color = type), size = 5) +
    geom_text(aes(label = label, color = type), nudge_x = 0.2, nudge_y = 0.2, size = 10, parse = TRUE) +
    coord_fixed()

# Save figure -------------------------------------------------------------

ggsave("figure/cutting_line.pdf", p, width = 14, height = 21, dpi = 300)
ggsave("figure/cutting_line2.pdf", p2, width = 14, height = 14, dpi = 300)
