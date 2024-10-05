##########################################################################
## This R script generates the toy example 1 for comparing the results 
## of the proposed method, W-NOMINATE, and Bayesian IRT (Figures 1 & 5).
##########################################################################
## Data: Synthetic data (small chamber with two uncorrelated dimensions)
## See Section 2, Example 1 for further details.
##########################################################################
## Instructions:
## 1. Part 1 is for fitting the model. Since this process takes time and 
##    involves random data generation, it is recommended to skip this part 
##    and load the saved results in Part 2.
## 2. Part 2 is for generating the figures.
##########################################################################

library(tidyverse)
library(here)
setwd(here("codes", "illustration"))

##########################################################################
# Part 1 ------------------------------------------------------------------
##########################################################################
## Skip this part and load the saved results in Part 2

## Setup
n <- 4 # number of legislators
m <- 100*4 # number of votes
votes <- matrix(0, nrow = n, ncol = m)
votes[3:4,1:100] <- 1
votes[1:2,101:200] <- 1
votes[c(2,4),201:300] <- 1
votes[c(1,3),301:400] <- 1
# See Table 1 of the paper for the resulting roll call votes data

# IRT ---------------------------------------------------------------------

library(pscl)
rc <- rollcall(votes, yea = 1, nay = 0)
set.seed(123)
idl <- ideal(rc, dropList=list(lop=0,legisMin=20),
                   priors=NULL, startvals="eigen", 
                   d=2, maxiter=10000, thin=10, burnin=5000,
                   impute=FALSE, normalize=FALSE,
                   store.item=TRUE, file=NULL, verbose=TRUE)
plot(idl)
idl$xbar


# NOMINATE ----------------------------------------------------------------

library(wnominate)
set.seed(123)
wn <- wnominate(rc, polarity = c(1,1))
plot(wn)
wn$weights


# l1ideal -----------------------------------------------------------------

library(l1ideal)
res <- l1ideal(rc, dimensions = 2,  mcmc  = 10000, thin = 10, burnin = 5000, 
               minvotes = 20,
               lop = 0, verbose = 1000, seed = 123)
plot.l1ideal(res)
res_df = summary.l1ideal(res)

save.image("../../data/illustration/toy_example.RData")

##########################################################################
# Part 2 ------------------------------------------------------------------
##########################################################################
# Load the saved results
load("../../data/illustration/toy_example.RData")

df <- data.frame(label = c("A1", "A2", "B1", "B2"),
           group1 = as.factor(c("A", "A", "B", "B")),
           group2 = as.factor(c("1","2","1","2")),
           l1ideal1 = res_df$mean[res_df$parameter == "ideal_point_1d"],
           l1ideal2 = res_df$mean[res_df$parameter == "ideal_point_2d"],
           ideal1 = res_df$mean[res_df$parameter == "ideal_point_1d"]/abs(res_df$mean[res_df$parameter == "ideal_point_1d"]),
           ideal2 = res_df$mean[res_df$parameter == "ideal_point_2d"]/abs(res_df$mean[res_df$parameter == "ideal_point_2d"]),
           irt1 = idl$xbar[,1],
           irt2 = idl$xbar[,2],
           nominate1 = wn$legislators[,"coord1D"],
           nominate2 = wn$legislators[,"coord2D"],
           stringsAsFactors = FALSE)

# For illustrating how the l1ideal works (Figure 5)
library(mvtnorm)

# normal prior
mu    <- c(0,0)
sigma <- matrix(c(1, 0, 0, 1), nrow=2)
rng   <- 2.5
N     <- 200
X     <- seq(mu[1]-rng*sigma[1, 1], mu[1]+rng*sigma[1, 1], length.out=N)
Y     <- seq(mu[2]-rng*sigma[2, 2], mu[2]+rng*sigma[2, 2], length.out=N)
set.seed(123)
genZ <- function(x, y) { dmvnorm(cbind(x, y), mu, sigma) }
matZ <- outer(X, Y, FUN="genZ")

ctr <- data.frame(x1 = rep(X, N),
                 x2 = rep(Y, each = N),
                 dens = as.vector(matZ))

colors <- colorRampPalette(c("white", "#3C5488B2"))( 18 )

val <- sqrt(2*abs(qnorm(0.008))^2)

# For visualizing restriction of NOMINATE
circleFun <- function(center = c(0,0),diameter = 2, npoints = 1000){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
circ <- circleFun()
library(ggrepel)

## Figure 1
## (a) Ideal Results
p1 <- ggplot() +
  theme_classic() + 
  geom_point(data = df, aes(x = ideal1, y = ideal2,
                            shape = group1, color = group2), size = 4,
             show.legend = FALSE) +
  scale_shape_manual(name = "Group",
                     breaks=c("A","B"),
                     values=c("circle", "triangle"),
                     guide = "none") +
  scale_color_manual(name = "Group",
                     breaks=c("1","2"),
                     values=c("orange", "brown"),
                     guide = "none") +
  geom_text(data = df,
            aes(x = ideal1, y = ideal2-0.2,
                label = label), size = 4) +
  labs(x = "Dimension 1: Number", y = "Dimension 2: Alphabet",
       title = "(a) Ideal Results") +
  theme(axis.text=element_blank(),
        axis.ticks=element_blank()) +
  coord_fixed(ratio = 1, xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5))

## (b) W-NOMINATE Results
p2 <- ggplot() +
  theme_classic() + 
  geom_path(data = circ, aes(x=x,y=y),color = "grey") +
  geom_point(data = df, aes(x = nominate1, y = nominate2,
                            shape = group1, color = group2), size = 4,
             show.legend = FALSE) +
  scale_color_manual(name = "Group",
                     breaks=c("1","2"),
                     values=c("orange", "brown")) +
  geom_text(data = df %>%
              filter(group1 == "B") %>%
              mutate(label = paste0(label, "(",round(nominate1,1), ",", round(nominate2,1), ")")), 
            aes(x = nominate1 -0.05, y = nominate2 -0.1,
                           label = label), size = 4)  +
  geom_text(data = df %>%
              filter(group1 == "A") %>%
              mutate(label = paste0(label, "(",round(nominate1,1), ",", round(nominate2,1), ")")), 
            aes(x = nominate1 +0.05, y = nominate2 -0.1,
                label = label), size = 4) +
  labs(x = "Dimension 1", y = "Dimension 2",
       title = "(b) W-NOMINATE") +
  coord_fixed(ratio = 1)

## (c) Bayesian IRT Results
p3 <- ggplot() +
  scale_fill_manual(values=colors) +
  theme_classic() + 
  geom_point(data = df, aes(x = irt1, y = irt2,
                            shape = group1, color = group2), size = 4,
             show.legend = FALSE) +
  scale_color_manual(name = "Group",
                     breaks=c("1","2"),
                     values=c("orange", "brown")) +
  geom_text_repel(data = df %>%
              mutate(label = paste0(label, "(",round(irt1,1), ",", round(irt2,1), ")")),  
            aes(x = irt1, y = irt2 -0.5,
                           label = label), size = 4, seed = 2) +
  labs(x = "Dimension 1", y = "Dimension 2",
       title = "(c) Bayesian IRT") +
  coord_fixed(ratio = 1)

## previous version is written in terms of 1.7 instead of 2.4
## illustration is the same, thus we can use the same code
## just need to scale the values
df <- df |>
  mutate(l1ideal1_label = l1ideal1,
         l1ideal2_label = l1ideal2,
         l1ideal1 = l1ideal1  * 1.7/2.4,
         l1ideal2 = l1ideal2  * 1.7/2.4,)

## (a) BMIM
p.l1 <- ggplot() +
  geom_contour_filled(data = ctr, aes(x1, x2, z = dens),
                      bins = 18, show.legend = TRUE) +
  scale_fill_manual(values=colors) +
  theme_classic() +
  geom_polygon(data = data.frame(x = c(val, 0, -val, 0), y = c(0, val, 0, -val)), aes(x = x, y = y),
               col = "#E64B35B2", alpha = 0) +
  geom_point(data = df, aes(x = l1ideal1, y = l1ideal2,
                            shape = group1, color = group2), size = 4,
             show.legend = FALSE) +
  geom_point(data = data.frame(x1 = c(val/2, val/2, -val/2, -val/2),
                               x2 = c(-val/2, val/2, -val/2, val/2)), 
             aes(x = x1, y = x2),
             color = "purple", alpha = 0.5) +
  scale_shape_manual(name = "Group",
                     breaks=c("A","B"),
                     values=c("circle", "triangle"),
                     guide = "none") +
  scale_color_manual(name = "Group",
                     breaks=c("1","2"),
                     values=c("orange", "brown"),
                     guide = "none") +
  labs(x = "Dimension 1: Alphabet", y = "Dimension 2: Number",
       title = bquote("(a)"~p==1~"")) +
  coord_fixed(xlim = c(-val+0.3, val-0.3), ylim = c(-val+0.3, val-0.3)) +
   theme(
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(), 
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank()  
  )

circ2 <- circleFun(diameter = abs(qnorm(0.008))*2)
circ3 <- circleFun(diameter = abs(qnorm(0.008))*2,npoints = 150)

## (b) p = 2
p.l2 <- ggplot() +
  geom_contour_filled(data = ctr, aes(x1, x2, z = dens),
                      bins = 18, show.legend = FALSE) +
  scale_fill_manual(values=colors) +
  theme_classic() +
  geom_path(data = circ2, aes(x=x,y=y),color = "#E64B35B2") +
  geom_point(data = circ3, 
             aes(x = x, y = y),
             color = "purple", alpha = 0.5) +
  labs(x = "Dimension 1", y = "Dimension 2",
       title = bquote("(b)"~p==2)) +
  coord_fixed(xlim = c(-val+0.3, val-0.3), ylim = c(-val+0.3, val-0.3))

val2 <- abs(qnorm(0.008))

## p = \infty
p.linf <- ggplot() +
  geom_contour_filled(data = ctr, aes(x1, x2, z = dens),
                      bins = 18, show.legend = FALSE) +
  scale_fill_manual(values=colors) +
  theme_classic() +
  geom_polygon(data = data.frame(x = c(val2,val2,-val2,-val2), 
                                 y = c(val2,-val2,-val2,val2)), aes(x = x, y = y),
               col = "#E64B35B2", alpha = 0) +  
  geom_point(data = data.frame(x = c(val2, 0, -val2, 0), y = c(0, val2, 0, -val2)),
             aes(x = x, y = y),
             color = "purple", alpha = 0.5) +
  labs(x = "Dimension 1", y = "Dimension 2",
       title = expression("(c)"~p==infinity)) +
  coord_fixed(xlim = c(-val+0.3, val-0.3), ylim = c(-val+0.3, val-0.3))

# Save Figure -------------------------------------------------------------

library(patchwork)
## Figure 1
p1 + p2 + p3
ggsave(filename = "../../figure/toy_example.pdf", width=11, height=4)

## Figure 5
p.l1 + p.l2 + p.linf
ggsave(filename = "../../figure/toy_example_lp.pdf", width=11, height=4)