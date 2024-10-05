##########################################################################
## This R script generates the toy example 2 for comparing the results 
## of the proposed method, W-NOMINATE, and Bayesian IRT (Figure 2).
##########################################################################
## Data: Synthetic data (two voting clusters with correlated dimensions).
## See Section 2, Example 2 for further details.
##########################################################################
## Instructions:
## 1. Part 1 is for fitting the model. Since this process takes time and 
##    involves random data generation, it is recommended to skip this part 
##    and load the saved results in Part 2.
## 2. Part 2 is for saving the figures.
##########################################################################

library(tidyverse)
library(here)
setwd(here("codes", "illustration"))

##########################################################################
# Part 1 ------------------------------------------------------------------
##########################################################################
# Functions ---------------------------------------------------------------
## For generating synthetic data

rmvnorm <- function(n,mu,Sigma){
  E <- matrix(rnorm(n*length(mu)),n,length(mu))
  t( t(E%*%chol(Sigma)) + c(mu) )
}

generate.data.ext <- function(dimensions = 2, 
                              w1 = 0.6,
                              n1 = 40, 
                              n2 = 40, 
                              n3 = 10, 
                              n4 = 10,
                              m = 500,
                              mu1 = c(-0.25,-0.25), 
                              mu2 = c(0.25,0.25),
                              mu3 = c(0.75,-0.75), 
                              mu4 = c(-0.75,0.75), 
                              sigma1 = 0.02, 
                              sigma2 = 0.02, 
                              sigma3 = 0.01, 
                              sigma4 = 0.01, 
                              theta = 1.5, 
                              beta = 10,
                              seed = NULL,
                              link = c("probit", "logistic"),
                              utility = c("gaussian", "quadratic", "linear")) {
  if(!dimensions%in%c(1,2)) stop("'dimensions' must be either 1 or 2.\n")
  if(dimensions==1) w1 <- 1
  
  set.seed(seed)
  
  #### 1. Generate ideal points ####
  n <- n1+n2+n3+n4
  L <- data.frame(name = paste("Legislator",1:n), 
                  group = as.factor(c(rep("group1",n1),rep("group2",n2),rep("group3",n3),rep("group4",n4))),
                  stringsAsFactors = F)
  L[c("true_ideal_point_1d","true_ideal_point_2d")] <- rbind(rmvnorm(n1,mu1,diag(2)*sigma1),
                                                             rmvnorm(n2,mu2,diag(2)*sigma2),
                                                             rmvnorm(n3,mu3,diag(2)*sigma3),
                                                             rmvnorm(n4,mu4,diag(2)*sigma4))
  
  #### 2. Generate Yea & Nay outcome ####
  O <- data.frame(name = paste("Vote",1:m), 
                  true_yea_position_1d = runif(m,-theta,theta),
                  true_yea_position_2d = runif(m,-theta,theta),
                  true_nay_position_1d = runif(m,-theta,theta),
                  true_nay_position_2d = runif(m,-theta,theta))
  
  #### 3. Compute Pr(Yea) & Pr(Nay) ####
  Dijy <- L[rep(1:n, each = m),c("true_ideal_point_1d","true_ideal_point_2d")] - 
    do.call("rbind", rep(list(O[c("true_yea_position_1d","true_yea_position_2d")]), n))
  Dijy <- as.matrix(Dijy)%*%matrix(c(w1,0,0,(1-w1)),2,2) # weighted
  Dijn <- L[rep(1:n, each = m),c("true_ideal_point_1d","true_ideal_point_2d")] - 
    do.call("rbind", rep(list(O[c("true_nay_position_1d","true_nay_position_2d")]), n))
  Dijn <- as.matrix(Dijn)%*%matrix(c(w1,0,0,(1-w1)),2,2) # weighted
  if (utility == "gaussian") {
    Dijy <- (Dijy)^2
    Uijy <- exp(-rowSums(Dijy)/2)*beta
    Dijn <- (Dijn)^2
    Uijn <- exp(-rowSums(Dijn)/2)*beta
  } else if (utility == "quadratic") {
    Dijy <- (Dijy)^2
    Uijy <- -rowSums(Dijy)*beta
    Dijn <- (Dijn)^2
    Uijn <- -rowSums(Dijn)*beta
  } else if (utility == "linear") {
    Dijy <- abs(Dijy)
    Uijy <- -rowSums(Dijy)*beta
    Dijn <- abs(Dijn)
    Uijn <- -rowSums(Dijn)*beta
  }
  
  if (link == "logistic") {
    Ystar <- exp(Uijy)/(exp(Uijy)+exp(Uijn))
  } else if (link == "probit") {
    Ystar <- pnorm(Uijy-Uijn,0,1)
  }
  Ystar <- matrix(Ystar,n,m,byrow=TRUE)
  
  #### 4. Set Y ####
  U <- matrix(runif(m*n,0,1),n,m)
  Y <- U<Ystar
  Y <- 1*Y
  rownames(Y) <- L$name
  colnames(Y) <- O$name
  
  if(dimensions==1) {
    L <- L[,c("name","group","true_ideal_point_1d")]
    O <- O[,c("true_yea_position_1d","true_nay_position_1d")]
  }
  
  dat <- list(votes = Y, 
              legis_data = L, 
              votes_data = O, 
              true_weight = c(w1, 1-w1), 
              true_beta = beta,
              code = match.call())
  
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

circleFun <- function(center = c(0,0),diameter = 2, npoints = 1000){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

proj <- function(u, v) {
  return(as.vector( (u %*% v) / (v %*% v) ) * v)
}

# Synthetic data ----------------------------------------------------------


dat <- generate.data.ext(link = "probit", utility = "gaussian", w1=0.5, seed=123,
                         mu1 = c(-0.5,-0.5),
                         mu2 = c(0.5,0.5),
                         n1 = 50,
                         n2 = 50,
                         n3 = 0,
                         n4 = 0,
                         m = 1000,
                         sigma1 = 0.1,
                         sigma2 = 0.1,
                         theta = 1.5)

rc <- pscl::rollcall(dat$votes, yea=1, nay=0, missing=2, notInLegis=3,
                     legis.names=dat$legis_data$name, legis.data=dat$legis_data)

cols <- gg_color_hue(4)

plot_true <- ggplot() + 
  geom_point(aes(x = true_ideal_point_1d, 
                 y = true_ideal_point_2d, 
                 col = group, 
                 shape = group), 
             data = dat$legis_data, alpha = 0.9) +
  theme_classic() +
  theme(legend.position="none") +
  labs(x="Dimension 1",y="Dimension 2",title="(a) True Ideal Points") +
  geom_segment(aes(x = -1.5, y = 0, xend = 1.5, yend = 0), arrow = arrow(length = unit(0.3, "cm"), ends="both", type="closed"))+
  geom_segment(aes(x = 0, y = -1.5, xend = 0, yend = 1.5), arrow = arrow(length = unit(0.3, "cm"), ends="both", type="closed"))+ 
  annotate("text", x = .75, y = -0.1, label = "Economic dimension", size = 3.5) +
  annotate("text", x = -.1, y = .75, label = "Social dimension", size = 3.5, angle=90) +
  geom_segment(aes(x = -1.5, y = -1.5, xend = 1.5, yend = 1.5), linetype="dashed",colour = "red", arrow = arrow(length = unit(0.3, "cm"), ends="both", type="closed")) + 
  annotate("text", x = -.5, y = -.5, label = "A", size = 7) +
  annotate("text", x = .5, y = .5, label = "B", size = 7)  + 
  scale_colour_manual(values = cols)


# W-NOMINATE --------------------------------------------------------------

# anchors to fix the sign flip
ind <- rep(NA, 2)
proj.u <- apply(dat$legis_data %>% select(true_ideal_point_1d, true_ideal_point_2d), 
      1, function(u) proj(u, c(1,1)))[1,]
ind[1] <- which.max(proj.u)
proj.u <- apply(dat$legis_data %>% 
                  select(true_ideal_point_1d, true_ideal_point_2d), 
                1, function(u) proj(u, c(-1,1)))[2,]
ind[2] <- which.max(proj.u)

set.seed(1)
wn.sim <- wnominate::wnominate(rc, ubeta=15, uweights=0.5, dims=2, minvotes=20,
                    lop=0.025,trials=3, polarity=c(ind[1],ind[2]), verbose=T)
circ <- circleFun()

plot_wnom <- ggplot() +
  geom_path(data = circ, aes(x=x,y=y),color = "grey") +
  geom_point(aes(x = coord1D, y = coord2D, col = group, shape = group),
             data = wn.sim$legislators, alpha = 0.9) +
  scale_colour_manual(values = cols) +
  theme_classic() + 
  theme(legend.position="none") + 
  labs(x="Dimension 1",y="Dimension 2",title="(b) W-NOMINATE Estimates") +
  geom_segment(aes(x = -1, y = 0, xend = 1, yend = 0), linetype="dashed", colour = "red", arrow = arrow(length = unit(0.3, "cm"), ends="both", type="closed"))+
  geom_segment(aes(x = 0, y = -1, xend = 0, yend = 1), colour = grey(0.4), arrow = arrow(length = unit(0.3, "cm"), ends="both", type="closed"))+ 
  annotate("text", x = .5, y = -0.1, label = "Socio-economic dimension", size = 3.5, colour = "red") +
  annotate("text", x = -.1, y = .5, label = "Residual dimension", size = 3.5, colour = grey(0.4), angle=90)+ 
  annotate("text", x = -.5, y = 0, label = "A", size = 7) +
  annotate("text", x = .5, y = 0, label = "B", size = 7)


# Bayesian IRT ------------------------------------------------------------

set.seed(1)
idl <- pscl::ideal(rc, dropList=list(lop=0,legisMin=10),
             priors=NULL, startvals="eigen", 
             d=2, maxiter=10000, thin=10, burnin=1000,
             impute=FALSE, normalize=FALSE,
             store.item=TRUE, file=NULL, verbose=TRUE)
proj.u <- apply(dat$legis_data %>% select(true_ideal_point_1d, true_ideal_point_2d), 
                1, function(u) proj(u, c(1,1)))[1,]
ind[1] <- which.min(proj.u)
ind[2] <- which.max(proj.u)
ind
proj.u.inc <- apply(dat$legis_data %>% select(true_ideal_point_1d, true_ideal_point_2d), 
                1, function(u) proj(u, c(1,0)))[1,]
inc = which.min(proj.u.inc)
inc
half.idl <- pscl::postProcess(idl,
                        constraints=list("Legislator 22"=c(dat$legis_data$true_ideal_point_1d[ind[1]],dat$legis_data$true_ideal_point_2d[ind[1]]),
                                         "Legislator 18"=c(dat$legis_data$true_ideal_point_1d[ind[1]],dat$legis_data$true_ideal_point_2d[ind[2]]),
                                         "Legislator 64"=c(dat$legis_data$true_ideal_point_1d[ind[2]],dat$legis_data$true_ideal_point_2d[ind[2]])))
g1 <- as.data.frame(half.idl$xbar) %>%
  mutate(group=dat$legis_data$group)
g2 <- dat$legis_data[c(ind[1],ind[2]),]
g3 <- data.frame(name="Legislator 18", group="group3",true_ideal_point_1d=dat$legis_data$true_ideal_point_1d[ind[1]],true_ideal_point_2d=dat$legis_data$true_ideal_point_2d[ind[2]])

plot_half <- ggplot() + 
  geom_point(aes(x = D1, y = D2, col = group, shape=group), data = g1, alpha = 0.9) + 
  scale_colour_manual(values = cols) + 
  geom_point(data=g2, aes(x = true_ideal_point_1d, y = true_ideal_point_2d),shape=3,size = 4, stroke = 1) + 
  geom_point(data=g3, aes(x = true_ideal_point_1d, y = true_ideal_point_2d),shape=3,size = 4, stroke = 1, col="red") + 
  theme_classic() + 
  theme(legend.position="none") + 
  labs(x="Dimension 1",y="Dimension 2",title="(c) IRT Estimates") + 
  annotate("text", x = .5, y = 0.05, label = "B", size = 7) +
  annotate("text", x = -.5, y = 0.05, label = "A", size = 7) +
  annotate("text", x = dat$legis_data$true_ideal_point_1d[ind[1]]+0.2, 
           y = dat$legis_data$true_ideal_point_2d[ind[2]], label = "misspecified", col = "red")    

##########################################################################
# Part 2 ------------------------------------------------------------------
##########################################################################
# Save figure -------------------------------------------------------------
plot_ls <- list(plot_true = plot_true, plot_wnom = plot_wnom, plot_half = plot_half)
saveRDS(plot_ls, "../../data/illustration/problem_plot.rds")

plot_ls <- readRDS("../../data/illustration/problem_plot.rds")
library(patchwork)
plot_ls[[1]] + plot_ls[[2]] + plot_ls[[3]]
ggsave(filename="../../figure/problem.pdf", width=12, height=4)
