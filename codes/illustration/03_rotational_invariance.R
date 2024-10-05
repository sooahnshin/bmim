##########################################################################
## This R script is used to visualize the illustrative example of 
## rotational invariance (Figure 4).
##########################################################################
## This is purely a visualization script that uses a stylized example 
## and does not involve any data analysis.
##########################################################################

library(tidyverse)
setwd(here("codes", "illustration"))

# Functions ---------------------------------------------------------------

library(gridExtra)
# Reference: https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots/28594060#28594060
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

circleFun <- function(center = c(0,0),diameter = 2, npoints = 1000){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

cols <- gg_color_hue(3)
cols <- cols[c(1,3)]
n1 = 12*8 + 1
circ_leg1 <- circleFun()
circ_leg2 <- circleFun(diameter = 1.5)
leg1_l2 <- circleFun(npoints = n1)
leg2_l2 <- circleFun(diameter = 1.5, npoints = n1)
leg1_l2 <- leg1_l2[c(66:97, 2:65),]
leg2_l2 <- leg2_l2[c(66:97, 2:65),]
dat1 <- data.frame(x = c(leg1_l2$x, leg2_l2$x), y = c(leg1_l2$y, leg2_l2$y),
                 group = c(1:(n1-1), 1:(n1-1)))
## (a) l2 distance
plot_l2 <- ggplot() +
  geom_path(data = circ_leg1, aes(x=x,y=y), color = cols[1], alpha = 0.5) +
  geom_path(data = circ_leg2, aes(x=x,y=y), color = cols[2], alpha = 0.5) +
  geom_path(data = dat1%>%filter(group == 49), aes(x=x,y=y,group=group), color = "black") +
  geom_point(data = leg1_l2[49,], aes(x=x,y=y), color = cols[1], size = 4) +
  geom_point(data = leg2_l2[49,], aes(x=x,y=y), color = cols[2], shape = 15, size = 4) +
  theme_classic() + 
  labs(x="Dimension 1",y="Dimension 2",
       title="(a) l2 distance")

## add possible positions of the legislators
for(k in 1:47) {
  plot_l2 <- plot_l2 +
    geom_path(data = dat1%>%filter(group == (49-k)), aes(x=x,y=y,group=group), color = "black", alpha = 1-0.03*k) +
    geom_point(data = leg1_l2[49-k,], aes(x=x,y=y), color = cols[1], size = 4, alpha = 1-0.03*k) +
    geom_point(data = leg2_l2[49-k,], aes(x=x,y=y), color = cols[2], shape = 15, size = 4, alpha = 1-0.03*k) +
    geom_path(data = dat1%>%filter(group == (49+k)), aes(x=x,y=y,group=group), color = "black", alpha = 1-0.03*k) +
    geom_point(data = leg1_l2[49+k,], aes(x=x,y=y), color = cols[1], size = 4, alpha = 1-0.03*k) +
    geom_point(data = leg2_l2[49+k,], aes(x=x,y=y), color = cols[2], shape = 15, size = 4, alpha = 1-0.03*k) 
  
}
plot_l2 <- plot_l2 + 
  annotate('text', x = leg1_l2[49,1]+0.05, y = leg1_l2[49,2]+0.05,
           label = "x[1]",parse = TRUE,size=5)  +
  annotate('text', x = leg2_l2[49,1]-0.05, y = leg2_l2[49,2]-0.05,
           label = "o[n1]",parse = TRUE,size=5)



n2 <- 13
leg1_l1 <- circleFun(npoints = n2)
leg2_l1 <- circleFun(diameter = 1.5, npoints = n2)
leg1_l1 <- leg1_l1[-c(1,4,7,10,13),]
leg2_l1 <- leg2_l1[-c(1,4,7,10,13),]

dat2 <- data.frame(x = c(leg1_l1$x, leg1_l1$x, leg2_l1$x), 
                  y = c(leg1_l1$y, leg2_l1$y, leg2_l1$y),
                  group = c(1:(n2-5), 1:(n2-5), 1:(n2-5)))
dat2.1 <- dat2 %>% filter(group %in% c(1,4,5,8))
dat2.2 <- dat2 %>% filter(!group %in% c(1,4,5,8))
dat2.2[5:8,"x"] <- dat2.2[9:12,"x"]
dat2.2[5:8,"y"] <- dat2.2[1:4,"y"]
## (b) l1 distance
plot_l1 <- ggplot() +
  geom_abline(slope = 1, intercept = 0, color = "grey", linetype = "dotted") +
  geom_abline(slope = -1, intercept = 0, color = "grey", linetype = "dotted") +
  geom_vline(xintercept = 0, color = "grey", linetype = "dotted") +
  geom_hline(yintercept = 0, color = "grey", linetype = "dotted") +
  geom_path(data = dat2.1 %>% filter(group==2), aes(x=x,y=y,group=group), color = "black") +
  geom_path(data = dat2.2 %>% filter(group==2), aes(x=x,y=y,group=group), color = "black") +
  geom_path(data = dat2.1 %>% filter(group!=2), aes(x=x,y=y,group=group), color = "black", alpha = 0.5) +
  geom_path(data = dat2.2 %>% filter(group!=2), aes(x=x,y=y,group=group), color = "black", alpha = 0.5) +
  geom_point(data = leg1_l1[2,], aes(x=x,y=y), color = cols[1], size = 4) +
  geom_point(data = leg2_l1[2,], aes(x=x,y=y), color = cols[2], shape = 15, size = 4) +
  geom_point(data = leg1_l1[-2,], aes(x=x,y=y), color = cols[1], size = 4, alpha = 0.7) +
  geom_point(data = leg2_l1[-2,], aes(x=x,y=y), color = cols[2], shape = 15, size = 4, alpha = 0.7) +
  theme_classic() + 
  labs(x="Dimension 1",y="Dimension 2", title="(b) l1 distance") +
  annotate('text', x = leg1_l1[2,1]+0.05, y = leg1_l1[2,2]+0.05, 
           label = "x[1]",parse = TRUE,size=5)  +
  annotate('text', x = leg2_l1[2,1]-0.05, y = leg2_l1[2,2]-0.05, 
           label = "o[n1]",parse = TRUE,size=5) 

# Save figure -------------------------------------------------------------

library(patchwork)
plot_l2 + plot_l1 + plot_layout(ncol = 2)
ggsave(filename="../../figure/rotation.pdf", width=10, height=5)

