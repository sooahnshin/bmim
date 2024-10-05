##########################################################################
## Main Analysis: US House of Representatives (52nd to 55th) 
## Visualization of the Results
## (Main Figures 3 & 7)
## (Appendix Figures 3, 23, 24, 25)
## (Appendix Tables 2 & 3)
##########################################################################
## Instructions: Run the following code to generate and save the main figures.
##########################################################################

library(here)
setwd(here("codes", "congress"))

##########################################################################
# Main ------------------------------------------------------------------
##########################################################################

# Motivating Figure -------------------------------------------------------
#### DW-NOMINATE of H53 ####

source("funs.R")

circleFun <- function(center = c(0,0),diameter = 2, npoints = 1000){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

load("../../data/congress/results/H53.RData")

res$running_time
# [1] "16518.911 seconds"

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
# DW-NOMINATE Score is available from the raw data, downloaded from the Voteview website
ideal_df <- data.frame(ideal_point_1d = legis$nominate_dim1,
                       ideal_point_2d = legis$nominate_dim2,
                       name = legis$bioname,
                       stringsAsFactors = F)
ideal_df$color.group <- color.group
ideal_df$shape.group <- shape.group
ideal_df <- ideal_df %>%
  mutate(state = legis$state_abbrev) %>%
  mutate(section = case_when(state %in% south ~ "S",
                             state %in% west ~ "W",
                             state %in% north ~ "N"))
circ <- circleFun()

p1 <- ggplot() + 
  geom_path(data = circ, aes(x=x,y=y),color = "grey") +
  geom_point(aes(x = ideal_point_1d, y = ideal_point_2d, 
                 shape = section, col = color.group), 
             data = ideal_df %>% mutate(color.group = ifelse(color.group == "Southern Democrat", "Democrat", color.group)), 
             alpha = 0.8, size = 2.5) +
  theme_classic() +
  labs(x="Dimension 1: Economic/Redistributive", y="Dimension 2: Other Votes",
       title="DW-NOMINATE of the 53rd US H.R. (1893-1895)") +
  coord_equal(xlim = c(-1,1), ylim = c(-1,1)) +
  scale_shape_manual(name = "State", 
                     breaks = c("N", "S", "W"), 
                     values = c(19, 17, 15),
                     labels = c("Northeast", "South", "West")) +
  scale_color_manual(name = "Party", breaks = group, values = group.col) +
  theme(legend.position = "bottom", legend.box="vertical")

## Figure 3
pdf(file="../../figure/H53_motiv.pdf", width=6, height=6)
p1
dev.off()

# Application Figure -------------------------------------------------------
#### Main ####
ideal_df2 <- ideal_df
# df contains the estimated ideal points
ideal_df2$ideal_point_1d <- df[df$parameter=="ideal_point_1d","mean"]
ideal_df2$ideal_point_2d <- df[df$parameter=="ideal_point_2d","mean"]

# BMIM
p1 <- ggplot() + 
  geom_point(aes(x = ideal_point_1d, y = ideal_point_2d, 
                 shape = section, col = color.group), 
             data = ideal_df2, 
             alpha = 0.7, size = 1.5) +
  theme_classic() +
  labs(x="Dimension 1: Partisan", y="Dimension 2: Gold Standard",
       title="(a) L1 Norm Ideal Point Estimation") +
  scale_shape_manual(name = "State", 
                     breaks = c("N", "S", "W"), 
                     values = c(19, 17, 15),
                     labels = c("Northeast", "South", "West")) +
  scale_color_manual(name = "Group", breaks = group, values = group.col) +
  theme(legend.position = "bottom")

# DW-NOMINATE
p2 <- ggplot() + 
  geom_point(aes(x = ideal_point_1d, y = ideal_point_2d, 
                 shape = section, col = color.group), 
             data = ideal_df, 
             alpha = 0.7, size = 1.5) +
  theme_classic() +
  labs(x="Dimension 1: Economic/Redistributive", y="Dimension 2: Other Votes",
       title="(b) DW-NOMINATE") +
  coord_equal(xlim = c(-1,1), ylim = c(-1,1)) +
  scale_shape_manual(name = "State", 
                     breaks = c("N", "S", "W"), 
                     values = c(19, 17, 15),
                     labels = c("Northeast", "South", "West")) +
  scale_color_manual(name = "Group", breaks = group, values = group.col) +
  theme(legend.position = "bottom")

mylegend <- g_legend(p2)

# layout
lay <- rbind(c(1,1,1,1,1,1,1,1,1,1),
             c(2,2,2,2,2,3,3,3,3,3),
             c(2,2,2,2,2,3,3,3,3,3),
             c(2,2,2,2,2,3,3,3,3,3),
             c(2,2,2,2,2,3,3,3,3,3),
             c(2,2,2,2,2,3,3,3,3,3),
             c(4,4,4,4,4,4,4,4,4,4))

p.title <- ggplot() + 
  annotate("text", x = 1, y = 1, size=5, label = "Ideal Points of the 53rd US H.R. (1893-1895)") + 
  theme_void()

## Figure 7
pdf(file="../../figure/H53_appl.pdf", width=8, height=5)
grid.arrange(p.title,
             p1 + theme(legend.position = "none"), p2 + theme(legend.position = "none"),
             mylegend, layout_matrix = lay)
dev.off()

##########################################################################
# Appendix ------------------------------------------------------------------
##########################################################################

#### Silver Bills ####
# we create rollcall votes matrix for three silver related rollcalls (#6, #60, #224)
rcv1 <- votes[,which(bills$rollnumber == 6)]
rcv1[is.na(rcv1)] <- 3 # NA coding
rcv2 <- votes[,which(bills$rollnumber == 60)]
rcv2[is.na(rcv2)] <- 3
rcv3 <- votes[,which(bills$rollnumber == 224)]
rcv3[is.na(rcv3)] <- 3

# estimated yea/nay locations
rcv1.y1 <- df |> filter(parameter == "yea_position_1d", name == 6) |> pull(mean)
rcv1.y2 <- df |> filter(parameter == "yea_position_2d", name == 6) |> pull(mean)
rcv1.n1 <- df |> filter(parameter == "nay_position_1d", name == 6) |> pull(mean)
rcv1.n2 <- df |> filter(parameter == "nay_position_2d", name == 6) |> pull(mean)

rcv2.y1 <- df |> filter(parameter == "yea_position_1d", name == 60) |> pull(mean)
rcv2.y2 <- df |> filter(parameter == "yea_position_2d", name == 60) |> pull(mean)
rcv2.n1 <- df |> filter(parameter == "nay_position_1d", name == 60) |> pull(mean)
rcv2.n2 <- df |> filter(parameter == "nay_position_2d", name == 60) |> pull(mean)

rcv3.y1 <- df |> filter(parameter == "yea_position_1d", name == 224) |> pull(mean)
rcv3.y2 <- df |> filter(parameter == "yea_position_2d", name == 224) |> pull(mean)
rcv3.n1 <- df |> filter(parameter == "nay_position_1d", name == 224) |> pull(mean)
rcv3.n2 <- df |> filter(parameter == "nay_position_2d", name == 224) |> pull(mean)

# first rcv
p.rcv1 <- ggplot() + 
  geom_point(aes(x = ideal_point_1d, y = ideal_point_2d, 
                 shape = section, 
                 col = factor(rcv1, 
                              levels = c(0,1,2,3),
                              labels = c("Gold", "Silver", "abstention", "n.a."))), 
             data = ideal_df2, 
             alpha = 0.7, size = 1.5) +
  theme_classic() +
  labs(x=expression("Democrat" %<->% "Republican"), 
       y=expression("Gold" %<->% "Silver"),
       title="(a) Bland Amendment (H.R.1 Amendment)") +
  coord_equal(xlim = c(-3,3), ylim = c(-3,3)) +
  scale_shape_manual(name = "State", 
                     breaks = c("N", "S", "W"), 
                     values = c(19, 17, 15),
                     labels = c("Northeast", "South", "West")) +
  scale_color_manual(name = "Vote",
                     breaks = c("Gold", "Silver", "abstention", "n.a."),
                     values = c("goldenrod3", "azure4", "lightgrey", "lightgrey")) +  
  theme(legend.position = "bottom") 

p.rcv1 <- p.rcv1 +
  annotate("text", label = expression(o[y]),
           x = rcv1.y1, y = rcv1.y2, col = "azure4", size = 7) +
  annotate("text", label = expression(o[n]),
           x = rcv1.n1, y = rcv1.n2, col = "goldenrod3", size = 7)

# second rcv
p.rcv2 <- ggplot() + 
  geom_point(aes(x = ideal_point_1d, y = ideal_point_2d, 
                 shape = section, 
                 col = factor(rcv2, 
                              levels = c(0,1,2,3),
                              labels = c("Silver", "Gold", "abstention", "n.a."))), 
             data = ideal_df2, 
             alpha = 0.7, size = 1.5) +
  theme_classic() +
  labs(x=expression("Democrat" %<->% "Republican"), 
       y=expression("Gold" %<->% "Silver"),
       title="(b) Sherman Act Repeal (H.R.1)") +
  coord_equal(xlim = c(-3,3), ylim = c(-3,3)) +
  scale_shape_manual(name = "State", 
                     breaks = c("N", "S", "W"), 
                     values = c(19, 17, 15),
                     labels = c("Northeast", "South", "West")) +
  scale_color_manual(name = "Vote",
                     breaks = c("Gold", "Silver", "abstention", "n.a."),
                     values = c("goldenrod3", "azure4", "lightgrey", "lightgrey")) +  
  theme(legend.position = "bottom") 

p.rcv2 <- p.rcv2 +
  annotate("text", label = expression(o[y]),
           x = rcv2.y1, y = rcv2.y2, col = "goldenrod3", size = 7) +
  annotate("text", label = expression(o[n]),
           x = rcv2.n1, y = rcv2.n2, col = "azure4", size = 7)

# third rcv
p.rcv3 <- ggplot() + 
  geom_point(aes(x = ideal_point_1d, y = ideal_point_2d, 
                 shape = section, 
                 col = factor(rcv3, 
                              levels = c(0,1,2,3),
                              labels = c("Gold", "Silver (yea)", "abstention", "n.a."))), 
             data = ideal_df2, 
             alpha = 0.7, size = 1.5) +
  theme_classic() +
  labs(x=expression("Democrat" %<->% "Republican"), 
       y=expression("Gold" %<->% "Silver"),
       title="(c) Free Silver Override (H.R.4956)") +
  coord_equal(xlim = c(-3,3), ylim = c(-3,3)) +
  scale_shape_manual(name = "State", 
                     breaks = c("N", "S", "W"), 
                     values = c(19, 17, 15),
                     labels = c("Northeast", "South", "West")) +
  scale_color_manual(name = "Vote",
                     breaks = c("Gold", "Silver", "abstention", "n.a."),
                     values = c("goldenrod3", "azure4", "lightgrey", "lightgrey")) +  
  theme(legend.position = "bottom") 

p.rcv3 <- p.rcv3 +
  annotate("text", label = expression(o[y]),
           x = rcv3.y1, y = rcv3.y2, col = "azure4", size = 7) +
  annotate("text", label = expression(o[n]),
           x = rcv3.n1, y = rcv3.n2, col = "goldenrod3", size = 7)

mylegend2 <- g_legend(p.rcv3)

p.title2 <- ggplot() + 
  annotate("text", x = 1, y = 1, size=5, label = "Roll Call Votes of the 53rd US H.R. (1893-1895)") + 
  theme_void()

# layout
lay.rcv <- rbind(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
                 c(2,2,2,2,2,3,3,3,3,3,4,4,4,4,4),
                 c(2,2,2,2,2,3,3,3,3,3,4,4,4,4,4),
                 c(2,2,2,2,2,3,3,3,3,3,4,4,4,4,4),
                 c(2,2,2,2,2,3,3,3,3,3,4,4,4,4,4),
                 c(2,2,2,2,2,3,3,3,3,3,4,4,4,4,4),
                 c(5,5,5,5,5,5,5,5,5,5,5,5,5,5,5))

## Figure 3/25
pdf(file="../../figure/H53_rcv.pdf", width=12, height=5)
grid.arrange(p.title2,
             p.rcv1 + theme(legend.position = "none"), 
             p.rcv2 + theme(legend.position = "none"),
             p.rcv3 + theme(legend.position = "none"),
             mylegend2, layout_matrix = lay.rcv)
dev.off()

#### Bills Appendix ####

# table of HR1 related votes
library(xtable)
table1 <- bills %>%
  filter(bill_number == "HR1") %>%
  mutate(Roll = as.character(rollnumber),
         Date = as.character(date),
         Yea = as.character(yea_count),
         Nay = as.character(nay_count),
         Description = str_to_sentence(dtl_desc)) %>%
  select(Roll, Date, Yea, Nay, Description) %>%
  xtable()
## Table 2
print(table1,
      hline.after = rep(c(1:10,2)))

# table of HR4956 related votes
table2 <- bills %>%
  filter(bill_number == "HR4956") %>%
  mutate(Roll = as.character(rollnumber),
         Date = as.character(date),
         Yea = as.character(yea_count),
         Nay = as.character(nay_count),
         Description = str_to_sentence(dtl_desc)) %>%
  select(Roll, Date, Yea, Nay, Description) %>%
  xtable()
## Table 3
print(table2,
      hline.after = rep(c(1:31,2)))

rn1 <- bills %>%
  filter(bill_number == "HR1") %>%
  pull(rollnumber)
ideal_df3 <- ideal_df2
p1.ls <- list()
for (i in 1:length(rn1)) {
  rc = votes[,which(bills$rollnumber == rn1[i])]
  rc[is.na(rc)] = 3
  ideal_df3$rc = rc
  p1.ls[[i]] <- ggplot() + 
    geom_point(aes(x = ideal_point_1d, y = ideal_point_2d, 
                   shape = section, 
                   col = factor(rc, 
                                levels = c(0,1,2,3),
                                labels = c("nay", "yea", "abstention", "n.a."))), 
               data = ideal_df3, 
               alpha = 0.7, size = 1.5) +
    theme_classic() +
    labs(x=expression("Democrat" %<->% "Republican"), 
         y=expression("Gold" %<->% "Silver"),
         title=paste0("Roll #", rn1[i])) +
    coord_equal(xlim = c(-3,3), ylim = c(-3,3)) +
    scale_shape_manual(name = "State", 
                       breaks = c("N", "S", "W"), 
                       values = c(19, 17, 15),
                       labels = c("Northeast", "South", "West")) +
    scale_color_manual(name = "Vote",
                       breaks = c("nay", "yea", "abstention", "n.a."),
                       values = c("Red", "Blue", "lightgrey", "lightgrey")) +
    theme(legend.position = "none")
}
p.eg <- ggplot() + 
  geom_point(aes(x = ideal_point_1d, y = ideal_point_2d, 
                 shape = section, 
                 col = factor(rc, 
                              levels = c(0,1,2,3),
                              labels = c("nay", "yea", "abstention", "n.a."))), 
             data = ideal_df3, 
             alpha = 0.7, size = 1.5) +
  theme_classic() +
  labs(x=expression("Democrat" %<->% "Republican"), 
       y=expression("Gold" %<->% "Silver"),
       title=paste0("Roll #", rn1[i])) +
  coord_equal(xlim = c(-3,3), ylim = c(-3,3)) +
  scale_shape_manual(name = "State", 
                     breaks = c("N", "S", "W"), 
                     values = c(19, 17, 15),
                     labels = c("Northeast", "South", "West")) +
  scale_color_manual(name = "Vote",
                     breaks = c("nay", "yea", "abstention", "n.a."),
                     values = c("red", "blue", "lightgrey", "lightgrey")) +
  theme(legend.position = "bottom",legend.box="vertical")

mylegend3 <- g_legend(p.eg)

lay2 <- rbind(c(1,1,2,2,3,3,4,4),
              c(1,1,2,2,3,3,4,4),
              c(5,5,6,6,7,7,8,8),
              c(5,5,6,6,7,7,8,8),
              c(9,9,10,10,11,11,11,11),
              c(9,9,10,10,11,11,11,11))

## Figure 23
pdf(file="../../figure/HR1_rcv1.pdf", width=9, height=6)
grid.arrange(p1.ls[[1]], p1.ls[[2]], p1.ls[[3]], p1.ls[[4]],
             p1.ls[[5]], p1.ls[[6]], p1.ls[[7]], p1.ls[[8]],
             p1.ls[[9]], p1.ls[[10]], mylegend3,
             layout_matrix = lay2)
dev.off()

rn2 <- bills %>%
  filter(bill_number == "HR4956") %>%
  pull(rollnumber)
ideal_df3 = ideal_df2
p2.ls <- list()
for (i in 1:length(rn2)) {
  rc = votes[,which(bills$rollnumber == rn2[i])]
  rc[is.na(rc)] = 3
  ideal_df3$rc = rc
  p2.ls[[i]] <- ggplot() + 
    geom_point(aes(x = ideal_point_1d, y = ideal_point_2d, 
                   shape = section, 
                   col = factor(rc, 
                                levels = c(0,1,2,3),
                                labels = c("nay", "yea", "abstention", "n.a."))), 
               data = ideal_df3, 
               alpha = 0.7, size = 1.5) +
    theme_classic() +
    labs(x=expression("Democrat" %<->% "Republican"), 
         y=expression("Gold" %<->% "Silver"),
         title=paste0("Roll #", rn2[i])) +
    coord_equal(xlim = c(-3,3), ylim = c(-3,3)) +
    scale_shape_manual(name = "State", 
                       breaks = c("N", "S", "W"), 
                       values = c(19, 17, 15),
                       labels = c("Northeast", "South", "West")) +
    scale_color_manual(name = "Vote",
                       breaks = c("nay", "yea", "abstention", "n.a."),
                       values = c("Red", "Blue", "lightgrey", "lightgrey")) +
    theme(legend.position = "none")
}

lay3 <- rbind(c(1,1,2,2,3,3,4,4),
              c(1,1,2,2,3,3,4,4),
              c(5,5,6,6,7,7,8,8),
              c(5,5,6,6,7,7,8,8),
              c(9,9,10,10,11,11,12,12),
              c(9,9,10,10,11,11,12,12))

## Figure 24
pdf(file="../../figure/HR4956_rcv1.pdf", width=9, height=6)
grid.arrange(p2.ls[[1]], p2.ls[[2]], p2.ls[[3]], p2.ls[[4]],
             p2.ls[[5]], p2.ls[[6]], p2.ls[[7]], p2.ls[[8]],
             p2.ls[[9]], p2.ls[[10]], p2.ls[[11]], p2.ls[[12]],
             layout_matrix = lay3)
dev.off()

pdf(file="../../figure/HR4956_rcv2.pdf", width=9, height=6)
grid.arrange(p2.ls[[12+1]], p2.ls[[12+2]], p2.ls[[12+3]], p2.ls[[12+4]],
             p2.ls[[12+5]], p2.ls[[12+6]], p2.ls[[12+7]], p2.ls[[12+8]],
             p2.ls[[12+9]], p2.ls[[12+10]], p2.ls[[12+11]], p2.ls[[12+12]],
             layout_matrix = lay3)
dev.off()

lay4 <- rbind(c(1,1,2,2,3,3,4,4),
              c(1,1,2,2,3,3,4,4),
              c(5,5,6,6,7,7,8,8),
              c(5,5,6,6,7,7,8,8),
              c(9,9,9,9,10,10,11,11),
              c(9,9,9,9,10,10,11,11))

pdf(file="../../figure/HR4956_rcv3.pdf", width=9, height=6)
grid.arrange(p2.ls[[24+1]], p2.ls[[24+2]], p2.ls[[24+3]], p2.ls[[24+4]],
             p2.ls[[24+5]], p2.ls[[24+6]], p2.ls[[24+7]], ggplot() + theme_void(),
             mylegend3, ggplot() + theme_void(), ggplot() + theme_void(),
             layout_matrix = lay4)
dev.off()









