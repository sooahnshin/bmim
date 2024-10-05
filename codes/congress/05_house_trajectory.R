##########################################################################
## Main Analysis: US House of Representatives (52nd to 55th) 
## Visualization of the Results from the 52nd to 55th Congresses. 
## (Main Figure 8)
## (Appendix Figures 26, 27, 28)
##########################################################################
## Instructions: Run the following code to generate and save the figures.
##########################################################################

library(here)
setwd(here("codes", "congress"))

##########################################################################
# Main ------------------------------------------------------------------
##########################################################################

library(ggrepel)
source("funs.R")

circleFun <- function(center = c(0,0),diameter = 2, npoints = 1000){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

# H52 -------------------------------------------------------

# prepare data
load("../../data/congress/results/H52.RData")

res$running_time
# [1] "14037.595 seconds"
south <- c("MD", "DE", "VA", "WV", "KY", "NC", "SC", "TN", "GA", "AL", "FL", "MS", "LA", "AR", "MO", "TX")
west <- c("IA", "MN", "KS", "NE", "SD", "ND", "CO", "CA", "WY", "WA", "OR", "MT", "ID", "NV")
north <- unique(legis$state_abbrev[!legis$state_abbrev %in% c(south, west)])
group <- c("Southern Democrat", "Democrat", "Ind. Democrat", "Republican", "National Greenbacker", "Readjuster Democrat", "Independent",
           "Readjuster", "Ind. Republican", "Union", "Union Labor", "Populist", "Silver", "Silver Republican", "Progressive Republican",
           "Socialist", "Progressive", "Prohibitionist", "Farmer-Labor", "American Labor")
group.col = c("#e36934", "#1405BD", "#635FBD", "#DE0100", "#006400", "#0004A1", "#ABABAB", 
              "#000382", "#DE7E78", "#7f3300", "#5550c8", "#62bd6d", "#777777", "#ff4444", "#912929", 
              "#cd3700", "#b63434", "#ff00ff", "#EAEB13", "#ff0000")


res <- postprocess.l1ideal(res)
l1object <- res
color.group <- legis$group
shape.group <- legis$group
dim <- length(l1object$legislators)
df <- summary.l1ideal(l1object)

# NOMINATE
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

# BMIM
ideal_df2 <- ideal_df
ideal_df2$ideal_point_1d <- -df[df$parameter=="ideal_point_1d","mean"]
ideal_df2$ideal_point_2d <- df[df$parameter=="ideal_point_2d","mean"]

# BMIM figure
l1.52 <- ggplot() + 
  geom_rect(aes(xmin = -3.1, xmax = 3.1, ymin = 0, ymax = 3.1),
            fill = "lightgrey", alpha = 0.5) +
  annotate("text", x = -3.1+0.1, y = 3-0.1, label = "Anti-gold", color = "grey40", 
           hjust = 0, fontface = "bold") +
  geom_point(aes(x = ideal_point_1d, y = ideal_point_2d, 
                 shape = section, col = color.group), 
             data = ideal_df2, 
             alpha = 0.7, size = 1.5) +
  theme_classic() +
  labs(x="Dimension 1: Partisan", y="Dimension 2: Gold Standard",
       title="(a) L1 Norm Ideal Point Estimation") +
  coord_equal(xlim = c(-3,3), ylim = c(-3,3)) +
  scale_shape_manual(name = "State", 
                     breaks = c("N", "S", "W"), 
                     values = c(19, 17, 15),
                     labels = c("Northeast", "South", "West")) +
  scale_color_manual(name = "Group", breaks = group, values = group.col) +
  theme(legend.position = "bottom")

# NOMINATE figure
wn.52 <- ggplot() + 
  geom_path(data = circ, aes(x=x,y=y),color = "grey") +
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

# save data
df52 <- ideal_df2
dfwn52 <- ideal_df


# H53 ---------------------------------------------------------------------

# prepare data
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

# NOMINATE
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

# BMIM
ideal_df2 <- ideal_df
ideal_df2$ideal_point_1d <- df[df$parameter=="ideal_point_1d","mean"]
ideal_df2$ideal_point_2d <- df[df$parameter=="ideal_point_2d","mean"]

# BMIM figure
l1.53 <- ggplot() + 
  geom_rect(aes(xmin = -3.1, xmax = 3.1, ymin = 0, ymax = 3.1),
            fill = "lightgrey", alpha = 0.5) +
  annotate("text", x = -3.1+0.1, y = 3-0.1, label = "Anti-gold", color = "grey40", 
           hjust = 0, fontface = "bold") +
  geom_point(aes(x = ideal_point_1d, y = ideal_point_2d, 
                 shape = section, col = color.group), 
             data = ideal_df2, 
             alpha = 0.7, size = 1.5) +
  theme_classic() +
  labs(x="Dimension 1: Partisan", y="Dimension 2: Gold Standard",
       title="(a) L1 Norm Ideal Point Estimation") +
  coord_equal(xlim = c(-3,3), ylim = c(-3,3)) +
  scale_shape_manual(name = "State", 
                     breaks = c("N", "S", "W"), 
                     values = c(19, 17, 15),
                     labels = c("Northeast", "South", "West")) +
  scale_color_manual(name = "Group", breaks = group, values = group.col) +
  theme(legend.position = "bottom")

# NOMINATE figure
wn.53 <- ggplot() + 
  geom_path(data = circ, aes(x=x,y=y),color = "grey") +
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

# save data
df53 <- ideal_df2
dfwn53 <- ideal_df

# H54 -------------------------------------------------------

# prepare data
load("../../data/congress/results/H54.RData")
res$running_time
# [1] "7690.4 seconds"

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

# NOMINATE
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

# BMIM
ideal_df2 <- ideal_df
ideal_df2$ideal_point_1d <- -df[df$parameter=="ideal_point_2d","mean"]
ideal_df2$ideal_point_2d <- -df[df$parameter=="ideal_point_1d","mean"]

# BMIM figure
l1.54 <- ggplot() + 
  geom_rect(aes(xmin = -3.1, xmax = 0, ymin = -3.1, ymax = 3.1),
            fill = "lightgrey", alpha = 0.5) +
  annotate("text", x = -3.1+0.1, y = 3-0.1, label = "Anti-gold", color = "grey40", 
           hjust = 0, fontface = "bold") +
  geom_point(aes(x = ideal_point_1d, y = ideal_point_2d, 
                 shape = section, col = color.group), 
             data = ideal_df2, 
             alpha = 0.7, size = 1.5) +
  theme_classic() +
  labs(x="Dimension 1: Partisan", y="Dimension 2: Gold Standard",
       title="(a) L1 Norm Ideal Point Estimation") +
  coord_equal(xlim = c(-3,3), ylim = c(-3,3)) +
  scale_shape_manual(name = "State", 
                     breaks = c("N", "S", "W"), 
                     values = c(19, 17, 15),
                     labels = c("Northeast", "South", "West")) +
  scale_color_manual(name = "Group", breaks = group, values = group.col) +
  theme(legend.position = "bottom")

# NOMINATE figure
wn.54 <- ggplot() + 
  geom_path(data = circ, aes(x=x,y=y),color = "grey") +
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

# save data
df54 <- ideal_df2
dfwn54 <- ideal_df

# H55 -------------------------------------------------------

# prepare data
load("../../data/congress/results/H55.RData")
res$running_time
# [1] "9146.716 seconds"

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

# NOMINATE
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

# BMIM
ideal_df2 <- ideal_df
ideal_df2$ideal_point_1d <- df[df$parameter=="ideal_point_2d","mean"]
ideal_df2$ideal_point_2d <- -df[df$parameter=="ideal_point_1d","mean"]

# BMIM figure
l1.55 <- ggplot() + 
  geom_rect(aes(xmin = -3.1, xmax = 0, ymin = -3.1, ymax = 3.1),
            fill = "lightgrey", alpha = 0.5) +
  annotate("text", x = -3.1+0.1, y = 3-0.1, label = "Anti-gold", color = "grey40", 
           hjust = 0, fontface = "bold") +
  geom_point(aes(x = ideal_point_1d, y = ideal_point_2d, 
                 shape = section, col = color.group), 
             data = ideal_df2, 
             alpha = 0.7, size = 1.5) +
  theme_classic() +
  labs(x="Dimension 1: Partisan", y="Dimension 2: Gold Standard",
       title="(a) L1 Norm Ideal Point Estimation") +
  coord_equal(xlim = c(-3,3), ylim = c(-3,3)) +
  scale_shape_manual(name = "State", 
                     breaks = c("N", "S", "W"), 
                     values = c(19, 17, 15),
                     labels = c("Northeast", "South", "West")) +
  scale_color_manual(name = "Group", breaks = group, values = group.col) +
  theme(legend.position = "bottom")

# NOMINATE figure
wn.55 <- ggplot() + 
  geom_path(data = circ, aes(x=x,y=y),color = "grey") +
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

# save data
df55 <- ideal_df2
dfwn55 <- ideal_df

# Main --------------------------------------------------------------------

# combine figures

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
             data = rbind(df52,
                          df53,
                          df54,
                          df55), 
             alpha = 0.7, size = 1.5) +
  theme_classic() +
  labs(x="Dimension 1: Partisan", y="Dimension 2: Gold Standard",
       title="(a) L1 Norm Ideal Point Estimation") +
  coord_equal(xlim = c(-1,1), ylim = c(-1,1)) +
  scale_shape_manual(name = "State", 
                     breaks = c("N", "S", "W"), 
                     values = c(19, 17, 15),
                     labels = c("Northeast", "South", "West")) +
  scale_color_manual(name = "Group", breaks = group, values = group.col) +
  theme(legend.position = "bottom")
# title
l1.title <- ggplot() + 
  annotate("text", x = 1, y = 1, size=5, label = "L1 Norm Ideal Point Estimation") + 
  theme_void()
mylegend <- g_legend(leg)

# NY representatives
ny54 <- df54 %>%
  filter(state == "NY" & color.group == "Democrat")
ny55 <- df55 %>%
  filter(state == "NY" & color.group == "Democrat")

## Figure 26
pdf(file="../../figure/l1ideal_appl.pdf", width=9, height=10)
grid.arrange(l1.title,
             l1.52 + labs(x="Dimension 1: Partisan", y="Dimension 2: Gold Standard", title = "(a) The 52nd US H.R. (1891-1893)") + theme(legend.position = "none"), 
             l1.53 + labs(x="Dimension 1: Partisan", y="Dimension 2: Gold Standard", title = "(b) The 53rd US H.R. (1893-1895)") + theme(legend.position = "none"),
             l1.54 + 
               labs(x="Dimension 1: Partisan/Gold Standard", y="Dimension 2: Business/Capital Markets", title = "(c) The 54th US H.R. (1895-1897)") + theme(legend.position = "none") + 
               geom_text_repel(aes(x = ideal_point_1d, y = ideal_point_2d, 
                                   label = state),
                               min.segment.length = 0, seed = 42, 
                               data = ny54),
             l1.55 + 
               labs(x="Dimension 1: Partisan/Gold Standard", y="Dimension 2: Business/Capital Markets", title = "(d) The 55th US H.R. (1897-1899)") + theme(legend.position = "none") + 
               geom_text_repel(aes(x = ideal_point_1d, y = ideal_point_2d, 
                                   label = state),
                               min.segment.length = 0, seed = 42, 
                               data = ny55),
             mylegend, layout_matrix = lay)
dev.off()


# DW-NOMINATE -------------------------------------------------------------

wn.title <- ggplot() + 
  annotate("text", x = 1, y = 1, size=5, label = "DW-NOMINATE") + 
  theme_void()

ny54 <- dfwn54 %>%
  filter(state == "NY" & color.group == "Democrat")
ny55 <- dfwn55 %>%
  filter(state == "NY" & color.group == "Democrat")

## Figure 27
pdf(file="../../figure/dwnom_appl.pdf", width=9, height=10)
grid.arrange(wn.title,
             wn.52 + 
               labs(title = "(a) The 52nd US H.R. (1891-1893)") + theme(legend.position = "none"), 
             wn.53 + 
               labs(title = "(b) The 53rd US H.R. (1893-1895)") + theme(legend.position = "none"),
             wn.54 + 
               labs(title = "(c) The 54th US H.R. (1895-1897)") + theme(legend.position = "none") +
               geom_text_repel(aes(x = ideal_point_1d, y = ideal_point_2d, 
                                   label = state),
                               min.segment.length = 0, seed = 42, 
                               data = ny54),
             wn.55 + 
               labs(title = "(d) The 55th US H.R. (1897-1899)") + theme(legend.position = "none") +
               geom_text_repel(aes(x = ideal_point_1d, y = ideal_point_2d, 
                                   label = state),
                               min.segment.length = 0, seed = 42, 
                               data = ny55),
             mylegend, layout_matrix = lay)
dev.off()

# IRT -------------------------------------------------------------

## H52
rm(list = setdiff(ls(), c("mylegend", "lay")))
load("../../data/congress/irt/irt_H52.RData")
south <- c("MD", "DE", "VA", "WV", "KY", "NC", "SC", "TN", "GA", "AL", "FL", "MS", "LA", "AR", "MO", "TX")
west <- c("IA", "MN", "KS", "NE", "SD", "ND", "CO", "CA", "WY", "WA", "OR", "MT", "ID", "NV")
north <- unique(legis$state_abbrev[!legis$state_abbrev %in% c(south, west)])
group <- c("Southern Democrat", "Democrat", "Ind. Democrat", "Republican", "National Greenbacker", "Readjuster Democrat", "Independent",
           "Readjuster", "Ind. Republican", "Union", "Union Labor", "Populist", "Silver", "Silver Republican", "Progressive Republican",
           "Socialist", "Progressive", "Prohibitionist", "Farmer-Labor", "American Labor")
group.col <- c("#e36934", "#1405BD", "#635FBD", "#DE0100", "#006400", "#0004A1", "#ABABAB", 
               "#000382", "#DE7E78", "#7f3300", "#5550c8", "#62bd6d", "#777777", "#ff4444", "#912929", 
               "#cd3700", "#b63434", "#ff00ff", "#EAEB13", "#ff0000")
dfirt52 <- data.frame(ideal_point_1d = idl$xbar[,1], ideal_point_2d = idl$xbar[,2],
                      name = legis$bioname, color.group = legis$group, shape.group = legis$group,
                      state = legis$state_abbrev, stringsAsFactors = F) %>%
  mutate(section = case_when(state %in% south ~ "S", state %in% west ~ "W", state %in% north ~ "N"))

irt.52 <- ggplot() + 
  geom_point(aes(x = ideal_point_1d, y = ideal_point_2d, shape = section, col = color.group), 
             data = dfirt52,  alpha = 0.7, size = 1.5) +
  theme_classic() +
  labs(x="Dimension 1", y="Dimension 2", title="(a) The 52nd US H.R. (1891-1893)") +
  scale_shape_manual(name = "State", breaks = c("N", "S", "W"), values = c(19, 17, 15),
                     labels = c("Northeast", "South", "West")) +
  scale_color_manual(name = "Group", breaks = group, values = group.col) +
  theme(legend.position = "bottom")

## H53
load("../../data/congress/irt/irt_H53.RData")
dfirt53 <- data.frame(ideal_point_1d = idl$xbar[,1], ideal_point_2d = idl$xbar[,2],
                      name = legis$bioname, color.group = legis$group, shape.group = legis$group,
                      state = legis$state_abbrev, stringsAsFactors = F) %>%
  mutate(section = case_when(state %in% south ~ "S", state %in% west ~ "W", state %in% north ~ "N"))

irt.53 <- ggplot() + 
  geom_point(aes(x = ideal_point_1d, y = ideal_point_2d, shape = section, col = color.group), 
             data = dfirt53,  alpha = 0.7, size = 1.5) +
  theme_classic() +
  labs(x="Dimension 1", y="Dimension 2", title="(b) The 53rd US H.R. (1893-1895)") +
  scale_shape_manual(name = "State", breaks = c("N", "S", "W"), values = c(19, 17, 15),
                     labels = c("Northeast", "South", "West")) +
  scale_color_manual(name = "Group", breaks = group, values = group.col) +
  theme(legend.position = "bottom")

## H54
load("../../data/congress/irt/irt_H54.RData")
dfirt54 <- data.frame(ideal_point_1d = idl$xbar[,1], ideal_point_2d = idl$xbar[,2],
                      name = legis$bioname, color.group = legis$group, shape.group = legis$group,
                      state = legis$state_abbrev, stringsAsFactors = F) %>%
  mutate(section = case_when(state %in% south ~ "S", state %in% west ~ "W", state %in% north ~ "N"))

irt.54 <- ggplot() + 
  geom_point(aes(x = ideal_point_1d, y = ideal_point_2d, shape = section, col = color.group), 
             data = dfirt54,  alpha = 0.7, size = 1.5) +
  theme_classic() +
  labs(x="Dimension 1", y="Dimension 2", title="(c) The 54th US H.R. (1895-1897)") +
  scale_shape_manual(name = "State", breaks = c("N", "S", "W"), values = c(19, 17, 15),
                     labels = c("Northeast", "South", "West")) +
  scale_color_manual(name = "Group", breaks = group, values = group.col) +
  theme(legend.position = "bottom")

## H55
load("../../data/congress/irt/irt_H55.RData")
legis <- legis[(legis$bioname %in% rownames(idl$xbar)),]
dfirt55 <- data.frame(ideal_point_1d = idl$xbar[,1], ideal_point_2d = idl$xbar[,2],
                      name = legis$bioname, color.group = legis$group, shape.group = legis$group,
                      state = legis$state_abbrev, stringsAsFactors = F) %>%
  mutate(section = case_when(state %in% south ~ "S", state %in% west ~ "W", state %in% north ~ "N"))

irt.55 <- ggplot() + 
  geom_point(aes(x = ideal_point_1d, y = ideal_point_2d, shape = section, col = color.group), 
             data = dfirt55,  alpha = 0.7, size = 1.5) +
  theme_classic() +
  labs(x="Dimension 1", y="Dimension 2", title="(d) The 55th US H.R. (1897-1899)") +
  scale_shape_manual(name = "State", breaks = c("N", "S", "W"), values = c(19, 17, 15),
                     labels = c("Northeast", "South", "West")) +
  scale_color_manual(name = "Group", breaks = group, values = group.col) +
  theme(legend.position = "bottom")

## combine
irt.title <- ggplot() + 
  annotate("text", x = 1, y = 1, size=5, label = "Bayesian IRT (without pre-/post-process)") + 
  theme_void()
ny54 <- dfirt54 %>%
  filter(state == "NY" & color.group == "Democrat")
ny55 <- dfirt55 %>%
  filter(state == "NY" & color.group == "Democrat")

## Figure 28
pdf(file="../../figure/irt_appl.pdf", width=9, height=10)
grid.arrange(irt.title,
             irt.52 + 
               theme(legend.position = "none"), 
             irt.53 + 
               theme(legend.position = "none"),
             irt.54 + 
               theme(legend.position = "none") +
               geom_text_repel(aes(x = ideal_point_1d, y = ideal_point_2d, 
                                   label = state),
                               min.segment.length = 0, seed = 42, 
                               data = ny54),
             irt.55 + 
               theme(legend.position = "none") +
               geom_text_repel(aes(x = ideal_point_1d, y = ideal_point_2d, 
                                   label = state),
                               min.segment.length = 0, seed = 42, 
                               data = ny55),
             mylegend, layout_matrix = lay)
dev.off()
