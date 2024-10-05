library(tidyverse)
library(l1ideal)
library(plyr) # for stacked histogram
library(gridExtra)

## generic functions
filter_votes <- function(rollcall, lop = 0, minvotes = 0) {
  votes <- rollcall$votes
  yea_code <- rollcall$codes$yea
  nay_code <- rollcall$codes$nay
  legis_filtered <- apply(matrix(votes %in% c(yea_code, nay_code), nrow(votes), ncol(votes)), 1, sum) >= minvotes
  lop_denominator <- pmin(apply(matrix(votes %in% yea_code, nrow(votes), ncol(votes)), 2, sum),
                          apply(matrix(votes %in% nay_code, nrow(votes), ncol(votes)), 2, sum))
  lop_numerator <- apply(matrix(votes %in% c(yea_code, nay_code), nrow(votes), ncol(votes)), 2, sum)
  votes_lop <- lop_denominator / lop_numerator
  votes_lop[lop_numerator == 0] <- 0
  votes_filtered <- votes_lop > lop
  
  tmp <- rollcall$votes[legis_filtered, votes_filtered]
  flag <- check_unanimous(votes = tmp, yea_code = yea_code, nay_code = nay_code)
  if(!is.null(flag)) {
    votes_filtered[which(colnames(votes) %in% flag)] <- FALSE
  }
  
  return(list(legis = legis_filtered, bills = votes_filtered))
}

check_unanimous <- function(votes, yea_code = 1, nay_code = 0) {
  votes_vec <- as.vector(votes)
  votes_vec[!votes_vec %in% c(yea_code, nay_code)] <- NA
  votes_vec[votes_vec == yea_code] <- 1
  votes_vec[votes_vec == nay_code] <- 0
  votes_matrix <- matrix(votes_vec, nrow = nrow(votes), ncol = ncol(votes))
  
  unanimous_yea_cols <- colnames(votes)[which(colSums(votes_matrix == 1, na.rm = TRUE) == 0)]
  unanimous_nay_cols <- colnames(votes)[which(colSums(votes_matrix == 0, na.rm = TRUE) == 0)]
  
  unanimous_cols <- unique(c(unanimous_yea_cols, unanimous_nay_cols))
  
  if (length(unanimous_cols) > 0) {
    return(unanimous_cols)
  } else {
    return(NULL)
  }
}

# Reference: https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots/28594060#28594060
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

cuttingline_angle <- function(res_df) { # output of summary.l1ideal(l1object)
  bills <- res_df %>%
    filter(parameter %in% c("yea_position_1d","yea_position_2d",
                            "nay_position_1d","nay_position_2d")) %>%
    select(parameter, mean, name) %>%
    pivot_wider(names_from = parameter, values_from = mean) %>%
    mutate(mid1 = (yea_position_1d+nay_position_1d)/2) %>%
    mutate(mid2 = (yea_position_2d+nay_position_2d)/2) %>%
    mutate(slope = -(yea_position_1d-nay_position_1d)/(yea_position_2d-nay_position_2d)) %>%
    mutate(intercept = -1*mid1*slope+mid2) %>%
    mutate(angle = ifelse(slope>0, atan(slope)*180/pi, atan(slope)*180/pi+180))
  return(bills)
}

hist_angle <- function(bills_df, p.title="Cutting Line Angles") { # output of cutting line angle (column-wise) bound with bill data frame 
  p <- bills_df %>%
    ggplot(aes(angle)) + 
    theme_bw() +
    geom_histogram(aes(y = (..count..)/sum(..count..)),
                   color="black", fill="white",
                   breaks=seq(0, 180, by=10)) + 
    scale_y_continuous(breaks=seq(0, 0.12, by=0.03), labels = scales::percent,
                       limits = c(0, 0.1205)) + 
    scale_x_continuous(breaks=seq(0, 180, by=20), labels = seq(0, 180, by=20)) + 
    labs(x=NULL,y=NULL,title=p.title)  +
    stat_bin(aes(y=(..count..)/sum(..count..) + 0.003, label=..count..), 
             geom="text", breaks=seq(0, 180, by=10))+
    theme(legend.position="none", 
          panel.grid = element_blank(),
          axis.ticks.x=element_blank())
  return(p)
}

hist_angle_stacked <- function(bills_df, # output of cutting line angle (column-wise) bound with bill data frame 
                       group = "bill_number",
                       p.title="Cutting Line Angles",
                       lb = 0) { 
  bills_df$bin <- .bincode(bills_df$angle, seq(0, 180, by=10))
  bills_df_bar <- ddply(bills_df, .(bills_df[,group], bills_df$bin), nrow)
  names(bills_df_bar) <- c("bill", "bin", "freq")
  p <- bills_df_bar %>%
    ggplot(aes(x = bin, y = freq/nrow(bills_df),  fill = bill)) +
    theme_bw() +
    geom_bar(stat = "identity", colour = "black", width = 1) +
    geom_text(aes(label=ifelse(freq > lb, ifelse(is.na(bill), "", bill), "")),
              position=position_stack(vjust=0.5), colour="black") + 
    scale_y_continuous(breaks=seq(0, 0.12, by=0.03), labels = scales::percent,
                       limits = c(0, 0.1205)) + 
    scale_x_continuous(breaks=seq(0.5, 18.5, by=2), labels = seq(0, 180, by=20)) + 
    labs(x=NULL,y=NULL,title=p.title)  +
    theme(legend.position="none", 
          panel.grid = element_blank(),
          axis.ticks.x=element_blank())
  return(p)
}
