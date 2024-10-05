##########################################################################
## Additional Analysis: US House of Representatives (52nd to 55th within a single model) 
## Pre-process Votes Data for Dynamic Analysis (running the entire period)
## See Appendix P for details.
##########################################################################
## Instructions:
## Run the following code for pre-processing the data for the dynamic analysis.
##########################################################################

library(here)
setwd(here("codes", "congress"))
library(tidyverse)
library(l1ideal)
source("funs.R")

## read data
load("../../data/congress/RData/H52.RData")
H_votes_52 <- H_votes; H_legis_52 <- H_legis; H_bills_52 <- H_bills
load("../../data/congress/RData/H53.RData")
H_votes_53 <- H_votes; H_legis_53 <- H_legis; H_bills_53 <- H_bills
load("../../data/congress/RData/H54.RData")
H_votes_54 <- H_votes; H_legis_54 <- H_legis; H_bills_54 <- H_bills
load("../../data/congress/RData/H55.RData")
H_votes_55 <- H_votes; H_legis_55 <- H_legis; H_bills_55 <- H_bills

southern <- c("MD", "DE", "VA", "WV", "KY", "NC", "SC", "TN", "GA", "AL", "FL", "MS", "LA", "AR", "MO", "OK", "TX")

## pre-process votes
colnames(H_votes_52) <- paste0("H52_", H_bills_52$rollnumber)
colnames(H_votes_53) <- paste0("H53_", H_bills_53$rollnumber)
colnames(H_votes_54) <- paste0("H54_", H_bills_54$rollnumber)
colnames(H_votes_55) <- paste0("H55_", H_bills_55$rollnumber)

H_votes_52$icpsr <- H_legis_52$icpsr
H_votes_53$icpsr <- H_legis_53$icpsr
H_votes_54$icpsr <- H_legis_54$icpsr
H_votes_55$icpsr <- H_legis_55$icpsr

## make it as long format
H_votes_52_long <- H_votes_52 %>% gather(key = "bill", value = "vote", -icpsr)
H_votes_53_long <- H_votes_53 %>% gather(key = "bill", value = "vote", -icpsr)
H_votes_54_long <- H_votes_54 %>% gather(key = "bill", value = "vote", -icpsr)
H_votes_55_long <- H_votes_55 %>% gather(key = "bill", value = "vote", -icpsr)

## merge
H_votes_long <- bind_rows(H_votes_52_long, H_votes_53_long, H_votes_54_long, H_votes_55_long)
# View(H_votes_long)

## make it as wide format
H_votes_wide <- H_votes_long %>% pivot_wider(names_from = bill, values_from = vote)
# View(H_votes_wide)
icpsr <- H_votes_wide$icpsr
bill_number <- colnames(H_votes_wide)[-1]
H_votes <- H_votes_wide %>% select(-icpsr)

## legislator data & bill data
H_bills <- bind_rows(H_bills_52, H_bills_53, H_bills_54, H_bills_55)
H_legis <- bind_rows(H_legis_52, H_legis_53, H_legis_54, H_legis_55)
H_legis <- H_legis %>% select(chamber, icpsr, state_abbrev, party_code, bioname, party_name)
H_legis <- distinct(H_legis)
H_legis <- tibble(icpsr = icpsr) %>% left_join(H_legis, by = "icpsr") %>% distinct()
## 3 legislators changed party
H_legis |>
  filter(icpsr %in% H_legis$icpsr[which(duplicated(H_legis$icpsr))])
## only keep legis data before they changed party
H_legis <- H_legis |>
  filter(!(icpsr == 6120 & party_code == 329)) |>
  filter(!(icpsr == 4157 & party_code == 354)) |>
  filter(!(icpsr == 8394 & party_code == 354))

## check
dim(H_votes)
length(icpsr)
nrow(H_legis)

## new rollnumber
H_bills <- H_bills %>%
  mutate(congress_rollnumber = paste0(congress, "_", rollnumber)) 

save(H_votes, H_legis, H_bills, 
       file = "../../data/congress/Rdata/H52_55.RData")