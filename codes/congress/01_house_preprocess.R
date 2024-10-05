##########################################################################
## Main Analysis: US House of Representatives (52nd to 55th)
## Pre-process Votes Data
##########################################################################
## Instructions:
## Run the following code to read data from the Voteview website and 
## pre-process it. The pre-processed data will be saved in the 
## data/congress/Rdata folder.
##########################################################################

library(tidyverse)
library(l1ideal)
library(future)
library(furrr)
plan(multisession)
library(here)
setwd(here("codes", "congress"))

## Read the data from the Voteview website
Hvotes_list <- paste0("https://voteview.com/static/data/out/votes/H0", 52:55, "_votes.csv") %>%
  lapply(., read.csv)
Hmembers_list <- paste0("https://voteview.com/static/data/out/members/H0", 52:55, "_members.csv") %>%
  lapply(., read_csv)
Hparties_list <- paste0("https://voteview.com/static/data/out/parties/H0", 52:55, "_parties.csv") %>% 
  lapply(., read_csv)
Hrollcalls_list <- paste0("https://voteview.com/static/data/out/rollcalls/H0", 52:55, "_rollcalls.csv") %>% 
  lapply(., read_csv)

## Function for preprocess
preprocess_votes <- function(i){
  ## drop president from the data
  president_icpsr <- Hmembers_list[[i]] %>%
    filter(chamber == "President") %>%
    pull(icpsr)
  ## recode votes so that yea=1, nay=0, abstention=2, not a member=3
  ## make it as n by m matrix where n is the number of legislators and m is the number of bills
  H_votes <- Hvotes_list[[i]] %>% 
    filter(chamber == "House") %>% 
    filter(!icpsr %in% president_icpsr) %>%
    dplyr::select(icpsr, rollnumber, cast_code) %>% 
    mutate(cast_code = case_when(
      as.numeric(cast_code) %in% c(1, 2, 3) ~ 1, # yea
      as.numeric(cast_code) %in% c(4, 5, 6) ~ 0, # nay
      as.numeric(cast_code) %in% c(7, 8, 9) ~ 2, # abstention
      as.numeric(cast_code) == 0 ~ 3 # not a member
    )) %>% 
    pivot_wider(names_from = "rollnumber", values_from = "cast_code") %>% 
    as.data.frame()
  Hicpsr <- H_votes$icpsr
  H_votes <- H_votes %>% select(-icpsr)
  Hrollnumber <- as.numeric(colnames(H_votes))
  ## generate legislator data
  H_legis <- Hmembers_list[[i]] %>%
    filter(icpsr %in% Hicpsr) %>%
    arrange(match(icpsr, Hicpsr)) %>%
    left_join(Hparties_list[[i]] %>% select(party_code, party_name) %>% distinct(),
              by = "party_code")
  ## generate bill data
  H_bills <- Hrollcalls_list[[i]] %>%
    filter(rollnumber %in% Hrollnumber)
  
  ## save
  save(H_votes, H_legis, H_bills, 
       file = paste0("../../data/congress/Rdata/H", unique(Hmembers_list[[i]]$congress), ".RData"))
}

## Pre-process votes
future_map(1:length(Hvotes_list), preprocess_votes)
