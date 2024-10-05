##########################################################################
## Main Analysis: US House of Representatives (52nd to 55th) 
## Fit Bayesian IRT (for comparison)
##########################################################################
## Instructions:
## The following code fits the Bayesian IRT model to the pre-processed data.
## Since this code involves running MCMC, it may take a while to run.
## You may want to skip this code and use the results saved in the 
## data/congress/irt folder in the next parts.
##########################################################################

setwd(here("codes", "congress"))
source("helper.R")

# H52 ---------------------------------------------------------------------
load("../../data/congress/RData/H52.RData")
load("../../data/congress/results/H52.RData")
# make rollcall object
rc <- pscl::rollcall(H_votes,
                     yea = 1, nay = 0, missing = c(NA, 2), notInLegis = 3,
                     legis.names = H_legis$bioname, legis.data = H_legis,
                     vote.names = H_bills$rollnumber, vote.data = H_bills)
set.seed(123)
# fit model
idl <- pscl::ideal(rc, dropList=list(lop=0,legisMin=20),
                   priors=NULL, startvals="eigen", 
                   d=2, maxiter=50000, thin=10, burnin=20000,
                   impute=FALSE, normalize=FALSE,
                   store.item=TRUE, file=NULL, verbose=TRUE)
# save results
save(bills, legis, votes, rc, idl,
     file = "../../data/congress/irt/irt_H52.RData")

# H53 ---------------------------------------------------------------------

load("../../data/congress/RData/H53.RData")
load("../../data/congress/results/H53.RData")
# make rollcall object
rc <- pscl::rollcall(H_votes,
                     yea = 1, nay = 0, missing = c(NA, 2), notInLegis = 3,
                     legis.names = H_legis$bioname, legis.data = H_legis,
                     vote.names = H_bills$rollnumber, vote.data = H_bills)
set.seed(123)
# fit model
idl <- pscl::ideal(rc, dropList=list(lop=0,legisMin=20),
                   priors=NULL, startvals="eigen", 
                   d=2, maxiter=50000, thin=10, burnin=20000,
                   impute=FALSE, normalize=FALSE,
                   store.item=TRUE, file=NULL, verbose=TRUE)
# save results
save(bills, legis, votes, rc, idl,
     file = "../../data/congress/irt/irt_H53.RData")

# H54 ---------------------------------------------------------------------

load("../../data/congress/RData/H54.RData")
load("../../data/congress/results/H54.RData")
# make rollcall object
rc <- pscl::rollcall(H_votes,
                     yea = 1, nay = 0, missing = c(NA, 2), notInLegis = 3,
                     legis.names = H_legis$bioname, legis.data = H_legis,
                     vote.names = H_bills$rollnumber, vote.data = H_bills)
set.seed(123)
# fit model
idl <- pscl::ideal(rc, dropList=list(lop=0,legisMin=20),
                   priors=NULL, startvals="eigen", 
                   d=2, maxiter=50000, thin=10, burnin=20000,
                   impute=FALSE, normalize=FALSE,
                   store.item=TRUE, file=NULL, verbose=TRUE)
# save results
save(bills, legis, votes, rc, idl,
     file = "../../data/congress/irt/irt_H54.RData")

# H55 ---------------------------------------------------------------------

load("../../data/congress/RData/H55.RData")
load("../../data/congress/results/H55.RData")
# make rollcall object
rc <- pscl::rollcall(H_votes,
                     yea = 1, nay = 0, missing = c(NA, 2), notInLegis = 3,
                     legis.names = H_legis$bioname, legis.data = H_legis,
                     vote.names = H_bills$rollnumber, vote.data = H_bills)
set.seed(123)
# fit model
idl <- pscl::ideal(rc, dropList=list(lop=0,legisMin=20),
                   priors=NULL, startvals="eigen", 
                   d=2, maxiter=50000, thin=10, burnin=20000,
                   impute=FALSE, normalize=FALSE,
                   store.item=TRUE, file=NULL, verbose=TRUE)
# save results
save(bills, legis, votes, rc, idl,
     file = "../../data/congress/irt/irt_H55.RData")
