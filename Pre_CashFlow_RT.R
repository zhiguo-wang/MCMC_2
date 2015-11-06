#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#   Program Name:   LIFP - Cash Flow - Retirement Plan (401k)
#   Version:        1.1
#
#   Created By:     Justin Xu
#   Modified By:    Justin Xu
#
#   Database Used:  Investment Portfolio.csv
#   Source Tables:  \\Inputs\\Financial Products
#   Target Tables:  \\Outputs\\
#
#   Purpose:        To create cash flow matrix of 401k investment portfolio
#
#   Version           Date           Author            change
#               |               |               |
#     1.1       |   10/16/15    |    Justin     |      First Version
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 2. Locate The Appropriate MCMC Matrix for The Individual
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 4. Cash Flows Related to 401k Plan 
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Cash Outflow: Unit Annual Investment is 1

cashOut_401k <- matrix(-1, nrow(pre_mcmc), ncol(pre_mcmc))

cashOut_401k[which(pre_mcmc != 1)] <- 0

## Cash Inflow:

### Low Risk

ad_pre_mcmc <- pre_mcmc

ad_pre_mcmc[which(ad_pre_mcmc == 3)] <- 0

raw_cashIn_401k_Low <- ad_pre_mcmc * yield_Low

raw_cashIn_401k_Low[which(raw_cashIn_401k_Low == 0)] <- 1

cashIn_401k_Low <- matrix(0, nrow(pre_mcmc), ncol(pre_mcmc))

for(i in 1 : ncol(pre_mcmc)) {
  accumulatedValue <- 0
  for (j in 1 : max(which(pre_mcmc[ , i] == 1))) {
    accumulatedValue <- accumulatedValue +
      prod(raw_cashIn_401k_Low[j : max(which(pre_mcmc[ , i] == 1)), i])
  }
  cashIn_401k_Low[max(which(pre_mcmc[ , i] == 1)) , i] <- accumulatedValue
}

### Moderate Risk

raw_cashIn_401k_Mid <- ad_pre_mcmc * yield_Mid

raw_cashIn_401k_Mid[which(raw_cashIn_401k_Mid == 0)] <- 1

cashIn_401k_Mid <- matrix(0, nrow(pre_mcmc), ncol(pre_mcmc))

for(i in 1 : ncol(pre_mcmc)) {
  accumulatedValue <- 0
  for (j in 1 : max(which(pre_mcmc[ , i] == 1))) {
    accumulatedValue <- accumulatedValue +
      prod(raw_cashIn_401k_Mid[j : max(which(pre_mcmc[ , i] == 1)), i])
  }
  cashIn_401k_Mid[max(which(pre_mcmc[ , i] == 1)) , i] <- accumulatedValue
}

### Aggressive Risk

raw_cashIn_401k_Agg <- ad_pre_mcmc * yield_Agg

raw_cashIn_401k_Agg[which(raw_cashIn_401k_Agg == 0)] <- 1

cashIn_401k_Agg <- matrix(0, nrow(pre_mcmc), ncol(pre_mcmc))

for(i in 1 : ncol(pre_mcmc)) {
  accumulatedValue <- 0
  for (j in 1 : max(which(pre_mcmc[ , i] == 1))) {
    accumulatedValue <- accumulatedValue +
      prod(raw_cashIn_401k_Agg[j : max(which(pre_mcmc[ , i] == 1)), i])
  }
  cashIn_401k_Agg[max(which(pre_mcmc[ , i] == 1)) , i] <- accumulatedValue
}