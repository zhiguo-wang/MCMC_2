#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#   Program Name:   LIFP - Cash Flow - Mortgage
#   Version:        1.1
#
#   Created By:     Justin Xu
#   Modified By:    Justin Xu
#
#   Datasource:     Individual Characteristics
#   Source Tables:  \\Inputs\\IndCha
#   Target Tables:  \\Outputs\\
#
#   Purpose:        To create cash flow matrix of mortgage
#
#   Version           Date           Author            change
#               |               |               |
#     1.1       |   10/16/15    |    Justin     |      First Version
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 1. Load MCMC Database (Unif_MCMC.RData)
#    And Load Individual Characteristics (Inputs_IndCha.RData)
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 2. Locate The Appropriate MCMC Matrix for The Individual
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 3. Divide The MCMC Matrix into pre_mcmc and post_mcmc
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 4. Cash Flows of Mortgage 
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cashOut_Mortgage <- matrix(0, nrow(pre_mcmc), ncol(pre_mcmc))

yearMortPeriod <- monMortPeriod %/% 12

remainMonMort <- monMortPeriod %% 12 

cashOut_Mortgage[1 : yearMortPeriod, ] <- monMortPay * 12

cashOut_Mortgage[yearMortPeriod + 1, ] <- monMortPay * remainMonMort

## Test in which scenario and which year an individual died before retirement age 

adj_scenario_Mort <- nrow(pre_mcmc) - as.matrix(table(which(pre_mcmc == 3, arr.ind = TRUE)[ , 2]))

## the column numbers of scenarios which need to be adjusted

scenarioCol_Mort <- as.numeric(row.names(adj_scenario_Mort))[
  which(adj_scenario_Mort < ceiling(monMortPeriod / 12))]

for (i in scenarioCol_Mort) {
  remainMort <- 0
  for (j in 1 : (ceiling(monMortPeriod / 12) - min(which(pre_mcmc[ , i] == 3)))) {
    remainMort <- 
      remainMort + 
      cashOut_Mortgage[min(which(pre_mcmc[ , i] == 3)) + j, i] * (1 + mortgageInterest) ^ (-j)
    cashOut_Mortgage[min(which(pre_mcmc[ , i] == 3)) + j, i] <- 0
  }
  cashOut_Mortgage[min(which(pre_mcmc[ , i] == 3)), i] <- 
    cashOut_Mortgage[min(which(pre_mcmc[ , i] == 3)), i] + remainMort
}

cashOut_Mortgage <- - cashOut_Mortgage

## When healthy, before actual budget, mortgage is already covered

cashOut_Mortgage[which(pre_mcmc == 1)] <- 0