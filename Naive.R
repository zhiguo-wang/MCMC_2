#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#   Program Name:   LIFP - Naive Strategy (Deposit All Actual Budget)
#   Version:        1.1
#
#   Created By:     Justin Xu
#   Modified By:    Justin Xu
#
#   Datasource:     
#   Source Tables:  \\Inputs\\
#   Target Tables:  \\Outputs\\
#
#   Purpose:        To calculate the ruin prob. under naive strategy
#
#   Version           Date           Author            change
#               |               |               |
#     1.1       |   10/16/15    |    Justin     |      First Version
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

pre_naive_ruin <- function() {

source("R Model\\Pre_CashFlow_ActualBudget.R")

source("R Model\\Pre_CashFlow_ExistingEmployee&Employer.R")

source("R Model\\Pre_CashFlow_InitialAssets&EmergencyFunds&Debt.R")

source("R Model\\Pre_CashFlow_ExpensesForDisability&Death&Retirement.R")

source("R Model\\Pre_CashFlow_Mortgage.R")

source("R Model\\Pre_CashFlow_Rent.R")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 1. Cash Inflow
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

pre_naive_cashIn <- cashIn_ActualBudget + cashIn_EmergencyFund +
  cashIn_InitialAssets + ee_cashIn_DI + ee_cashIn_HC + ee_cashIn_TL + 
  ee_cashIn_UL + ee_cashIn_WL + ee_cashIn_401k + ee_cashIn_OtherRet +
  er_cashIn_DI + er_cashIn_HC + er_cashIn_TL + er_cashIn_UL + er_cashIn_WL +
  er_cashIn_401k

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 2. Cash Outflow
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

pre_naive_cashOut <- cashOut_Death + cashOut_Disability +
  cashOut_RetirementNeeds + cashOut_Debt + cashOut_Mortgage + cashOut_Rent
  
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 3. Net Cash Flow at Retirement Age
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

pre_naive_netCash <- pre_naive_cashIn + pre_naive_cashOut

pre_financialPosition_retAge <- rep(0, ncol(pre_mcmc))

for (i in 1 : ncol(pre_mcmc)) {
  accVal <- 0
  for (j in 1 : nrow(pre_mcmc)) {
    accVal <- accVal +
      pre_naive_netCash[j, i] * (1 + depositInterest) ^ (nrow(pre_mcmc) + 1 - j)
  }
  pre_financialPosition_retAge[i] <- accVal
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 3. Ruin Probability by Amount
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

pre_ruin <- 
  abs(sum(pre_financialPosition_retAge[pre_financialPosition_retAge < 0])) / 
  sum(abs(pre_financialPosition_retAge))

return(pre_ruin)

}