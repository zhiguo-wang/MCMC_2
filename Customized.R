#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#   Program Name:   LIFP - Customized Allocation
#   Version:        1.2
#
#   Created By:     Justin Xu
#   Modified By:    Zhiguo Wang
#
#   Datasource:     
#   Source Tables:  \\Inputs\\
#   Target Tables:  \\Outputs\\
#
#   Purpose:        To calculate the ruin prob. under customized allocation
#
#   Version           Date           Author            change
#               |               |               |
#     1.1       |   10/16/15    |    Justin     |      First Version
#     1.2       |   10/22/15    |   Zhiguo      |   1. Define more constants used through entire model
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Load Functions
source(paste0(root, sourceCode,"functions.R"))
source(paste0(root, sourceCode,"EcoAss.R"))

preYears <- retireAge - currentAge 

mcmc <- getLifePath(currentAge, gender)
pre_mcmc <- mcmc[1 : preYears, ]
post_mcmc <- mcmc[(retireAge - currentAge + 1) : nrow(mcmc), ]

mat.death <- getMatrixAtDeath(pre_mcmc)
mat.disable <- getMatrixAtDisable(pre_mcmc)


# retrive Yield Curves
# getPreYieldCurves(preYears) --- Defined in <Function.R>
yieldCurves <- getPreYieldCurves(retireAge - currentAge)

yield_Low <- yieldCurves$Low
yield_Mid <- yieldCurves$Mid
yield_Agg <- yieldCurves$Agg
rm(yieldCurves)

# calculate SSB 
# function <getSSB()> is defined in Function.R
annualSSB <- getSSB()


# Weight of saving investment (Low risk, Mid risk, Aggressive risk);
# Used when calculating 401k, retirement saving, ..
perAgg <- (retireAge - currentAge) / retireAge
## % on Moderate Risk = (1 - % on Aggressive Risk) / 2
perMid <- (1 - perAgg) / 2
## % on Low Risk = (1 - % on Aggressive Risk) / 2
perLow <- (1 - perAgg) / 2

##########################################################################################

source(paste0(root, sourceCode, "Pre_CashFlow_ActualBudget.R"))

source(paste0(root, sourceCode, "Pre_CashFlow_ExistingEmployee&Employer.R"))

source(paste0(root, sourceCode, "Pre_CashFlow_InitialAssets&EmergencyFunds&Debt.R"))

source(paste0(root, sourceCode, "Pre_CashFlow_ExpensesForDisability&Death&Retirement.R"))

source(paste0(root, sourceCode, "Pre_CashFlow_HC.R"))

source(paste0(root, sourceCode, "Pre_CashFlow_TL.R"))

source(paste0(root, sourceCode, "Pre_CashFlow_WL.R"))

source(paste0(root, sourceCode, "Pre_CashFlow_DI.R"))

source(paste0(root, sourceCode, "Pre_CashFlow_Mortgage.R"))

source(paste0(root, sourceCode, "Pre_CashFlow_Rent.R"))
source(paste0(root, sourceCode, "MarginalApproach.R"))
source(paste0(root, sourceCode, "MarginalApproach_Hierarchy.R"))
source(paste0(root, sourceCode, "Pre_CashFlow_IA.R"))


pre_customized_ruin <- function(pre_allocation) {

## Test pre_allocation by actual budget

#if (sum(pre_allocation) > actBudget) {
#  stop("Customized Allocation Exceeds Actual Budget!")
#}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 1. Cash Inflow
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 1.1 Healthcare Expenses (Credit) (pre_allocation[1]: Healthcare Expenses)

vecNet_Healthcare <- rep(0, nrow(pre_mcmc))

for (i in 1 : nrow(pre_mcmc)) {
  vecNet_Healthcare[i] <-
    (pre_allocation[1] + cashOut_Healthcare[1]) * 
    (1 + healthcareInflation) ^ (i - 1)
}

cashNet_Healthcare <- replicate(ncol(pre_mcmc), vecNet_Healthcare)

cashNet_Healthcare[which(pre_mcmc != 1)] <- 0




## 1.2 Term Life Insurance (pre_allocation[2]: TL premium)

pre_cashIn_TL <- abs(pre_allocation[2] / cashOut_TL[1]) * cashIn_TL

## 1.3 Whole Life Insurance (pre_allocation[3]: WL premium)

pre_cashIn_WL <- abs(pre_allocation[3] / rate_WL) * cashIn_WL

## 1.4 Disability Insurance (pre_allocation[4]: DI premium)

pre_cashIn_DI <- abs(pre_allocation[4] / cashOut_DI[1]) * cashIn_DI

## 1.5 Retirement Plan (401k) (Based on Pre_CashFlow_ExistingEmployee&Employer.R)

### Income Matrix

vec_cus401k <- pre_allocation[5] * (1 + wageInflation) ^ (1 : preYears -1)
mat_cus401k <- vec_cus401k * (pre_mcmc == 1)

### Low Risk

pre_cashIn_401k_Low <- matrix(0, nrow(pre_mcmc), ncol(pre_mcmc))
pre_cashIn_401k_Low <- getAccValue(mat_cus401k * perLow, yield_Low, mat.death)


### Moderate Risk

pre_cashIn_401k_Mid <- matrix(0, nrow(pre_mcmc), ncol(pre_mcmc))
pre_cashIn_401k_Mid <- getAccValue(mat_cus401k * perMid, yield_Mid, mat.death)


### Aggressive Risk

pre_cashIn_401k_Agg <- matrix(0, nrow(pre_mcmc), ncol(pre_mcmc))
pre_cashIn_401k_Agg <- getAccValue(mat_cus401k * perAgg, yield_Agg, mat.death)


## Customized 401k

pre_cashIn_401k <- pre_cashIn_401k_Low + pre_cashIn_401k_Mid + pre_cashIn_401k_Agg

## 1.6 Total Cusomized Cash Inflow

pre_customized_cashIn <- cashIn_EmergencyFund + cashIn_InitialAssets +
  pre_cashIn_401k +
  cashNet_Healthcare + pre_cashIn_TL + pre_cashIn_WL + pre_cashIn_DI +
  ee_cashIn_DI + ee_cashIn_HC + ee_cashIn_TL + 
  ee_cashIn_UL + ee_cashIn_WL + ee_cashIn_401k + ee_cashIn_OtherRet +
  er_cashIn_DI + er_cashIn_HC + er_cashIn_TL + er_cashIn_UL + er_cashIn_WL +
  er_cashIn_401k
pre_customized_cashIn[1,] <- pre_customized_cashIn[1, ] + cashIn_IA
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 2. Cash Outflow
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

pre_customized_cashOut <- cashOut_Death + cashOut_Disability +
  cashOut_RetirementNeeds + cashOut_Debt + cashOut_Mortgage + cashOut_Rent

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 3. Net Cash Flow at Retirement Age
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

pre_customized_netCash <- pre_customized_cashIn + pre_customized_cashOut

pre_cusFinancialPosition_retAge <- rep(0, ncol(pre_mcmc))

for (i in 1 : ncol(pre_mcmc)) {
  accVal <- 0
  for (j in 1 : nrow(pre_mcmc)) {
    accVal <- accVal +
      pre_customized_netCash[j, i] * (1 + generalInflation) ^ (nrow(pre_mcmc) + 1 - j)
  }
  pre_cusFinancialPosition_retAge[i] <- accVal
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 4. Ruin Probability by Amount
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

pre_customized_ruin <- 
  abs(sum(pre_cusFinancialPosition_retAge[pre_cusFinancialPosition_retAge < 0])) / 
  sum(abs(pre_cusFinancialPosition_retAge))

#update ending asset used for post model

endNetAssetVec <<- pre_cusFinancialPosition_retAge
return(pre_customized_ruin)

}

## proc.time determines how much real and CPU time (in seconds) the currently running R process has already taken.
proc.time()