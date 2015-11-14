#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#   Program Name:   LIFP - Ideal Budget & Allocation
#   Version:        1.1
#
#   Created By:     Justin Xu
#   Modified By:    Zhiguo Wang
#
#   Datasource:     
#   Source Tables:  \\Inputs\\
#   Target Tables:  \\Outputs\\
#
#   Purpose:        Establish a benchmark to ensure an individual has a high
#                   prob. of being financial sustainable
#
#   Version           Date           Author            change
#               |               |               |
#     1.1       |   11/02/15    |    Justin     |      First Version
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Ideal Allocation for Pre
## 1. Healthcare Expenses 2. Term Life 3. Whole Life 4. Disability Insurance 5. 401k

preIdeal_allocation <- c(0, 0, 0, 0, 0)

#++++++++++++++++++++++++++
# 1. Healthcare Insurance
#++++++++++++++++++++++++++

preIdeal_allocation[1] <- - cashOut_Healthcare[1]

#++++++++++++++++++++++++++
# 2. Term Life Insurance
#++++++++++++++++++++++++++

## Term Life Insurance will cover 3 parts: mortgage and debt, colledge tuition, survivor income

## 2.1 Coverage for mortgage and debt

mort_vec <- rep(monMortPay, monMortPeriod)

mortBalance <- sum(mort_vec * (1 + ((1 + mortgageInterest) ^ (1 / 12) - 1)) ^
                     -(0 : (monMortPeriod - 1)))

## 2.2 Coverage for colledge tuition

EFC <- read.csv(paste0(root,"Inputs\\Expected Family Contribution\\EFC.csv"), header = TRUE)

eduCost <- EFC[which(abs(annualIncome + spouseIncome - EFC$AGI) ==
                       min(abs(annualIncome + spouseIncome - EFC$AGI)))[1],
               min(numDpt + 1, ncol(EFC))]

totalEduCost <- eduCost * 4 * numDpt

## 2.3 Coverage for survivor income
## pre_mat_income and repSurvivorIncome come from Pre_CashFlow_ExpensesForDisability&Death&Retirement

## find out the worst senario
survIncome <- max(pre_mat_income * repSurvivorIncome * mat.death)

preIdeal_allocation[2] <- (mortBalance + totalEduCost + survIncome) / 10000 * (- cashOut_TL[1])

#++++++++++++++++++++++++++
# 3. Whole Life Insurance
#++++++++++++++++++++++++++

## Whole Life Insurance will cover 2 parts: temporary living needs, burial cost

tempLive <- max(pre_mat_income * (1 - repSurvivorIncome) * mat.death)

preIdeal_allocation[3] <- (burialCost + tempLive) / 10000 * (- cashOut_WL[1])

#++++++++++++++++++++++++++
# 4. Disability Insurance
#++++++++++++++++++++++++++

preIdeal_allocation[4] <- max(- cashOut_Disability) / 10000 * (- cashOut_DI[1])

#++++++++++++++++++++++++++
# 5. 401K
#++++++++++++++++++++++++++

endingAssets <- 0

unitEnding401k <- function() {
  
  vec_401k <- (1 + wageInflation) ^ (1 : preYears -1)
  
  mat_401k <- vec_401k * (pre_mcmc == 1)

  
  ### Low Risk
  
  pre_cashIn_401k_Low <- getAccValue(mat_401k * perLow, yield_Low, mat.death)
  
  
  ### Moderate Risk
  
  pre_cashIn_401k_Mid <- getAccValue(mat_401k * perMid, yield_Mid, mat.death)
  
  
  ### Aggressive Risk
  
  pre_cashIn_401k_Agg <- getAccValue(mat_401k * perAgg, yield_Agg, mat.death)
  
  
  ## Ideal 401k
  
  pre_cashIn_401k <- pre_cashIn_401k_Low + pre_cashIn_401k_Mid + pre_cashIn_401k_Agg
  
  endingAssets <- mean(pre_cashIn_401k[pre_cashIn_401k != 0])

  return(endingAssets)
  
}

preIdeal_allocation[5] <- max(- cashOut_RetirementNeeds[nrow(cashOut_RetirementNeeds), ]) / unitEnding401k()