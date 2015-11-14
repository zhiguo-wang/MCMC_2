#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
#   Program Name:   LIFP - Cash Flow - Expenses for Dis., Dea. and Ret. Needs          
#   Version:        1.1
#
#   Created By:     Justin Xu
#   Modified By:    Justin Xu
#
#   Database Used:  IndCha.csv & EcoAss.csv
#   Source Tables:  \\Inputs\\
#   Target Tables:  \\Outputs\\
#
#   Purpose:        To create cash flow matrix of expenses
#
#   Version           Date           Author            change
#               |               |               |
#     1.1       |   10/18/15    |    Justin     |      First Version
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 2. Locate The Appropriate MCMC Matrix for The Individual
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


retireHealth <- mcmc[nrow(pre_mcmc) + 1, ]


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 4. Expenses for Disability 
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Assumption of replacement of income when disability happens
## If breadWinner = Y, % replacement of income = income / (income + spouse income) * 0.75
## If breadWinner = N, % replacement of income = income / (income + spouse income) * 0.50

if (breadWinner == "Y") {
  repIncome <- annualIncome / (annualIncome + spouseIncome) * 0.75
} else {
  repIncome <- annualIncome / (annualIncome + spouseIncome) * 0.5
}

## income matrix for pre

pre_vec_income <- annualIncome * (1 + wageInflation) ^ (1:preYears -1)


pre_mat_income <- replicate(ncol(pre_mcmc), pre_vec_income)

cashOut_Disability <- -pre_vec_income * (pre_mcmc == 2) * repIncome


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 5. Expenses for Death
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



burialCost <- 25000

cashOut_Death <- -(pre_mat_income * repSurvivorIncome + burialCost) * mat.death


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 6. Financial Needs for Retirement
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cashOut_RetirementNeeds <- matrix(0, nrow(pre_mcmc), ncol(pre_mcmc))

## income matrix for post


post_vec_income <- annualIncome * (1 + wageInflation) ^ ((preYears+1) : nrow(mcmc))

post_mat_income <- replicate(ncol(post_mcmc), post_vec_income)

post_mat_income[nrow(post_mcmc), ] <- 0

## Assumption of Retirement Financial Needs: Substitution Rate of Wage = 0.7
## Assumption of LTC Needs: Substitution Rate of Wage = 0.7 * 1.5 = 1.05
post_cashOut_RetirementNeeds <- -post_vec_income * 0.7 * ((post_mcmc == 1) + (post_mcmc == 2) * 1.5)
post_cashOut_RetirementNeeds <- (post_cashOut_RetirementNeeds + annualSSB) * (post_mcmc != 3)

generalDiscountFactor <- (1 + generalInflation) ^ (-(1:nrow(post_mcmc))+1)
vec_cashOut_RetirementNeeds <- apply(generalDiscountFactor * post_cashOut_RetirementNeeds, 2, sum)

cashOut_RetirementNeeds[nrow(pre_mcmc), ] <- vec_cashOut_RetirementNeeds
