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
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 3.2.2 Cash Outflow_LTC Living Expenses

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
retireSalary <- annualIncome * (1 + wageInflation) ^ (retireAge - currentAge)
a0 <- 5313.678641
a1 <- 0.179338079

b0 <- -780.2190476
b1 <- 80.20857143

c0 <- 1311.347392
c1 <- -120.3701846
c2 <- 0.288165909

cashout.mat.retire <- 0 * post_mcmc
cashout.mat.retire[which(post_mcmc == 1)] <- (a0 + a1 * retireSalary)+ (b0 + b1 * retireAge) + 
                                         (c0 + c1 * retireAge + c2 * retireSalary)

cashout.mat.retire[which(post_mcmc == 2)] <- ((a0 + a1 * retireSalary)+ (b0 + b1 * retireAge) + 
                                         (c0 + c1 * retireAge + c2 * retireSalary)) * 2

cashout.mat.retire <- cashout.mat.retire * as.vector((1 + livExpInflation) ^ (1 : nrow(post_mcmc)-1))

cashout.mat.retireNeeds <- (cashout.mat.retire - annualSSB) * (post_mcmc != 3)

generalDiscountFactor <- (1 + generalInflation) ^ -(1:nrow(post_mcmc))
vec_cashOut_RetirementNeeds <- -apply(generalDiscountFactor * cashout.mat.retireNeeds, 2, sum)
apv.retirementNees <- mean(vec_cashOut_RetirementNeeds[vec_cashOut_RetirementNeeds != 0])


cashOut_RetirementNeeds[nrow(pre_mcmc), ] <- apv.retirementNees
