#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#   Program Name:   LIFP - Cash Flow - Existing Coverage from Employee
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
# 1. Source Cash Flow R files (TL, WL, DI, HC, UI, 401K, RT after Tax)
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
# 4. Term Life Insurance 
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Cash Inflow: Death Benefits


ee_cashIn_TL <- mat.death * eeTL
er_cashIn_TL <- mat.death * erTL



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 5. Whole Life Insurance 
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Cash Inflow: Death Benefits

ee_cashIn_WL <- mat.death * eeWL

er_cashIn_WL <- mat.death * erWL


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 6. Disability Insurance 
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Cash Inflow: Disability Benefits

ee_cashIn_DI <- eeDI * (pre_mcmc == 2)
er_cashIn_DI <- erDI * (pre_mcmc == 2)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 7. Healthcare Expenses 
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Cash Inflow: Healthcare Expenses (debit the estimated healthcare expenses)

ee_cashIn_HC <- eeHealth * (pre_mcmc == 1)

er_cashIn_HC <- erHealth * (pre_mcmc == 1)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 8. Retirement (401k) 
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Cash Inflow:
## Assumption of 401k investment allocation:

## % on Aggressive Risk = (retirement age - current age) / retirement age

perAgg <- (retireAge - currentAge) / retireAge

## % on Moderate Risk = (1 - % on Aggressive Risk) / 2

perMid <- (1 - perAgg) / 2

## % on Low Risk = (1 - % on Aggressive Risk) / 2

perLow <- (1 - perAgg) / 2

### Income Matrix

vec_income <- annualIncome * ((1 + wageInflation) ^ (0:(nrow(pre_mcmc) -1)))
mat_income <- vec_income * (pre_mcmc == 1)


### Low Risk ----------------------------------------------------------------------------
ee_cashIn_401k_Low <- matrix(0, nrow(pre_mcmc), ncol(pre_mcmc))
er_cashIn_401k_Low <- matrix(0, nrow(pre_mcmc), ncol(pre_mcmc))
ee_cashIn_OtherRet_Low <- matrix(0, nrow(pre_mcmc), ncol(pre_mcmc))
    # Function <getAccValue(mat.CF, mat.r)> is defined in <Function.R>
ee_cashIn_401k_Low <- getAccValue(mat_income * ee401k * perLow, yield_Low, mat.death)
er_cashIn_401k_Low<- getAccValue(mat_income * er401k * perLow, yield_Low, mat.death)
ee_cashIn_OtherRet_Low <- getAccValue(mat_income * eeRetire * perLow, yield_Low, mat.death)

### Moderate Risk -----------------------------------------------------------------------
ee_cashIn_401k_Mid <- matrix(0, nrow(pre_mcmc), ncol(pre_mcmc))
er_cashIn_401k_Mid <- matrix(0, nrow(pre_mcmc), ncol(pre_mcmc))
ee_cashIn_OtherRet_Mid <- matrix(0, nrow(pre_mcmc), ncol(pre_mcmc))
    # Function <getAccValue(mat.CF, mat.r)> is defined in <Function.R>
ee_cashIn_401k_Mid <- getAccValue(mat_income * ee401k * perMid, yield_Mid, mat.death)
er_cashIn_401k_Mid <- getAccValue(mat_income * er401k * perMid, yield_Mid, mat.death)
ee_cashIn_OtherRet_Mid <- getAccValue(mat_income * eeRetire * perMid, yield_Mid, mat.death)

### Aggressive Risk ---------------------------------------------------------------------
ee_cashIn_401k_Agg <- matrix(0, nrow(pre_mcmc), ncol(pre_mcmc))
er_cashIn_401k_Agg <- matrix(0, nrow(pre_mcmc), ncol(pre_mcmc))
ee_cashIn_OtherRet_Agg <- matrix(0, nrow(pre_mcmc), ncol(pre_mcmc))
    # Function <getAccValue(mat.CF, mat.r)> is defined in <Function.R>
ee_cashIn_401k_Agg <- getAccValue(mat_income * ee401k * perAgg, yield_Agg, mat.death)
er_cashIn_401k_Agg <- getAccValue(mat_income * er401k * perAgg, yield_Agg, mat.death)
ee_cashIn_OtherRet_Agg <- getAccValue(mat_income * eeRetire * perAgg, yield_Agg, mat.death)


## Employee and Employer 401k

ee_cashIn_401k <- ee_cashIn_401k_Low + ee_cashIn_401k_Mid + ee_cashIn_401k_Agg
er_cashIn_401k <- er_cashIn_401k_Low + er_cashIn_401k_Mid + er_cashIn_401k_Agg

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 9. Retirement (Other Investments)
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Tax Analysis

tax <- 0.25

ee_cashIn_OtherRet <-  (1 - tax) * (ee_cashIn_OtherRet_Low 
                                    + ee_cashIn_OtherRet_Mid 
                                    + ee_cashIn_OtherRet_Agg)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 10. Universal Life Insurance
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Cash Inflow: Death Benefits

ee_cashIn_UL <- eeUL * mat.death

er_cashIn_UL <- erUL * mat.death

