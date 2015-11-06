#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
#   Program Name:   LIFP - Cash Flow - Disability Insurance           
#   Version:        1.1
#
#   Created By:     Justin Xu
#   Modified By:    Justin Xu
#
#   Database Used:  Disability Insurance.csv
#   Source Tables:  \\Inputs\\Financial Products
#   Target Tables:  \\Outputs\\
#
#   Purpose:        To create cash flow matrix of disability insurance
#
#   Version           Date           Author            change
#               |               |               |
#     1.1       |   10/16/15    |    Justin     |      First Version
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 1. Input Whole Life Insurance Rate, Load MCMC Database (Unif_MCMC.RData)
#    And Load Individual Characteristics (Inputs_IndCha.RData)
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rate_DI <- read.csv("Inputs\\Financial Products\\Disability Insurance.csv", header = TRUE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 4. Cash Flows Related to Disability Insurance 
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Cash Inflow: Disability Benefits

## Unit Benefit is 10,000 

cashIn_DI <- 10000 * (pre_mcmc == 2)

## Cash Outflow: Premium

ad_rate_DI <- rate_DI[1 : (nrow(pre_mcmc)) - 1 + which(rate_DI[ , 1] == currentAge), 2]

cashOut_DI <- -ad_rate_DI * (pre_mcmc == 1)

