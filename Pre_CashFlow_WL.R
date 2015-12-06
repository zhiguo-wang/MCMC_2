#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
#   Program Name:   LIFP - Cash Flow - Whole Life Insurance           
#   Version:        1.1
#
#   Created By:     Justin Xu
#   Modified By:    Justin Xu
#
#   Database Used:  Whole Life.csv
#   Source Tables:  \\Inputs\\Financial Products
#   Target Tables:  \\Outputs\\
#
#   Purpose:        To create cash flow matrix of whole life insurance
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

rate_WL <- read.csv("Inputs\\Financial Products\\Whole Life.csv", header = TRUE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 4. Cash Flows Related to Whole Life Insurance 
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Cash Inflow: Death Benefits

## Unit Benefit is 10,000 

cashIn_WL <- 10000 * mat.death

## Cash Outflow: Premium

if (gender == "M") {
  rate_WL <- rate_WL[which(rate_WL[ , 1] == currentAge), 2] 
} else {
  rate_WL <- rate_WL[which(rate_WL[ , 1] == currentAge), 3]
}
