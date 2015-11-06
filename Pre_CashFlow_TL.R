#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
#   Program Name:   LIFP - Cash Flow - Term Insurance           
#   Version:        1.1
#
#   Created By:     Justin Xu
#   Modified By:    Justin Xu
#
#   Database Used:  Term Life.csv
#   Source Tables:  \\Inputs\\Financial Products
#   Target Tables:  \\Outputs\\
#
#   Purpose:        To create cash flow matrix of term life insurance
#
#   Version           Date           Author            change
#               |               |               |
#     1.1       |   10/13/15    |    Justin     |      First Version
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rate_TL <- read.csv("Inputs\\Financial Products\\Term Life.csv", header = TRUE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 4. Cash Flows Related to Term Life Insurance 
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Cash Inflow: Death Benefits

## Unit Benefit is 10,000 

cashIn_TL <- 10000 * mat.death

## Cash Outflow: Premium

ad_rate_TL <- rate_TL[1 : preYears + currentAge, 2 + 1 * (gender == "F")]

cashOut_TL <- -ad_rate_TL * (pre_mcmc == 1)
