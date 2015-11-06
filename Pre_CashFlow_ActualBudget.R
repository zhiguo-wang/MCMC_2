#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#   Program Name:   LIFP - Cash Flow - Naive Strategy - Actual Budget
#   Version:        1.1
#
#   Created By:     Justin Xu
#   Modified By:    Justin Xu
#
#   Database Used:  Investment Portfolio.csv
#   Source Tables:  \\Inputs\\Financial Products
#   Target Tables:  \\Outputs\\
#
#   Purpose:        To create cash flow matrix of 401k investment portfolio
#
#   Version           Date           Author            change
#               |               |               |
#     1.1       |   10/16/15    |    Justin     |      First Version
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 1. Load MCMC Database (Unif_MCMC.RData)
#    Load Individual Characteristics (Inputs_IndCha.RData)
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
# 4. Cash Flow of Actual Budget 
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

vector_ActualBudget <- actBudget * (1 + wageInflation)^(0:(nrow(pre_mcmc)-1))

cashIn_ActualBudget <- vector_ActualBudget * (pre_mcmc == 1)

