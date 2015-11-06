#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#   Program Name:   LIFP - Cash Flow - Rent
#   Version:        1.1
#
#   Created By:     Justin Xu
#   Modified By:    Justin Xu
#
#   Datasource:     Individual Characteristics
#   Source Tables:  \\Inputs\\IndCha
#   Target Tables:  \\Outputs\\
#
#   Purpose:        To create cash flow matrix of rent
#
#   Version           Date           Author            change
#               |               |               |
#     1.1       |   10/16/15    |    Justin     |      First Version
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 1. Load MCMC Database (Unif_MCMC.RData)
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
# 3. Divide The MCMC Matrix into pre_mcmc and post_mcmc
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 4. Cash Flows of Rent 
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## When healthy, before actual budget, rent is already covered

cashOut_Rent <- - monRentPay * 12 * (pre_mcmc == 2)

