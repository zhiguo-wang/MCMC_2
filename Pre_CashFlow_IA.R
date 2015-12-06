#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#   Program Name:   LIFP - Cash Flow - Initial Assets
#   Version:        1.1
#
#   Created By:     Justin Xu
#   Modified By:    Justin Xu
#
#   Datasource:     Individual Characteristics
#   Source Tables:  \\Inputs\\IndCha
#   Target Tables:  \\Outputs\\
#
#   Purpose:        To create cash flow matrix of initial Assets
#
#   Version           Date           Author            change
#               |               |               |
#     1.1       |   10/16/15    |    Justin     |      First Version
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## Initial Assets = 31732.9152 * currentAge + 4.457102914 * annualIncome - 1163603.199
## Data source: NRSI Database_Census Data_2012 Assumption

cashIn_IA <- max(31732.9152 * currentAge + 4.457102914 * annualIncome - 1163603.199, 0)

