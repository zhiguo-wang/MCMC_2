#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
#   Program Name:   LIFP - Economic Assumptions
#   Version:        1.1
#
#   Created By:     Justin Xu
#   Modified By:    Justin Xu
#
#   Database Used:  Economic Assumptions.csv
#   Source Tables:  \\Inputs\\Economic Assumptions
#   Target Tables:  \\Outputs\\
#
#   Purpose:        To input individual characteristics
#
#   Version           Date           Author            change
#               |               |               |
#     1.1       |   10/17/15    |    Justin     |      First Version
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Input Individual Characteristics
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ecoAss <- read.csv(paste0(root,"Inputs\\Economic Assumptions\\Economic Assumptions.csv"), header = TRUE)

wageInflation <- ecoAss$Wage.inflation.rate

generalInflation <- ecoAss$General.inflation.rate

depositInterest <- ecoAss$Deposit.interest.rate

rentInflation <- ecoAss$Rent.inflation

mortgageInterest <- ecoAss$Mortgage.interest.rate

livExpInflation <- ecoAss$Living.expenses.inflation

healthcareInflation <- ecoAss$Healthcare.expenses.inflation

# Remove useless variable, release memory
rm(ecoAss)