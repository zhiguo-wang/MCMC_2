#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
#   Program Name:   LIFP - individual characteristics
#   Version:        1.1
#
#   Created By:     Justin Xu
#   Modified By:    Zhiguo Wang
#
#   Database Used:  Individual Information
#   Source Tables:  \\Inputs\\IndCha
#   Target Tables:  \\Outputs\\
#
#   Purpose:        To input individual characteristics
#
#   Version           Date           Author            change
#               |               |               |
#     1.1       |   09/21/15    |    Justin     |      First Version
#     1.101     |   10/22/15    |    Zhiguo     |      1. Method to read file (paste0);
#                                                      2. Remove useless variable (rm);
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Input Individual Characteristics
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

IndCha <- read.csv(paste0(root,"Inputs\\IndCha\\IndCha.csv"), header = TRUE)

currentAge <- IndCha$Current.Age

#gender <- IndCha$Gender

if(IndCha$Gender == "M" | IndCha$Gender == "m") {
  gender <- "M"
} else {
  gender <- "F"
}

state <- IndCha$State

annualIncome <- IndCha$Annual.Income

maritalStatus <- IndCha$Marital.Status

spouseIncome <- IndCha$Spouse.Income

monMortPay <- IndCha$Monthly.Mortgage.Payment

monMortPeriod <- IndCha$Remaining.Mortgage.Period..Month.

monRentPay <- IndCha$Monthly.Rent.Payment

debt <- IndCha$Unsecured.Debt

breadWinner <- IndCha$Breadwinner

retireAge <- IndCha$Retirement.Age

currentSavingBalance <- IndCha$Current.Saving.Balance..for.retirement.

emergencyFund <- IndCha$Emergency.Fund

eeHealth <- IndCha$EE...Healthcare.Expenses

eeWL <- IndCha$EE...Whole.Life

eeTL <- IndCha$EE...Term.Life

eeDI <- IndCha$EE...Annual.Disability.Income

eeUL <- IndCha$EE...Universal.Life

ee401k <- IndCha$EE.....Retirement.Contributions..401k.

eeRetire <- IndCha$EE.....Retirement.Contributions..after.tax.cost.

erHealth <- IndCha$ER...Healthcare.Expenses

erWL <- IndCha$ER...Whole.Life

erTL <- IndCha$ER...Term.Life

erDI <- IndCha$ER...Annual.Disability.Income

erUL <- IndCha$ER...Universal.Life

er401k <- IndCha$ER.....Retirement.Contributions..401k.

repSurvivorIncome <- IndCha$X..Replacement.for.Survivor.Income..whole.life.insurance.

for (i in 1 : 20) {
  nam <- paste("dependentAge", i, sep = ".")
  assign(nam, IndCha[1, 30 + i])
}

for (i in 1 : 20) {
  nam <- paste("dependentGender", i, sep = ".")
  assign(nam, levels(factor(IndCha[1, 50 + i])))
}

actBudget <- IndCha$Actual.Budget..not.including.costs.of.existing.coverage.

# remove useless variable, release memory
rm(IndCha)