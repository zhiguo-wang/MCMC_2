#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#   Program Name:   LIFP - Affordable Budget
#   Version:        1.1
#
#   Created By:     Justin Xu
#   Modified By:    Zhiguo Wang
#
#   Datasource:     
#   Source Tables:  \\Inputs\\
#   Target Tables:  \\Outputs\\
#
#   Purpose:        To estimate affordable budget
#
#   Version           Date           Author            change
#               |               |               |
#     1.1       |   11/13/15    |    Justin     |      First Version
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source(paste0(root, sourceCode,"IndCha.R"))

# Basic Living Expense equation: y = 5313.68 +  0.17933*annualIncome + 0*currentAge
# Health Care equation: y = -780.22 - 0.00000*annualIncome +  80.2086*currentAge
# Discretionary Expense equation: y = 1311.35 + 0.28817*annualIncome - 120.370*currentAge
# Insurance Expnese equation: y = -1960.28 + 0.1426*annualIncome - 73.36*currentAge
# Rent equation: y = 6748.81 +0*annualIncome -82.01*currentAge
# MortgcurrentAge equation: y = -2658.51 + 0.12*annualIncome + 0*currentAge

affordableBudget <- function() {
  
  basicLiving <- 5313.68 + 0.17933 * annualIncome
  
  healthcare <- -780.22 + 80.2086 * currentAge
  
  discretionary <- 1311.35 + 0.28817 * annualIncome - 120.370 * currentAge
  
  insurance <- -1960.28 + 0.1426 * annualIncome - 73.36 * currentAge
  
  rent <- 6748.81 - 82.01 * currentAge
  
  mort <- -2658.51 + 0.12 * annualIncome
  
  affBudget <- annualIncome * (1 - 0.25) - basicLiving - healthcare -
    discretionary - rent - mort
  
  return(affBudget)
  
}

affBudget <- affordableBudget()
