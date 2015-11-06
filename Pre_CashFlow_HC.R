#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#   Program Name:   LIFP - Cash Flow - Healthcare Expenses
#   Version:        1.1
#
#   Created By:     Justin Xu
#   Modified By:    Justin Xu
#
#   Regression:     ExpensesAnalysis.R - Healthcare Expenses Regression (model2_2)
#   Source Tables:  \\Inputs\\Expenses
#   Target Tables:  \\Outputs\\
#
#   Purpose:        To create cash flow matrix of healthcare expenses
#
#   Version           Date           Author            change
#               |               |               |
#     1.1       |   10/16/15    |    Justin     |      First Version
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 1. Load MCMC Database (Unif_MCMC.RData) & Healthcare Expenses Regression
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source(paste0(root, sourceCode, "ExpensesAnalysis.R"))

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
# 4. Cash Flow of Healthcare Expenses 
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
vector_Healthcare <- (summary(model2_2)$coefficients["age", "Estimate"] * (currentAge + (1 : nrow(pre_mcmc)) - 1) +
                    summary(model2_2)$coefficients["(Intercept)", "Estimate"]) *
                    (1 + healthcareInflation) ^ (1 : nrow(pre_mcmc) -1)

cashOut_Healthcare <- -vector_Healthcare * (pre_mcmc == 1)
