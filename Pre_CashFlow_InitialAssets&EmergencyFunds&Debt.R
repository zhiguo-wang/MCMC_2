#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
#   Program Name:   LIFP - Initial Assets & Emergency Funds & Unsecured Debt
#   Version:        1.1
#
#   Created By:     Justin Xu
#   Modified By:    Justin Xu
#
#   Database Used:  IndCha.csv
#   Source Tables:  \\Inputs\\
#   Target Tables:  \\Outputs\\
#
#   Purpose:        To create cash flow matrix of the above three
#
#   Version           Date           Author            change
#               |               |               |
#     1.1       |   10/16/15    |    Justin     |      First Version
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 4. Initial Assets
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cashIn_InitialAssets <- matrix(0, nrow(pre_mcmc), ncol(pre_mcmc))

cashIn_InitialAssets[1, ] <- currentSavingBalance

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 5. Emergency Fund
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cashIn_EmergencyFund <- (mat.disable + mat.death) * emergencyFund

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 6. Unsecured Debt
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cashOut_Debt <- mat.death * -debt
