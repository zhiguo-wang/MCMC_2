#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#   Program Name:   LIFP - Cash Flow - Social Security Benefits
#   Version:        1.1
#
#   Created By:     Justin Xu
#   Modified By:    Justin Xu
#
#   Datasource:     Individual Characteristics
#   Source Tables:  \\Inputs\\IndCha
#   Target Tables:  \\Outputs\\
#
#   Purpose:        To create cash flow matrix of SSB
#
#   Version           Date           Author            change
#               |               |               |
#     1.1       |   11/04/15    |    Justin     |      First Version
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 1. Social Security Primary Insurance Amount (PIA) Tiers
## Data source: MM and Employer Inputs - Social Security Inputs

PIAtiers <- c(0.90, 0.32, 0.15)

PIAtiresAmt <- c(816, 4101)

## 2. Monthly Income

monIncome <- annualIncome / 12

## 3. PIA

if (monIncome <= PIAtiresAmt[1]) {
  monPIA <- monIncome * PIAtiers[1]
} else if (monIncome <= sum(PIAtiresAmt) & monIncome > PIAtiresAmt[1]) {
  monPIA <- PIAtiresAmt[1] * PIAtiers[1] + (monIncome - PIAtiresAmt[1]) * PIAtiers[2]
} else {
  monPIA <- PIAtiresAmt[1] * PIAtiers[1] + PIAtiresAmt[2] * PIAtiers[2] +
    (monIncome - sum(PIAtiresAmt)) * PIAtiers[3]
}

annualPIA <- monPIA * 12

## 4. Early or late retirement factors
## SSB normal retirement age for most users: 67

if (retireAge < 67) {
  if (67 - retireAge <= 3) {
    adjFactor <- (67 - retireAge) * (12 * 5 / 900)
  } else {
    adjFactor <- 3 * (12 * 5 / 900) + (67 - retireAge - 3) * (12 * 5 / 1200)
  }
} else if (retireAge == 67) {
  adjFactor <- 0
} else {
  adjFactor <- - (retireAge - 67) * (12 * 16 / 2400)
}

## 5. Adjusted annual SSB

annualSSB <- annualPIA * (1 - adjFactor) * (1 + wageInflation) ^ (retireAge - currentAge)