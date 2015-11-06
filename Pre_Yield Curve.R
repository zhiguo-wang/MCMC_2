#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#   Program Name:   LIFP - Cash Flow - Yield Curve (401k)
#   Version:        1.1
#
#   Created By:     Justin Xu
#   Modified By:    Justin Xu
#
#   Database Used:  Investment Portfolio.csv
#   Source Tables:  \\Inputs\\Financial Products
#   Target Tables:  \\Outputs\\
#
#   Purpose:        To generate 3 groups of stochastic yield curve
#
#   Version           Date           Author            change
#               |               |               |
#     1.1       |   10/16/15    |    Justin     |      First Version
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 1. Input Investment Portfolio
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

investPort <- read.csv("Inputs\\Financial Products\\Investment Portfolio.csv", header = TRUE)

source("R Model\\IndCha.R")

dataBase <- load("Unif_MCMC.RData")

mcmc <- get(paste("umcmc_", gender, "_", currentAge, sep = ""))

rm(list = dataBase)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 2. Stochastic Yield Curve (Assumption: Short-term Rate ~ Lognormal)
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

for (i in 1 : 116) {

## Low Risk

mu_Low <- investPort$Low.Risk[1]

sigma_Low <- investPort$Low.Risk[2]

assign(paste("yield_Low_", i, sep = ""),
       matrix((rlnorm(i * ncol(mcmc), meanlog = mu_Low, sdlog = sigma_Low)),
              i, ncol(mcmc)))

## Moderate Risk

mu_Mid <- investPort$Moderate[1]

sigma_Mid <- investPort$Moderate[2]

assign(paste("yield_Mid_", i, sep = ""),
       matrix((rlnorm(i * ncol(mcmc), meanlog = mu_Mid, sdlog = sigma_Mid)),
              i, ncol(mcmc)))

## Aggressive Risk

mu_Agg <- investPort$Aggressive[1]

sigma_Agg <- investPort$Aggressive[2]

assign(paste("yield_Agg_", i, sep = ""),
       matrix((rlnorm(i * ncol(mcmc), meanlog = mu_Agg, sdlog = sigma_Agg)),
              i, ncol(mcmc)))

}

## remove unused variables

varlist = ls()

usableVarList <- grep("yield*", varlist, value = FALSE)

rm(list = varlist[-usableVarList])

## save MCMC matrices as a R image

save.image(file = "Pre_YieldCurves.RData")

