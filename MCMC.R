#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
#   Program Name:   Lifetime Individual Financial Planning Model - MC- MC Process             
#   Version:        1.1
#
#   Created By:     Justin Xu
#   Modified By:    Justin Xu
#
#   Database Used:  Annuity 2000 Basic Table, ..., etc. Minimum Age: 5 Maximum Age: 115
#   Source Tables:  \\Inputs\\MCMC
#   Target Tables:  \\Outputs\\
#
#   Purpose:        To create MC - MC matrices
#
#   Version           Date           Author            change
#               |               |               |
#     1.1       |   09/21/15    |    Justin     |      First Version
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

setwd("E:\\UCONN PhD\\Dissertation")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 1. Input Mortality and Disability Rates
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

headea_m <- read.csv("Inputs\\MCMC\\Healthy\\Annuity 2000 Basic Table - Male.csv", header = TRUE)

headea_f <- read.csv("Inputs\\MCMC\\Healthy\\Annuity 2000 Basic Table - Female.csv", header = TRUE)

headis_m <- read.csv("Inputs\\MCMC\\Healthy\\research-ltc-study-2000-11-agg-incidence-rates Male.csv", header = TRUE)

headis_f <- read.csv("Inputs\\MCMC\\Healthy\\research-ltc-study-2000-11-agg-incidence-rates Female.csv", header = TRUE)

dishea_m <- read.csv("Inputs\\MCMC\\Unhealthy\\2005 Group Term Life Waiver Study  Ultimate Probability of Recovery Following Disability Male.csv", header = TRUE)

dishea_f <- read.csv("Inputs\\MCMC\\Unhealthy\\2005 Group Term Life Waiver Study  Ultimate Probability of Recovery Following Disability Female.csv", header = TRUE)

disdea_m <- read.csv("Inputs\\MCMC\\Unhealthy\\PBGC Table Va Mortality Rates for Disabled Male.csv", header = TRUE)

disdea_f <- read.csv("Inputs\\MCMC\\Unhealthy\\PBGC Table Va Mortality Rates for Disabled Female.csv", header = TRUE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 2. Minimum Age and Maximum Age
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

minage <- max(headea_m$Age[1], headea_f$Age[1], headis_m$Age[1], headis_f$Age[1],
              dishea_m$Age[1], dishea_f$Age[1], disdea_m$Age[1], disdea_f$Age[1])

maxage <- min(max(headea_m$Age), max(headea_f$Age), max(headis_m$Age),
              max(headis_f$Age), max(dishea_m$Age), max(dishea_f$Age),
              max(disdea_m$Age), max(disdea_f$Age))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 3. Transition Matrix in Markov Chain
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 3.1 Male

for (i in minage : maxage) {
  assign(paste("male_", i, sep = ""),
         matrix(c(1 - headis_m$Incidence[i + 1 - minage] - headea_m$Mortality[i + 1 - minage],
                  dishea_m$Recovery[i + 1 - minage],
                  0,
                  headis_m$Incidence[i + 1 - minage],
                  1 - dishea_m$Recovery[i + 1 - minage] - disdea_m$Mortality[i + 1 - minage],
                  0,
                  headea_m$Mortality[i + 1 - minage],
                  disdea_m$Mortality[i + 1 - minage],
                  1), 3, 3))
}

# 3.2 Female

for (i in minage : maxage) {
  assign(paste("female_", i, sep = ""),
         matrix(c(1 - headis_f$Incidence[i + 1 - minage] - headea_f$Mortality[i + 1 - minage],
                  dishea_f$Recovery[i + 1 - minage],
                  0,
                  headis_f$Incidence[i + 1 - minage],
                  1 - dishea_f$Recovery[i + 1 - minage] - disdea_f$Mortality[i + 1 - minage],
                  0,
                  headea_f$Mortality[i + 1 - minage],
                  disdea_f$Mortality[i + 1 - minage],
                  1), 3, 3))
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 4. Monte Carlo Simulation
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Simulation Times

simtim <- 1000

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
## 4.1 Assumption 1: decay factors follows uniform distribution for each year 
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 4.1.1 Assu1: Male

for (i in minage : maxage) {
  assign(paste("umc_m_", i, sep = ""), matrix(1, maxage - i + 2, simtim))
  assign(paste("urn_m_", i, sep = ""), matrix(runif((maxage - i + 2) * simtim), 
                                               maxage - i + 2, simtim))
}

for (i in minage : maxage) {
  tempmc <- get(paste("umc_m_", i, sep = ""))
  temprn <- get(paste("urn_m_", i, sep = ""))
  for (j in 1 : simtim) {
    status <- 1
    for (k in 1 : (nrow(tempmc) - 1)) {
      temptm <- get(paste("male_", i + k - 1, sep = ""))
      if (temprn[k, j] <= temptm[status, 3]) {
        status <- 3
        tempmc[k + 1, j] <- 3
      } else if (temprn[k, j] > temptm[status, 3] & temprn[k, j] <= sum(temptm[status, 2 : 3])) {
        status <- 2
        tempmc[k + 1, j] <- 2
      }
    }
  }
  assign(paste("umcmc_M_", i, sep = ""), tempmc)
}

# 4.1.2 Assu1: Female

for (i in minage : maxage) {
  assign(paste("umc_f_", i, sep = ""), matrix(1, maxage - i + 2, simtim))
  assign(paste("urn_f_", i, sep = ""), matrix(runif((maxage - i + 2) * simtim), 
                                              maxage - i + 2, simtim))
}

for (i in minage : maxage) {
  tempmc <- get(paste("umc_f_", i, sep = ""))
  temprn <- get(paste("urn_f_", i, sep = ""))
  for (j in 1 : simtim) {
    status <- 1
    for (k in 1 : (nrow(tempmc) - 1)) {
      temptm <- get(paste("female_", i + k - 1, sep = ""))
      if (temprn[k, j] <= temptm[status, 3]) {
        status <- 3
        tempmc[k + 1, j] <- 3
      } else if (temprn[k, j] > temptm[status, 3] & temprn[k, j] <= sum(temptm[status, 2 : 3])) {
        status <- 2
        tempmc[k + 1, j] <- 2
      }
    }
  }
  assign(paste("umcmc_F_", i, sep = ""), tempmc)
}

## proc.time determines how much real and CPU time (in seconds) the currently running R process has already taken.
proc.time()

## remove unused variables
varlist = ls()

usableVarList <- grep("^umcmc*", varlist, value = FALSE)

rm(list = varlist[-usableVarList])

## save MCMC matrices as a R image
save.image(file = "Unif_MCMC.RData")