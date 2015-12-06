
cer_ann_due <- getCertainAnnuityDue()


# Living expense regression coefficients
a0 <- 5313.678641
a1 <- 0.179338079

b0 <- -780.2190476
b1 <- 80.20857143

c0 <- 1311.347392
c1 <- -120.3701846
c2 <- 0.288165909

st <- 1000
Burial_cost <- 25000
r_spia <- read.csv(paste0(root, "inputs\\SPIArate.csv"))



MaxAge = 115
currentAge
annualIncome
gender
retireAge
life_ept <- sum(post_mcmc != 3)/(length(posCol))
retireSalary <- annualIncome * (1 + wageInflation) ^ (retireAge - currentAge)
annualSSB
period <- nrow(post_mcmc)



# Stochastic Yield Curve for post-retirement
yieldCurves <- getPreYieldCurves(MaxAge - retireAge + 2)

YCP_L <- yieldCurves$Low
YCP_M <- yieldCurves$Mid
YCP_A <- yieldCurves$Agg
rm(yieldCurves)

# living expenses inflation vector

lev <- (1 + livExpInflation) ^ (1 : period)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 1.8 Financial Products

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# whole life insurance premium rate w_rate


    w_rate <- rate_WL




#phycon <- getPhycon(Gender,AgeRe,MaxAge,st)
phycon <- post_mcmc
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


LTCb <- ((a0 + a1 * annualIncome)+ (c0 + c1 * retireAge + c2 * annualIncome)) * 0.5

# percentage for LTC
# http://www.ownyourfuturetexas.org/long-term-care-insurance/sample-premium-rates/
# http://www.tdi.texas.gov/pubs/consumer/lrgpolicy3.html

# LTC benefit period

#ratio = annual benefits / annual premium
ratio_LTC <- ((100 + 50) * 365) / 4354
ratio_LTC <- 30                                                                     #### temporary setup, need further discussion
# LTC annual premium
pre_LTC <- LTCb / ratio_LTC
# LTC single premium
# use certain annuity to represent healthy life annuity
sig_pre_LTC <- pre_LTC * cer_ann_due[round(length(which(phycon == 1)) / 1000)]      


    
unit <- 0.1

####  Ideal case  ######
#    FinSin <-  MP[j, idealAssetID] * (1 + APVstd[which(APVstd[,1]==AgeRe), ifelse(Gender=="M",2,3)] * 1.64)
FinSin <-  endAveAsset    
#**********************#
     
#whole life benefit is adjusted based on the premium given by pre-model
w_prem <- pre_allocation[3]           	# whole life insurance annual premium    
w_bene <- w_prem / rate_WL * 10000
# Perm insurance
sig_pre_perm <- w_prem * cer_ann_due[round(length(which(phycon == 1)) / 1000)]       

per_LTC <- sig_pre_LTC / FinSin

per_perm <- sig_pre_perm / FinSin

per_LTCPerm <- round(per_LTC + per_perm, digits = 1) + unit

ntest <- round(1 - per_LTCPerm, digits = 1) / unit

# list all possible combinations of per_LTC and per_Ann
possible_allocations <- matrix(seq(0.5, (1 - per_LTCPerm), by = unit), , 1)

possible_results <- matrix(0, nrow(possible_allocations), 8)

for (i in 1 : nrow(possible_allocations)) {
    possible_results[i, ] <- post_opt(FinSin, retireSalary, possible_allocations[i], life_ept)
}

# addjusted possible results (eliminate the cases that % portfolio < % annuity)
if(length(which(possible_results[ , 3] < possible_results[ , 4])) == 0 ){
    add_pr <- possible_results
} else {
    add_pr <- possible_results[-which(possible_results[ , 3] < possible_results[ , 4]), -c(8)]
}


# the optimal results for a single test
# check if there is only one solution or no solution
if(is.null(nrow(add_pr))){    
    test_optr <- add_pr
}else{
    test_optr <- add_pr[which(add_pr[ , 2] == min(add_pr[ , 2])), ]
}
#if multi results have rp = 0, then choose greatest ANetA -- aggregated net asset at death
if(!is.null(nrow(test_optr)) && nrow(test_optr)>1){
    test_optr <- test_optr[which(test_optr[,1] == max(test_optr[,1])),]
}
names(test_optr) <- c("ANetA", "RuinProb", "Investment", "SPIA", "DSPIA", "LTC", "WL","shortfalls")





