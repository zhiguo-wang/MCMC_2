#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 1.9 the present value of certain annuity due (duration from 1 to 50)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

getCertainAnnuityDue <- function(){
    cer_ann_due <- vector(length = 50)
    for (i in 1 : 50) {
        cad <- 0
        for (j in 1 : i) {
            cad <- cad + (1 + 0.05) ^ (1 - j)
        }
        cer_ann_due[i] <- cad
    }
    return(cer_ann_due)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Force the long term care benefit to no more than 5 years
# If disable period is greater than 5 years, the long term care only covers 5 years

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
shrink <- function(x){
    if( length( x[x>0] > 5) ){
        x[which(x>0)[-c(1:5)]] <- 0
    }
    return(x)
}


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# optimization function
# return: ruin probability
#         optimized allocation
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
post_opt <- function(FinSin, retireSalary, per_Inv, life_ept) {
    
    # certain annuity (cash inflow) from investment portfolio
    
    guarantee_rate <- 0.02
    ca_vector <- seq(0, life_ept - 1)
    for (i in 1 : life_ept) {
        ca_vector[i] <- (1 + guarantee_rate) ^ (-ca_vector[i])
    }
    certain_annuity_inv <- per_Inv * FinSin / sum(ca_vector)
    
    
    # Deferred SPIA
    # assume the deferred annuity benefits = certain annuity from investment portfolio
    # http://www.dinkytown.net/java/FixedAnnuity.html
    # deferred annuity benefits
    dfa <- certain_annuity_inv * (MaxAge - (retireAge + life_ept)) * (1 / 3)
    
    # deferred annuity single premium
    pre_dfa <- dfa / (60954 / 18000)
    per_dfa <- pre_dfa / FinSin
    if (per_dfa + per_Inv + per_LTC + per_perm > 1 ) {
        per_dfa <- max(1 - per_LTC - per_perm - per_Inv, 0)
    }
    
    # SPIA
    # percentage for SPIA
    per_Ann <- max(1 - per_Inv - per_dfa - per_LTC - per_perm, 0)
        
    # SPIA single premium immediate annuity
    
    pre_ima <- FinSin * per_Ann
    
    # stagger immediate annuity strategy benefits
    # https://www.immediateannuities.com/
    # http://money.cnn.com/tools/annuities/
    # 0.066 is the ratio between premium and benefits
    
    spia <- pre_ima * r_spia[which(r_spia==retireAge),if(gender=="M")2 else 3]
    
    # perm life insurance benefits
    
    plb <- w_bene
    
    # perm life insurance premium (rate comes from Misc lookup tables)
        
    pre_perm <- w_prem
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # 3. Build the cash flow matrix
        
    # 3.1 Cash Inflows
        
    # 3.1.1 Cash Inflow_Social Security Benefits
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    cashin_ssb <- matrix(0, period, st)
    cashin_ssb[which(phycon != 3)] <- annualSSB
    #cashin_ssb[1 : (life_ept + 1), ] <- 0
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # 3.1.2 Cash Inflow_SPIA
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    cashin_spia <- matrix(0, period, st)
    #for (i in 1 : st) {
    #  cashin_spia[which(phycon[ , i] != 3), i] <- spia_vector[which(phycon[ , i] != 3)]
    #}
    cashin_spia[which(phycon != 3)] <- spia
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # 3.1.3 Cash Inflow_LTC
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    cashin_LTC <- matrix(0, period, st)
    cashin_LTC[which(phycon == 2)] <- LTCb
    
    # according to LTC_period to adjust the benefits
      
    #cashin_LTC <- apply(cashin_LTC, 2, shrink)
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # 3.1.4 Cash Inflow_Whole Life Insurance 
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    cashin_dea <- matrix(0, period, st)
    for (i in 1 : st) {
        cashin_dea[min(which(phycon[ , i] == 3)), i] <- plb
    }
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # 3.1.5 Cash Inflow_Deferred SPIA
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    cashin_dfa <- matrix(0, period, st)
    for (i in 1 : st) {
        if (length(which(phycon[ , i] == 1)) >= (life_ept + 1)) {
            cashin_dfa[(life_ept + 1), i] <- dfa
        }
    }
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # 3.2 Cash Outflows
    
    # 3.2.1 Cash Outflow_Basic Living Expenses & Healthcare Expenses & Add Expenses
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # assume the basic living expenses is a percentage (bas_liv_rate) of the Salary
    cashout_bdle <- matrix(0, period, st)
    cashout_bdle[which(phycon == 1)] <- ((a0 + a1 * retireSalary)+ (b0 + b1 * retireAge) + 
                                             (c0 + c1 * retireAge + c2 * retireSalary))
    #change needed/discuss needed. 3rd term * 0.5?
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # 3.2.2 Cash Outflow_LTC Living Expenses
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    cashout_bdle[which(phycon == 2)] <- ((a0 + a1 * retireSalary)+ (b0 + b1 * retireAge) + 
                                             (c0 + c1 * retireAge + c2 * retireSalary)) * 2
    cashout_bdle <- cashout_bdle * as.vector(lev)
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # 3.2.3 Cash Outflow_Death Expenses
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    cashout_dea <- matrix(0, period, st)
    for (i in 1 : st) {
        cashout_dea[min(which(phycon[ , i] == 3)), i] <- Burial_cost
    }
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # 3.2.4 Cash Outflow_SPIA
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    cashout_spia <- matrix(0, period, st)
    cashout_spia[1, ] <- pre_ima
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # 3.2.5 Cash Outflow_Deferred SPIA
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    cashout_dfa <- matrix(0, period, st)
    cashout_dfa[1, ] <- pre_dfa
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # 3.2.6 Cash Outflow_LTC
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    cashout_LTC <- matrix(0, period, st)
    cashout_LTC[which(phycon == 1)] <- pre_LTC
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # 3.2.7 Cash Outflow_Perm Insurance
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    cashout_perm <- matrix(0, period, st)
    cashout_perm[which(phycon == 1)] <- pre_perm
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # 4. Net Assets through Post-retirement
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    IniA <- matrix(0, period, st)
    IniA[1, ] <- FinSin
    NetA <- matrix(0, period, st)
    NetA[1, ] <- IniA[1, ] + cashin_ssb[1, ] + cashin_spia[1, ] + cashin_LTC[1, ] + 
        cashin_dea[1, ] + cashin_dfa[1, ] -
        cashout_bdle[1, ] - cashout_dea[1, ] - cashout_spia[1, ] - 
        cashout_dfa[1, ] - cashout_LTC[1, ] - cashout_perm[1, ]

    # cumulate the net assets
    for (i in 1 : st) {
           if(phycon[1 , i] == 3) next
        for (j in 2 : min(which(phycon[ , i] == 3))) {
                NetA[j, i] <- NetA[(j - 1), i] * (per_Ann + per_dfa) * YCP_A[j, i] + 
                NetA[(j - 1), i] * ((1 - (per_Ann + per_dfa)) * 0.5) * YCP_M[j, i] + 
                NetA[(j - 1), i] * ((1 - (per_Ann + per_dfa)) * 0.5) * YCP_L[j, i] + 
                cashin_ssb[j, i] + cashin_spia[j, i] + cashin_LTC[j, i] + 
                cashin_dea[j, i] + cashin_dfa[j, i] -
                cashout_bdle[j, i] - cashout_dea[j, i] - cashout_spia[j, i] - 
                cashout_dfa[j, i] - cashout_LTC[j, i] - cashout_perm[j, i]
         
        }
    }
    
    # Weighted Net Assets at Death Time
    WNetA <- numeric(period)
    for (i in 1 : st) {
        WNetA[i] <- NetA[max(which(NetA[ , i] != 0)), i]
    }
    WNetA <- WNetA * (post_mcmc[1, ] != 3) * (endNetAssetVec >= 0)
    # aggregate ruin probability
    agg_ruin <- abs(sum(WNetA[which(WNetA < 0)])) / 
        (abs(sum(WNetA[which(WNetA < 0)])) + 
             sum(WNetA[which(WNetA >= 0)]))
    # LTC ruin probability
    #temp <- phycon[ , which(WNetA < 0)]
    #reason2 <- 0
    #for (i in 1 : ncol(temp)) {
    #  count2 <- length(which(temp[ , i] == 2))
    #    if (count2 > 0) {
    #      reason2 <- reason2 + 1
    #    }
    #}
    #LTC_ruin <- reason2 / st
    
    # longevity ruin probability
    #liv_ruin <- agg_ruin - LTC_ruin
    
    # give more weights for ruin cases
    WNetA <- sort(WNetA, decreasing = TRUE)
    #weight_post <- matrix(seq(1 : st), 1, st)
    #for (i in 1 : st) {
    #  WNetA[i] <- WNetA[i] * (i / sum(weight_post))
    #}
    # find the break-even point
    #be_point <- max(which(WNetA > 0))
    # Final Weighted Net Assets at Death Time
    #FWNetA <- sum(WNetA[be_point : st]) / length(WNetA[be_point : st])
    #FWNetA <- (sum(WNetA[which(WNetA < 0)] * 0.8) +
    #          sum(WNetA[which(WNetA >= 0)] * 0.2)) / st
    
    # Average net assets
    ANetA <- sum(WNetA) / st
    
    # Shortfalls at death
    shortfalls <- sum(WNetA[which(WNetA < 0)]) / length(which(WNetA < 0))
    
    #result <- cbind(ANetA, agg_ruin, LTC_ruin, liv_ruin)
    result <- cbind(ANetA, agg_ruin, per_Inv, per_Ann, per_dfa, per_LTC, per_perm, shortfalls)
    
    return(result)
}


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' naive strategy function
#' cash income is from investment and ssb
#' cash outflow includes living, healthcare and additional expenses, and double of those if the person becomes disable
#'                       and death expenses-Burial_cost
post_naive <- function() {
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # 3.2 Cash inflows; Cash inflows are provided by ssb and investment
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    cashin_ssb <- matrix(0, period, st)
    cashin_ssb[which(phycon != 3)] <- annualSSB
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # 3.2 Cash Outflows
    
    # 3.2.1 Cash Outflow_Basic Living Expenses & Healthcare Expenses & Add Expenses
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # assume the basic living expenses is a percentage (bas_liv_rate) of the Salary
    cashout_bdle <- matrix(0, period, st)
    cashout_bdle[which(phycon == 1)] <- ((a0 + a1 * retireSalary)+ (b0 + b1 * retireAge) + 
                                             (c0 + c1 * retireAge + c2 * retireSalary))
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # 3.2.2 Cash Outflow_LTC Living Expenses
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    cashout_bdle[which(phycon == 2)] <- ((a0 + a1 * retireSalary)+ (b0 + b1 * retireAge) + 
                                             (c0 + c1 * retireAge + c2 * retireSalary)) * 2
    cashout_bdle <- cashout_bdle * as.vector(lev)
    
    # Whole life insurance premium
    cashout_perm <- matrix(0, period, st)
    cashout_perm[which(phycon == 1)] <- w_prem
    
    # Long term care premium
    cashout_LTC <- matrix(0, period, st)
    cashout_LTC[which(phycon == 1)] <- pre_LTC
    
    # Long Term care benefit
    cashin_LTC <- matrix(0, period, st)
    cashin_LTC[which(phycon == 2)] <- LTCb
        
    # Cash Outflow_Death Expenses
    #   = Burial_cost - Whole Life Insurance benefit
    cashout_dea <- matrix(0, period, st)
    for (i in 1 : st) {
        cashout_dea[min(which(phycon[ , i] == 3)), i] <- Burial_cost - w_bene
    }
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # 4. Net Assets through Post-retirement
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    IniA <- matrix(0, period, st)
    IniA[1, ] <- FinSin
    NetA <- matrix(0, period, st)
    NetA[1, ] <- IniA[1, ] + cashin_ssb[1, ] -
        cashout_bdle[1, ] - cashout_dea[1, ] - cashout_dea[1, ] - cashout_LTC[1, ] - cashout_perm[1, ]
        + cashin_LTC[1, ]
    
    # cumulate the net assets
    for (i in 1 : st) {
        if(phycon[1, i] == 3) next
        for (j in 2 : min(which(phycon[ , i] == 3))) {
            NetA[j, i] <- NetA[(j - 1), i] *YCP_M[j, i] + cashin_ssb[j, i] + cashin_LTC[j, i] -
                cashout_bdle[j, i] - cashout_dea[j, i] - cashout_LTC[j, i] - cashout_perm[j, i]
        }
    }
    
    # Weighted Net Assets at Death Time
    WNetA <- matrix(0, 1, st)
    for (i in 1 : st) {
        WNetA[1, i] <- NetA[max(which(NetA[ , i] != 0)), i]
    }
    WNetA <- WNetA * (post_mcmc[1, ] != 3) * (endNetAssetVec >= 0)
    # aggregate ruin probability
    agg_ruin <- abs(sum(WNetA[which(WNetA < 0)])) / 
        (abs(sum(WNetA[which(WNetA < 0)])) + 
             sum(WNetA[which(WNetA >= 0)]))
    
    
    
    # give more weights for ruin cases
    WNetA <- sort(WNetA, decreasing = TRUE)
    
    # Average net assets
    ANetA <- sum(WNetA) / st
    
    # Shortfalls at death
    shortfalls <- sum(WNetA[which(WNetA < 0)]) / length(which(WNetA < 0))
    
    #result <- cbind(ANetA, agg_ruin, LTC_ruin, liv_ruin)
    result <- cbind(ANetA, agg_ruin, shortfalls)
    
    return(result)
    #print(result)
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
}