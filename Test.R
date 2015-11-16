# Test Model Points

# Read in Test Points
testPoints <- read.csv(paste0(root,"Inputs\\DataTests\\IndChaModelPoints.csv"), header = TRUE)

results <- matrix(0, nrow = 10, ncol = 25)
colnames(results) <- c("Ideal Budget", "Ideal H", "Ideal Term", "Ideal Perm", "Ideal Di", "Ideal Ret", "IdealRP", 
                       "Rank H", "Rank Term", "Rank Perm", "Rank Disable", "Rank Retirement"
                       "Actual Budget", "OPT H", "OPT Term", "OPT Perm","OPT Di", "OPT Ret", "OPT RUIN",
                       "adjOPT H", "adjOPT Term", "adjOPT Perm", "adjOPT Di", "adjOPT Ret", "adjOPT Ruin")

for(caseID in 1:10){    
    # Set values to variables
    IndCha = testPoints[caseID, ]
    
    
    source(paste0(root, sourceCode,"SetValues.R"))
    source(paste0(root, sourceCode,"Customized.R"))
    source(paste0(root, sourceCode,"IdealBudget&Allocation.R"))
    
    rp.ideal <- pre_customized_ruin(preIdeal_allocation)
    
    cusRank <- ranking()
    ALLOC.adjOPT <- marginalApproach_hierarchy(preIdeal_allocation)
    rp.adjOPT <- pre_customized_ruin(ALLOC.adjOPT)
    
    ALLOC.OPT <- marginalApproach(preIdeal_allocation)
    rp.OPT <- pre_customized_ruin(ALLOC.OPT)
    
    
    results[caseID, 1] = sum(preIdeal_allocation)
    results[caseID, 2:6] = preIdeal_allocation
    results[caseID, 7] = rp.ideal
    
    results[caseID, 8] <- cusRank[[1]]
    results[caseID, 9] <- cusRank[[2]]
    results[caseID, 10] <- cusRank[[3]]
    results[caseID, 11] <- cusRank[[4]]
    results[caseID, 12] <- cusRank[[5]]
    
    results[caseID, 13] = sum(ALLOC.OPT)
    results[caseID, 14:18] = ALLOC.OPT
    results[caseID, 19] = rp.OPT
    
    results[caseID, 20:24] = ALLOC.adjOPT
    results[caseID, 25] = rp.adjOPT
}


write.csv(results, file = "outputs\\test1114_1to10.csv")
