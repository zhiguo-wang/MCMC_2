testPoints <- read.csv(paste0(root,"Inputs\\DataTests\\IndChaModelPoints_For Jay02.csv"), header = TRUE)
dataSize <- nrow(testPoints)

pre.IdealResult <- matrix(0, ncol = 7, nrow = dataSize)
colnames(pre.IdealResult) <- c("Ideal Budget", "Ideal H", "Ideal Term", "Ideal Perm", "Ideal Di", "Ideal Ret", "IdealRP")

pre.ActResult <- matrix(0, ncol = 7, nrow = dataSize)
colnames(pre.ActResult) <- c("Actual Budget", "OPT H", "OPT Term", "OPT Perm","OPT Di", "OPT Ret", "OPT RUIN")

pre.ActAdjResult <- matrix(0, ncol = 6, nrow = dataSize)
colnames(pre.ActAdjResult) <- c("adjOPT H", "adjOPT Term", "adjOPT Perm", "adjOPT Di", "adjOPT Ret", "adjOPT Ruin")

post.IdealResult <- matrix(0, ncol = 7, nrow = dataSize)
colnames(post.IdealResult) <- c("Ideal Starting Assets", "Ideal % Investment",  "Ideal % SPIA", "Ideal % DSPIA", "Ideal % LTC",
                                "Ideal % Perm",    "IdealRP_Post")
post.ActResult <- matrix(0, ncol = 7, nrow = dataSize)
colnames(post.ActResult) <- c("Actual Starting Assets", "Actual % Investment", 
                              "Actual % SPIA", "Actual % DSPIA", "Actual % LTC",    "Actual % Perm",	"ActualRP_Post")
rank <- matrix(0, ncol = 5, nrow = dataSize)
colnames(rank) <- c("Rank H", "Rank Term", "Rank Perm", "Rank Disable", "Rank Retirement")

source(paste0(root,sourceCode, "PostFunctions.R"))

caseID = 1
for(caseID in 1 : dataSize){
    
    print(paste0("CASE [", caseID, "]. started......"))
    
    IndCha = testPoints[caseID, ]
    
    source(paste0(root, sourceCode,"SetValues.R"))
    source(paste0(root, sourceCode,"Customized.R"))
    source(paste0(root, sourceCode,"IdealBudget&Allocation.R"))
    endNetAssetVec <- 0
    
    rp.ideal <- pre_customized_ruin(preIdeal_allocation)
    
    pre_allocation <- preIdeal_allocation
    pre.IdealResult[caseID, ] <- c(sum(preIdeal_allocation), 
                                   preIdeal_allocation[1],
                                   preIdeal_allocation[2],
                                   preIdeal_allocation[3],
                                   preIdeal_allocation[4],
                                   preIdeal_allocation[5],
                                   rp.ideal)
    print("Pre-retirement ideal situation completed")
    
    # Need Adjusted
    posCol <- which(endNetAssetVec > 0 & (post_mcmc[1, ] != 3))
    endAveAsset <- mean(endNetAssetVec[posCol] - vec_cashOut_RetirementNeeds[posCol])
    #
    source(paste0(root, sourceCode, "PostModel.R"))
    post.IdealResult[caseID, ] <- c(endAveAsset, 
                                    test_optr["Investment"],
                                    test_optr["SPIA"],
                                    test_optr["DSPIA"],
                                    test_optr["LTC"],
                                    test_optr["WL"],
                                    test_optr["RuinProb"])
    print("Post-retirement ideal situation completed")
    
    cusRank <- ranking()
    rank[caseID, ] <- c(cusRank[[1]], cusRank[[2]], cusRank[[3]], cusRank[[4]], cusRank[[5]])
    
    ALLOC.OPT <- marginalApproach(preIdeal_allocation)
    rp.OPT <- pre_customized_ruin(ALLOC.OPT)
    pre.ActResult[caseID, ] <- c(sum(ALLOC.OPT), 
                                 ALLOC.OPT[1],
                                 ALLOC.OPT[2],
                                 ALLOC.OPT[3],
                                 ALLOC.OPT[4],
                                 ALLOC.OPT[5],
                                 rp.OPT)
    print("Pre-retirement Actual situation w/o RANKING completed")
    ALLOC.adjOPT <- marginalApproach_hierarchy(preIdeal_allocation)
    rp.adjOPT <- pre_customized_ruin(ALLOC.adjOPT)
    pre.ActAdjResult[caseID, ] <- c(ALLOC.adjOPT[1],
                          ALLOC.adjOPT[2],
                          ALLOC.adjOPT[3],
                          ALLOC.adjOPT[4],
                          ALLOC.adjOPT[5],
                          rp.adjOPT)
    print("Pre-retirement Actual situation w/ RANKING completed")
    
    #need adjusted
    posCol <- which(endNetAssetVec > 0 & (post_mcmc[1, ] != 3))
    endAveAsset <- mean(endNetAssetVec[posCol] - vec_cashOut_RetirementNeeds[posCol])
    #
    pre_allocation <- ALLOC.adjOPT
    source(paste0(root, sourceCode, "PostModel.R"))
    post.ActResult[caseID, ] <- c(endAveAsset, 
                                  test_optr["Investment"],
                                  test_optr["SPIA"],
                                  test_optr["DSPIA"],
                                  test_optr["LTC"],
                                  test_optr["WL"],
                                  test_optr["RuinProb"])
    print("Post-retirement Actual situation completed")
    print(paste0("CASE [", caseID, "]. completed......"))
}


write.csv(cbind(pre.IdealResult, pre.ActResult,rank, pre.ActAdjResult, post.IdealResult, post.ActResult), file = paste0(root, output, "testResult1204001.csv"))
