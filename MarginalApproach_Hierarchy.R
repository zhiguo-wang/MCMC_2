#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#   Program Name:   LIFP - Marginal Approach_Hierarchy
#   Version:        1.1
#
#   Created By:     Justin Xu
#   Modified By:    Zhiguo Wang
#
#   Datasource:     
#   Source Tables:  \\Inputs\\
#   Target Tables:  \\Outputs\\
#
#   Purpose:        To obtain the optimal allocations for financial products
#                   and adjust the result by hierarchy of priority
#
#   Version           Date           Author            change
#               |               |               |
#     1.1       |   11/05/15    |    Justin     |      First Version
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## Input the hierarchy of priority of each financial products
hierarchy <- read.csv(paste0(root,"Inputs\\Hierarchy\\Hierarchy.csv"), header = TRUE)

## Locate the customer's hierarchy

ranking <- function() {
  
  rankID <- paste0(hierarchy$Age, hierarchy$Gender, hierarchy$Marital.Status,
                   hierarchy$Dependents, hierarchy$Mortgage, hierarchy$Breadwinner)
  
  ageGroup <- c(25, 30, 35, 40, 45, 50, 55, 60, 65)
  
  ageDiff <- abs(ageGroup - currentAge)
  
  age <- ageGroup[which.min(ageDiff)]
  
  if (maritalStatus == "M") {
    marriage <- "Y"
  } else {
    marriage <- "N"
  }
  
  if (numDpt != 0) {
    dependents <- "Y"
  } else {
    dependents <- "N"
  }
  
  if (monMortPay != 0) {
    mortgage <- "Y"
  } else {
    mortgage <- "N"
  }
  
  cusID <- paste0(age, gender, marriage, dependents, mortgage, breadWinner)

  cusRank <- hierarchy[which(rankID == cusID), -1 : -6]
  
  return(cusRank)
  
}

cusRank <- ranking()

## Ideal Allocation for Pre
## 1. Healthcare Expenses 2. Term Life 3. Whole Life 4. Disability Insurance 5. 401k



marginalApproach_hierarchy <- function(preIdeal_allocation) {
  
  ## Healthcare expenses will be always fully covered
  
  preIdeal_allocation[1] <- - cashOut_Healthcare[1]
  
  originalAllocation <- preIdeal_allocation
  
  stepWidth <- 500
  
  ruinProb <- c(1.1, 0, 0, 0, 0)
  
  satisfaction <- c(1, 1, 1, 1, 1)
  
  budget <- sum(preIdeal_allocation)
  
  while(budget > actBudget) {
    
    stfc1 <- satisfaction
    
    stfc2 <- satisfaction
    
    stfc2[2 : 5] <- (preIdeal_allocation[2 : 5] - stepWidth) / originalAllocation[2 : 5]
    
    ## Under the hierarchy, decide which one can be decreased
    if (stfc1[which(cusRank == 5)] > stfc2[which(cusRank == 4)]) {
      list <- c(which(cusRank == 5))
    } else if (stfc1[which(cusRank == 5)] <= stfc2[which(cusRank == 4)] &
                 stfc1[which(cusRank == 4)] > stfc2[which(cusRank == 3)]) {
      list <- c(which(cusRank == 4), which(cusRank == 5))
    } else if (stfc1[which(cusRank == 5)] <= stfc2[which(cusRank == 4)] &
                 stfc1[which(cusRank == 4)] <= stfc2[which(cusRank == 3)] &
                   stfc1[which(cusRank == 3)] > stfc2[which(cusRank == 2)]) {
      list <- c(which(cusRank == 3), which(cusRank == 4), which(cusRank == 5))
    } else {
      list <- c(which(cusRank == 2), which(cusRank == 3), which(cusRank == 4), which(cusRank == 5))
    }
    
    ruinProb <- rep(0, length(list))
    
    for (i in list) {
      
      allocation <- preIdeal_allocation
      
      allocation[i] <- preIdeal_allocation[i] - stepWidth
      
      ruinProb[which(list == i)] <- pre_customized_ruin(allocation)
      
    }
    
    preIdeal_allocation[list[which.min(ruinProb)]] <- preIdeal_allocation[list[which.min(ruinProb)]] - stepWidth
    
    satisfaction <- preIdeal_allocation / originalAllocation
    
    budget <- sum(preIdeal_allocation)
    
  }
    
  print(proc.time())
  
  return(preIdeal_allocation)
  
}