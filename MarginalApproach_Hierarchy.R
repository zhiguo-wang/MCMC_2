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



marginalApproach_hierarchy <- function(ideal) {
  
  ## Healthcare expenses will be always fully covered
  
  ideal[1] <- - cashOut_Healthcare[1]
    
  stepWidth <- 200
  
  ruinProb <- c(1.1, 0, 0, 0, 0)
  
  allocation <- ideal
  allocation[which(cusRank == 5)] <- allocation[which(cusRank == 5)] * (actBudget / sum(preIdeal_allocation))
  budget <- sum(allocation)
  
  while(budget > actBudget) {
    stepWidth <- min(budget - actBudget, stepWidth)
    stfc1 <- allocation / ideal
        
    stfc2 <- (allocation - stepWidth) / ideal
    stfc2[which(stfc2 < 0)] <- 0
    
    ## Under the hierarchy, decide which one can be decreased
    if (stfc1[which(cusRank == 5)] > stfc2[which(cusRank == 4)]) {
      list <- c(which(cusRank == 5))
    } else if (stfc1[which(cusRank == 4)] > stfc2[which(cusRank == 3)]) {
      list <- c(which(cusRank == 4), which(cusRank == 5))
    } else if (stfc1[which(cusRank == 3)] > stfc2[which(cusRank == 2)]) {
      list <- c(which(cusRank == 3), which(cusRank == 4), which(cusRank == 5))
    } else {
      list <- c(which(cusRank == 2), which(cusRank == 3), which(cusRank == 4), which(cusRank == 5))
    }
    
    minRuinProb <- 1
    
    for (i in list) {
      if(allocation[i] <= 0){
          next
      }
      temp.allocation <- allocation
      temp.allocation[i] <- allocation[i] - stepWidth
      
      if(temp.allocation[i] <= 0){
          temp.allocation[i] = 0
      }
      
      tempRuin <- pre_customized_ruin(temp.allocation)
      if(tempRuin < minRuinProb){
          minRuinProb = tempRuin
          optProduct = i
      } 
      
    }
    
    allocation[optProduct] <- max(0, allocation[optProduct] - stepWidth)
   
    satisfaction <- allocation / ideal
    
    budget <- sum(allocation)
  }
    
  
  return(allocation)
  
}