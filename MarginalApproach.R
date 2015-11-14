#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#   Program Name:   LIFP - Marginal Approach
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
#
#   Version           Date           Author            change
#               |               |               |
#     1.1       |   10/16/15    |    Justin     |      First Version
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Ideal Allocation for Pre
## 1. Healthcare Expenses 2. Term Life 3. Whole Life 4. Disability Insurance 5. 401k


marginalApproach <- function(idealAllocation) {
    
  ## Healthcare expenses will be always fully covered
  
  idealAllocation[1] <- - cashOut_Healthcare[1]
  
  stepWidth <- 200
  
  ruinProb <- c(1.1, 0, 0, 0, 0)
  
  budget <- sum(idealAllocation)
  
  while(budget > actBudget) {
    
    ## Term Life
    
    allocation <- idealAllocation
    
    allocation[2] <- max(idealAllocation[2] - stepWidth, 0)
    
    ruinProb[2] <- pre_customized_ruin(allocation)
    
    ## Whole Life
    
    allocation <- idealAllocation
    
    allocation[3] <- max(idealAllocation[3] - stepWidth, 0)
    
    ruinProb[3] <- pre_customized_ruin(allocation)
    
    ## Disability Insurance
    
    allocation <- idealAllocation
    
    allocation[4] <- max(idealAllocation[4] - stepWidth, 0)
    
    ruinProb[4] <- pre_customized_ruin(allocation)
    
    ## 401k
    
    allocation <- idealAllocation
    
    allocation[5] <- max(idealAllocation[5] - stepWidth, 0)
    
    ruinProb[5] <- pre_customized_ruin(allocation)
    
    ## Find which product has the least compact on ruin prob.
    
    index <- which(idealAllocation > 0)
    
    index1 <- index[ruinProb[index] == min(ruinProb[index])]
    
    idealAllocation[index1] <- max(idealAllocation[index1] - stepWidth, 0)
    
    budget <- sum(idealAllocation)   
    
  }
  
  print(proc.time())
  
  return(idealAllocation)
  
}