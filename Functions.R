#Author: Zhiguo Wang
#Date: 10/14/2015
#Version: 1.0

#**************************************#
# Simulate Excel "vLookup" function, approximate match;
# Parameters: 
#   lookup_value: The value you want to look up
#   table_array : The dataset in which the vLookup will search for the lookup_value and the return value
#   col_index   : The column number that contains the return value.
# Return        : The value in col_index and in the same row as lookup_value; 

vLookup <- function(lookup_value, table_array, col_index){
    rowIndex <- which.min(table_array[, 1] - lookup_value)
    return(table_array(rowIndex, col_index)) 
}

# Get stochastic path #
getLifePath <- function(CurAge, Gender){
    load(paste0(root, workspaces,"Unif_MCMC.RData"))
    return(get(paste0("umcmc_",Gender, "_", CurAge)))
}


# return a matrix which
#   1. has the same size as pre_mcmc
#   2. has value 1 at the first death status, and 0 at other entries
getMatrixAtDeath <- function(MCMatrix){
    x <- 1 * (MCMatrix == 3)
    x1 <- rbind(rep(0, length = 1000), x)
    x1 <- x1[1:(nrow(x1)-1), ]
    x <- x - x1
    return(x)
}


# return a matrix which
#   1. has the same size as pre_mcmc
#   2. has value 1 at the first disable status, and 0 at other entries
getMatrixAtDisable <- function(MCMatrix){
    x <- 0 * MCMatrix
    
    for(i in 1:ncol(MCMatrix)){
        if(is.element(2,MCMatrix[,i])){
            x[which.max(MCMatrix[,i]==2),i] = 1
        }else{
            next
        }
    }
    
    return(x)
}

#' Return a matrix which
#'   is the same size of pre_mcmc
#'   is 1 before the first 2 or 3 appears
getMatrixHealthy <- function(MCMatrix){
    x <- 1 * (MCMatrix != 1)
    
    for (i in 1 : row(MCMatrix)){
        y <- MCMatrix[i,]
        
    }
}

#' Return a matrix which
#'   is the same size of pre_mcmc
#'   is 1 at the last "1" Healthy status
getMatrixLastH <- function(MCMatrix){
    # Mark all "1"s in matrix
    MCMatrix <- (1:nrow(MCMatrix)) * (MCMatrix == 1)
    
    # Locate the last postion that has "1"
    rows <- apply(MCMatrix, 2, which.max)
    
    # Write 1 to corresponding position
    returnMat <- 0 * MCMatrix
    for(i in 1: ncol(returnMat)){
        returnMat[rows[i], i] <- 1
    }
    return(returnMat)
}


# Return investment yield curves based on current age and retirement age
# Return value is a list of 3 sets of yield curves:
#      Low, Mid, Agg
# Return values is used in "Pre_CashFlow_ExistingEmployee&Employer.R"
# Parameters:
#   preYears: = retirementAge - currentAge
getPreYieldCurves <- function(preYears){
    
    load(paste0(root, workspaces, "Pre_YieldCurves.RData"))
    
    yield_Low <- get(paste0("yield_Low_", preYears))
    
    yield_Mid <- get(paste0("yield_Mid_", preYears))
    
    yield_Agg <- get(paste0("yield_Agg_", preYears))
        
    return(list("Low" = yield_Low, "Mid" = yield_Mid, "Agg" = yield_Agg))
}

# Compute Accumulative yield factor (r1,r2,r3,...)
# AV = CF1 * r1 + CF2 * r2 ....
# Return: matrix[preYears, 1000]
#    r11  r12  r13...
#    r21  r22  r23...
#    ...  ...  ...
getAccFactor <- function(yieldCurves, pre_mcmc){
    yieldCurves <- yield_Low * (pre_mcmc == 1)
    yieldCurves[which(yieldCurves == 0)] <- 1 
    for(i in (nrow(yieldCurves) - 1) : 1){
        yieldCurves[i,] <- yieldCurves[i,] * yieldCurves[i+1,]
    }
    return(yieldCurves* (pre_mcmc == 1))
}

# Return a matrix which -- version 1022
#  each element is an accumulated value for each scenario;
#  each element is accumulated from CurrentAge to " the Last Healthy"
#  Parameters: CF -- cash flow matrix
#              r  -- rate matrix
getAccValue1 <- function(mat.CF, mat.r){
       
    # Return matrix is the same size as Cash Flow matrix
    accMatrix <- 0 * mat.CF
    accMatrix[1,] <- mat.CF[1,] * mat.r[1,]
    for(i in 2 : nrow(mat.CF)){
        accMatrix[i,] <- (mat.CF[i,] + accMatrix[i-1,]) *  mat.r[i,]        
    }
    ### "pre_mcmc" need to be pre-calculated in <customized.R>
    return(accMatrix * getMatrixLastH(pre_mcmc))
}


# Return a matrix which -- version 1102
#  each element is an accumulated value for each scenario;
#  each element is accumulated from CurrentAge to " the Last Healthy"
#  Parameters: CF -- cash flow matrix
#              r  -- rate matrix
getAccValue <- function(mat.CF, mat.r, mat.death){
    
    
    # Return matrix is the same size as Cash Flow matrix
    accMatrix <- 0 * mat.CF
    accMatrix[1,] <- mat.CF[1,] * mat.r[1,]
    
    for(i in 2 : nrow(mat.CF)){
        accMatrix[i,] <- (mat.CF[i,] + accMatrix[i-1,]) *  mat.r[i,]        
    }
    
    ### 401k transfer as death benefit when death happens; stay in account otherwise.
    x <- accMatrix * mat.death
    lastRow <- nrow(x)
    x[lastRow, ] <- accMatrix[lastRow, ] * (pre_mcmc[lastRow, ] != 3) +x[lastRow, ]
    
    return(x)
}

# calculate social security benifit
# This function will use variables in the Global Working Environment:
#   annualIncome, retireAge, currentAge, wageInflation
getSSB <- function(){
    # The ceiling of income covered by SSB
    SSBase.2014 <- 117000
    
    # Percentage contributes to SSB
    perct <- c(0.9, 0.32, 0.15)
    
    # Monthly Income
    mInc <- min(SSBase.2014, annualIncome)/12
    bend <- c(816,4917)
    
    # Tires corresponding to each percentage [perct]
    tier <- c(min(bend[1], mInc), 
              max(0, min(mInc,bend[2] - bend[1])),
              max(0, mInc - bend[2]))
    # monthly Social Security Primary Insurance Amount (PIA)
    PIA.unadjusted <- sum(tier * perct)
    
    # Adjustment factor for early or late retirement
    if (retireAge < 67) {
        adjFactor <- min(3, 67-retireAge) * (12 * 5 / 900) +
            max(0, 67-retireAge - 3) * (12 * 5 / 1200)
    } else if (retAge >= 67) {
        adjFactor <- max(3, retireAge - 67) * (12 * 16 / 2400)
    }
    
    
    # Adjusted for early or late start
    return(PIA.unadjusted * (1 - adjFactor) * 12 * (1 + wageInflation) ^ (retireAge - currentAge))
    #https://www.ssa.gov/OACT/TR/TRassum.html
    # inflation may not consist with SSA website
}