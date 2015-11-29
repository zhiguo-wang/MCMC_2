# Calculate Federal tax
# Parameters: 
#   curInc: Current Income
#   spoInc: Spouse Income
# Return:   Adjusted Federal Tax
getFedTax <- function(curInc, spoInc, married){
        
    # 1. Tax adjustment forward from 2011 to 2014
    #    File :  MM lookup table with testing page 0918 v39
    #    Sheet: IRS lookups
    #    Cell : I10
    TaxAdj.1114 <- 1.076891
    
    
    # 1. Tax adjustment back from 2014 to 2011
    #    File :  MM lookup table with testing page 0918 v39
    #    Sheet: IRS lookups
    #    Cell : I9
    TaxAdj.1411 <- 0.928599
    
    
    # 1. Income adjustment forward from 2011 to 2014
    #    File :  MM lookup table with testing page 0918 v39
    #    Sheet: IRS lookups
    #    Cell : C10
    IncomeAdj.1114 <- 1.092727
    
    
    # 1. Income adjustment back from 2014 to 2011
    #    File :  MM lookup table with testing page 0918 v39
    #    Sheet: IRS lookups
    #    Cell : C10
    IncomeAdj.1411 <- 0.915141659
    
    spoInc <- spoInc * (married == "M")
    HIBT <- curInc + spoInc
    # Estimated 2011 HBIT [IRS lookups(C14) = C13 * C9]
    estHIBT <- round(HIBT * IncomeAdj.1411, 0)
    
    # Highest HIBT in 2011 IRS tables
    IRSMaxHIBT <- 10000000
    
    # HIBT to use for tax lookups
    IRSLookupHIBT <- min(estHIBT, IRSMaxHIBT) 
    
    # Read in IRS tax stats data
    taxStats <- read.csv(paste0(root, "Inputs\\TaxRates\\IRSTaxStats.csv"))
    fedTaxType <- 3 + 1 * (married != "M")
    
    LowerHIBT.row <- max(which(taxStats$AGI - IRSLookupHIBT <= 0))
    LowerHIBT <- taxStats[LowerHIBT.row, 2]
    LowerTax <- taxStats[LowerHIBT.row, fedTaxType]
    
    if(LowerHIBT.row >= nrow(taxStats)){
        return(LowerTax * TaxAdj.1114)
    }
    
    HigherHIBT.row <- min(nrow(taxStats), LowerHIBT.row + 1)
    HigherHIBT <- taxStats[HigherHIBT.row, 2]
    HigherTax <- taxStats[HigherHIBT.row, fedTaxType]
    
    tax <- (LowerTax * (HigherHIBT - estHIBT) + HigherTax * (estHIBT - LowerHIBT)) / (HigherHIBT - LowerHIBT)
    return (round(tax * TaxAdj.1114, 0))
}

# Calculate Raw Income Tax
# Parameters: 
#   curInc: Current Income
#   spoInc: Spouse Income
#   married: Marital Status
#   dependents: Number of Dependents
#   state: State of customer. state is "" if calculate Federal Raw Income Tax
#       ([Federal Raw Income Tax] is used when calculating state income tax)
# Return:   Raw Income Tax Tax
# Description: This function will not be used directly.
#              This function will be called by function (getStateIncomeTax(....))
getRawIncomeTax <- function(curInc, spoInc, married, dependents, state, rawFedTax = 0){
    
    ifMarried <- married == "M"
    spoInc <- spoInc * ifMarried
    taxStats <- read.csv(paste0(root, "Inputs\\TaxRates\\StateIncomeTax.csv"), stringsAsFactors = FALSE)
    taxStats[is.na(taxStats)] <- 0
    taxStatsByState <- taxStats[which(taxStats$State == state), ]
    
    taxCredit <- taxStatsByState$Exmp.M * ifMarried + 
        (taxStatsByState$Exmp.S * !ifMarried) +
        taxStatsByState$Exmp.D * dependents
    
    if("Yes" %in% taxStatsByState$FedTaxDeductible){
        maxDeduction <- taxStatsByState$maxDeduction.M * ifMarried + (taxStatsByState$maxDeduction.S * !ifMarried)
        deduction <- min(maxDeduction, rawFedTax) + rawFedTax * !(maxDeduction > 0)
        
    } else {
        deduction <- 0
    }
    
    fedStd <- 6200 + 6200 * ifMarried
    
    # taxable income for AGI 
    taxIncome <- curInc + spoInc - taxCredit * (taxStatsByState$ExmpType != "(c)") - deduction - fedStd
    
    
    if(ifMarried){
        Bracket.L <- taxStatsByState$Brackets.M.L
        Bracket.H <- taxStatsByState$Brackets.M.H
    } else{
        Bracket.L <- taxStatsByState$Brackets.S.L
        Bracket.H <- taxStatsByState$Brackets.S.H
    }
    
    
    
    tax.total <- taxStatsByState$Low * min(Bracket.L, taxIncome) + 
        taxStatsByState$Middle * max(0, min(Bracket.H, taxIncome) - Bracket.L) +
        taxStatsByState$High * max(0, taxIncome - Bracket.H)
    exmpCredit <- taxCredit * (taxStatsByState$ExmpType == "(c)")
    
    rawIncomeTax <- tax.total - exmpCredit
    
    return(round(rawIncomeTax, 0))
}

# Calculate State Income Tax
# Parameters: 
#   curInc: Current Income
#   spoInc: Spouse Income
#   married: Marital Status
#   dependents: Number of Dependents
#   state: State of customer
# Return:   State Income Tax Tax
getStateIncomeTax <- function(curInc, spoInc, married, dependents,state){
    spoInc <- spoInc * (married == "M")
    
    rawFedTax <- getRawIncomeTax(curInc, spoInc, married, dependents,"")
    
    rawStTax <- getRawIncomeTax(curInc, spoInc, married, dependents,state, rawFedTax)
    
    fadTax <- getFedTax(curInc, spoInc, married)
    
    stateIncomeTax <- rawStTax * min(rawFedTax, fadTax) / max(1, rawFedTax)
    return(round(stateIncomeTax, 0))
}
