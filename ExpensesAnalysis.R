#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#   Program Name:   LIFP - Expenses Analysis
#   Version:        1.1
#
#   Created By:     Tianyi Song
#   Modified By:    Justin Xu
#
#   Database Used:  Expenses.csv
#   Source Tables:  \\Inputs\\
#   Target Tables:  \\Outputs\\
#
#   Purpose:        To do regressions for different kinds of living expenses
#
#   Version           Date           Author            change
#               |               |               |
#     1.1       |   10/17/15    |    Justin     |      First Version
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 1. Input Expenses.csv
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

expenses <- read.csv(paste0(root, "Inputs\\Expenses\\Expenses.csv"),header=TRUE)

age <- expenses$Age

income <- expenses$Incomes

basicexp <- expenses$Living.expenses.without.mortgage.and.rent

mortgage <- expenses$Mortgage

rent <- expenses$Rent

healthexp <- expenses$Healthcare.expenses

addexp <- expenses$Additional.living.expenses

inisaving <- expenses$Initial.Savings

penIns <- expenses$Personal.insurance.and.pensions

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 2. Expenses Regressions
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Model1_1: Y = basic living expenses(without morgage, rent, healthcare expenses
## and pensions & insurance), X1 = Age, X2 = Income

model1_1 <- lm(basicexp ~ age + income)

model1_1

summary(model1_1)

## Model1_2: Y = basic living expenses, X1 = Age

model1_2 <- lm(basicexp ~ age)

model1_2

summary(model1_2)

## Model1_3: Y = basic living expenses, X1 = Income (the smallest p value)

model1_3 <- lm(basicexp ~ income)

model1_3

summary(model1_3)

## Model2_1: Y = healthcare expenses, X1 = Age, X2 = Income

model2_1 <- lm(healthexp ~ age + income)

model2_1

summary(model2_1)

## Model2_2: Y = healthcare expenses, X1 = Age (the smallest p value)

model2_2 <- lm(healthexp ~ age)

model2_2

summary(model2_2)

## Model2_3: Y = healthcare expenses, X1 = Income

model2_3 <- lm(healthexp ~ income)

model2_3

summary(model2_3)

## Model3_1: Y = additional living expenses, X1 = Age, X2 = Income 

model3_1 <- lm(addexp ~ age + income)

model3_1

summary(model3_1)

## Model3_2: Y = additional living expenses, X1 = Age

model3_2 <- lm(addexp ~ age)

model3_2

summary(model3_2)

## Model3_3: Y = additional living expenses, X1 = Income (the smallest p value)

model3_3 <- lm(addexp ~ income)

model3_3

summary(model3_3)

## Model4_1: Y = inital savings, X1 = Age, X2 = Income (the smallest p value)

model4_1 <- lm(inisaving ~ age + income)

model4_1

summary(model4_1)

## Model4_2: Y = inital savings, X1 = Age 

model4_2 <- lm(inisaving ~ age) 

model4_2

summary(model4_2)

## Model4_3: Y = inital savings, X1 = Income

model4_3 <- lm(inisaving ~ income)

model4_3

summary(model4_3)

## Model5_1: Y = personal insurance & pension plan, X1 = Age, X2 = Income (the smallest p value)

model5_1 <- lm(penIns ~ age + income)

model5_1

summary(model5_1)

## Model5_2: Y = personal insurance & pension plan, X1 = Age 

model5_2 <- lm(penIns ~ age) 

model5_2

summary(model5_2)

## Model5_3: Y = personal insurance & pension plan, X1 = Income

model5_3 <- lm(penIns ~ income)

model5_3

summary(model5_3)

## Model6_1: Y = mortgage, X1 = Age, X2 = Income 

model6_1 <- lm(mortgage ~ age + income)

model6_1

summary(model6_1)

## Model6_2: Y = mortgage, X1 = Age 

model6_2 <- lm(mortgage ~ age) 

model6_2

summary(model6_2)

## Model6_3: Y = mortgage, X1 = Income (the smallest p value)

model6_3 <- lm(mortgage ~ income)

model6_3

summary(model6_3)

## Model7_1: Y = rent, X1 = Age, X2 = Income 

model7_1 <- lm(rent ~ age + income)

model7_1

summary(model7_1)

## Model7_2: Y = rent, X1 = Age (the smallest p value)

model7_2 <- lm(rent ~ age) 

model7_2

summary(model7_2)

## Model7_3: Y = rent, X1 = Income 

model7_3 <- lm(rent ~ income)

model7_3

summary(model7_3)