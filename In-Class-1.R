# 1/23/20 In-Class Activity
# Group 9:
#   Ryan Sears
#   Jacob Schmidt
#   Marquise Baggiore
#   Wenquan Lai

#Load bank data from github. Attached to make "bank" objects easier to work with
bank <- read.csv("https://raw.githubusercontent.com/jschmidt345/Group_BLSS_SSC442/master/bank.csv")
#attach(bank)

#Part 1: Cleaning Factors in Bank
factor(marital, labels=c(1, 2, 3))


#Part 2: Regression of balance on all other variables
reg1 = lm(balance ~ age + job +marital + education + default + housing + loan 
          + contact + day + month + duration  + campaign, data = bank)

#Part 3: Create a nullreg to compare to reg1 in ANOVA test
nullreg1 = lm(balance ~ 1, data = bank)
anova(nullreg1, reg1)

#Part 4: NEED TO RUN ANALYSIS THEN REMOVE VARIABLES:
#Regression without variables not significant at the 5% level via F-test:

reg1 = lm(balance ~ age + job +marital + education + default + housing + loan 
          + contact + day + month + duration  + campaign, data = bank)

#Part 5: Discussion

