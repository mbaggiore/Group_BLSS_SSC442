bank <- read.csv(url("https://msudataanalytics.github.io/SSC442/Labs/data/bank.csv"))

#Create train/test split
bank_idx = sample(nrow(bank), 2000)
bank_trn = bank[bank_idx, ]
bank_tst = bank[-bank_idx, ]

#Trying to predict default status from age, balance, and housing
fit_bank = glm(default~ balance, data = bank_trn, family = binomial(link = "logit"))
round(coef(fit_bank,2))

summary(fit_bank)

library(boot)
set.seed(1)
cv.glm(bank_trn, fit_bank, K = 10)$delta

bank_tst_pred = ifelse(predict(fit_bank, bank_tst) > 0,
                            "default",
                            "not default")

make_conf_mat = function(predicted, actual) {
  table(predicted = predicted, actual = actual)
}

coma_bank = make_conf_mat(predicted = bank_tst_pred, actual = bank_tst$default)
coma_bank

#Well, this model is awful, but I'm not sure how to make it better.
#THere seems to be separability, but after trying a bunch of different 
#models it didn't seem to go away. The confusion matrix is wonky.
#The coefficients then don't have too much meaning, but in EC420/1 
#style, they each have an interpretation that:
#"a unit increase in <parameter> is on average associated with a change of
# <coef> in the expected probability that the lender defaulted"