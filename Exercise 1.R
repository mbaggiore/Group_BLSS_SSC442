
#Exercise 1.a: Execute the code below. Based on the results,
#rank the models from "most underfit" to "most overfit".
#
#Summary of the 4 models:
#model        means  cv.glm
#caps     (4) 0.339  (4) 0.2166961
#selected (3) 0.224  (3) 0.1587043
#addiive  (1) 0.066  (1) 0.08684467
#over     (2) 0.136  (2) 0.137

#Each model has the same lowest to highest ranking by mean and cv.glm. Then this ranking matches
#to a scale of a lower number being relatively underfit and higher number being over fit (i.e., (1)
#is likely overfit (very low misclassification) and (4) is likely underfit).

#install.packages("kernlab")
library(kernlab)
data("spam")
tibble::as.tibble(spam)

is.factor(spam$type)
levels(spam$type)

set.seed(42)
# spam_idx = sample(nrow(spam), round(nrow(spam) / 2))
spam_idx = sample(nrow(spam), 1000)
spam_trn = spam[spam_idx, ]
spam_tst = spam[-spam_idx, ]

fit_caps = glm(type ~ capitalTotal,
               data = spam_trn, family = binomial)
fit_selected = glm(type ~ edu + money + capitalTotal + charDollar,
                   data = spam_trn, family = binomial)
fit_additive = glm(type ~ .,
                   data = spam_trn, family = binomial)
fit_over = glm(type ~ capitalTotal * (.),
               data = spam_trn, family = binomial, maxit = 50)

# training misclassification rate
mean(ifelse(predict(fit_caps) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_selected) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_additive) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_over) > 0, "spam", "nonspam") != spam_trn$type)

library(boot)
set.seed(1)
cv.glm(spam_trn, fit_caps, K = 5)$delta[1]
cv.glm(spam_trn, fit_selected, K = 5)$delta[1]
cv.glm(spam_trn, fit_additive, K = 5)$delta[1]
cv.glm(spam_trn, fit_over, K = 5)$delta[1]

#Exercise 1.b: Re run with 100 folds and a different seed
set.seed(0)
cv.glm(spam_trn, fit_caps, K = 100)$delta[1]
cv.glm(spam_trn, fit_selected, K = 100)$delta[1]
cv.glm(spam_trn, fit_additive, K = 100)$delta[1]
cv.glm(spam_trn, fit_over, K = 100)$delta[1]

#New outputs:
#caps     (4) 0.2167153
#selected (3) 0.1587634
#additive (1) 0.08062664
#over     (2) 0.1349652

#Ranking/conclusions do not change.

#Excercise 1.2

#Confusiion Matrix code
make_conf_mat = function(predicted, actual) {
  table(predicted = predicted, actual = actual)
}

#Storing the predicted values of our classifiers
spam_tst_pred_caps = ifelse(predict(fit_caps, spam_tst) > 0,
                       "spam",
                       "nonspam")
spam_tst_pred_sel = ifelse(predict(fit_selected, spam_tst) > 0,
                        "spam",
                        "nonspam")
spam_tst_pred_add = ifelse(predict(fit_additive, spam_tst) > 0,
                        "spam",
                        "nonspam")
spam_tst_pred_over = ifelse(predict(fit_over, spam_tst) > 0,
                        "spam",
                        "nonspam")

#Four confusion matrices:
coma_caps = make_conf_mat(predicted = spam_tst_pred_caps,actual = spam_tst$type)
coma_selected = make_conf_mat(predicted = spam_tst_pred_sel,actual = spam_tst$type)
coma_additive = make_conf_mat(predicted = spam_tst_pred_add,actual = spam_tst$type)
coma_over = make_conf_mat(predicted = spam_tst_pred_over,actual = spam_tst$type)

#Sensitivity function
sens = function(coma){
   result=coma[2,2]/(coma[2,2] + coma[2,1])
   result
}

#Specificity function
spec = function(coma){
  result = coma[1,1]/(coma[1,1]+coma[1,2])
  result
}

coma_caps
sens(coma_caps)
spec(coma_caps)

coma_selected
sens(coma_selected)
spec(coma_selected)

coma_additive
sens(coma_additive)
spec(coma_additive)

coma_over
sens(coma_over)
spec(coma_over)

#Summary:
#If both specificity and sensitivity are higher from one model
#to the next, it is strictly better in terms of sens and spec. 
#We see that additive has higher sens and spec than both selected
#and caps, so let's consider the difference between additive and over

#Over has a slightly higher specificity, but a much lower sensitivity
#than additive. The question becomes: would we trade off .02 in specificity
# for .16 sensitivity? I think the answer is yes -- the difference
#is a factor of 8x, so I would say additive is the best model here.
#it can't be the best.
