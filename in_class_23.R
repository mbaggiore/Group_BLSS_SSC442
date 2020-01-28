data <- read.csv('bank.csv')


# regression

long_model <- balance ~ age + job + marital + education + default + balance + housing + loan + day + month + duration + campaign + previous + contact
fitted_multiple_model <- lm(long_model, data=data)
summary(fitted_multiple_model)
a <- predict(fitted_multiple_model, newdata = data, interval = "prediction", level = 0.99)

# 3 columns, married divorced single  
# 
new <- data$martial[df$marital=='married']


short_model <- balance ~ job$jobblue-collar
fitted_short <- lm(short_model, data=data)

# significant : age, jobhousemaid, jobretired, maritalmarried, martialsingle, tertiary edu, default yes, loan yes, monthmay, monthnov, monthoct, 
