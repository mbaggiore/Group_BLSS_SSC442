# Lab 3
library(MASS)
library(leaps)
library(caret)

ames_data <- ames_data <- read.csv('ames.csv', header = TRUE, sep=',')
data <- ames_data[, sapply(ames_data, is.integer)]
data_om <- na.omit(data)


# dropping data
drops <- c('OverallCond', 'OverallQual')
df <- data_om[, !(names(data_om) %in% drops)]

#prelimnary steps
get_complexity = function(model) {
  length(coef(model)) - 1
}
rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}



### EXERCISE 1 ###
traincontrol <- trainControl(method='cv', number=10)

stepmodel <- train(SalePrice ~., data=df,
                    method='leapBackward',
                    tuneGrid = data.frame(nvmax=1 : 2),
                    trControl = traincontrol)

rmse_result <- stepmodel$results['RMSE']
complexity_result <- stepmodel$results['nvmax']

rmse_plot <- as.vector(unlist(rmse_result, use.names = FALSE))
complexity_plot <- as.vector(unlist(complexity_result, use.names = FALSE))

# plot rmse against complexity
plot(complexity_plot, rmse_plot)
'
analysis :
- as complexity increases, rmse falls. 
- complexity of 2 has the highest rmse
- complexity of 15 has the lowest rmse
- if you use the full size model of complex = 34, you end up with the same rmse as a complexity of 20
- complexity of 20 is ideal. 
- using citerion rmse for statement
'

### EXERCISE 2 ###
num_obs <- nrow(df)
train_index <- sample(num_obs, size=trunc(.5 * num_obs))
train_data <- df[train_index, ]
test_data <- df[-train_index, ]


stepmodel_train <- train(SalePrice ~., data=train_data,
                   method='leapBackward',
                   tuneGrid = data.frame(nvmax=1 : 21),
                   trControl = traincontrol)

stepmodel_test <- train(SalePrice ~., data=test_data,
                         method='leapBackward',
                         tuneGrid = data.frame(nvmax=1 : 21),
                         trControl = traincontrol)

rmse_result_train <- stepmodel_train$results['RMSE']
complexity_result <- stepmodel_train$results['nvmax']

rmse_plot_train <- as.vector(unlist(rmse_result_train, use.names = FALSE))
complexity_plot <- as.vector(unlist(complexity_result, use.names = FALSE))

rmse_result_test <- stepmodel_test$results['RMSE']
complexity_result <- stepmodel_test$results['nvmax']

rmse_plot_test <- as.vector(unlist(rmse_result_test, use.names = FALSE))
complexity_plot <- as.vector(unlist(complexity_result, use.names = FALSE))

'1.'
plot(complexity_plot, rmse_plot_train)
lines(complexity_plot, rmse_plot_test, type = 'b', col='darkorange')

'2.'
# predicting sales price
model_test <- train(SalePrice ~ ., data=test_data,
                     method='leapForward',
                     tuneGrid=data.frame(nvmax=21),
                     trControl= traincontrol)

sales_prediction <- predict(model_train)
# sales prediction mean : $183703.3
# actual saleprice mean : $185506.2
# used 21 indepndent variables / predictors

# rmse for train and test
model_train <- train(SalePrice ~ ., data=train_data,
                    method='leapForward',
                    tuneGrid=data.frame(nvmax=21),
                    trControl= traincontrol)
rmse_train <- model_train$results['RMSE']
# train rmse = 48348.24
rmse_test <- model_test$results['RMSE']
# test rmse = 38932.07







