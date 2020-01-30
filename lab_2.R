library(ggplot2)
ames_data <- read.table('ames.csv', header = TRUE, sep=',')
# ex1
Ames <- ames_data[, sapply(ames_data, is.integer)]
Ames_final <- na.omit(Ames)


# scatter matrix

#pairs(Ames_final[1 : 12])
# not helpful at all, google had little to no answers and documentation
# was a mess


# corr matrix
cor(Ames_final, Ames_final$SalePrice)
'
Discussion:
Most of the corr values match our prior beliefs such as :
  - A positive correlation for Full bath, Lot area, and TotalBsmntSF
  
The one corr value we found odd was 
- OverallCond has a negative corr value
'

# scatter plot between SalePrice and GrLivArea fit with line
plot(SalePrice ~ GrLivArea, data=Ames_final)

abline(lm(SalePrice ~ GrLivArea, data=Ames_final))

# the largest outlier is at x ~ 4200, y ~ 7e + 05

# Ex 2

garage_reg <- lm(SalePrice ~ GarageArea, data=Ames_final)
# the value of a garage is $268.8

multiple_reg <- lm(SalePrice ~ MSSubClass + LotFrontage + LotArea + OverallQual + OverallCond + YearBuilt + YearRemodAdd + MasVnrArea + BsmtFinSF1 + BsmtFinSF2 + 
                     BsmtUnfSF + X1stFlrSF + X2ndFlrSF +
                     LowQualFinSF + BsmtFullBath + BsmtHalfBath +
                     FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr +
                     TotRmsAbvGrd + Fireplaces + GarageYrBlt + GarageCars + GarageArea+
                     WoodDeckSF + OpenPorchSF + EnclosedPorch + X3SsnPorch +
                     ScreenPorch + PoolArea + MiscVal + MoSold + YrSold , data=Ames_final)

# yes, there are relationships present between the predictors and the response
'Statisticallt significant predictors include (at 5% level):
MSSubClass, LotArea, OverallQual, OverallCond, YearBuilt,
MasVnrArea, BsmtFinSF1, X1stFlrSF, X2ndFlrSF, BsmtFullVath, 
BedroomAbvGr, KitchenAbvGr, TotRmsAbvGrd, Fireplaces, GarageCars,
WoodDeckSF, ScreenPorch, PoolArea
'
# the coefficient for the year variable implies that for every additional year the house was built (newer house), we would expect the sales price to increase by apporximately 3.170e+02

a <- plot(multiple_reg)

# we do not see any glaring errors with the fit, only that there are a few outliers
# The farthest outliers from the fit at at 1183, 524, and 1299
# put leverage plot answer here





