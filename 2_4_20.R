# bias-variance tradeoff
'
Why use squared error over median(distance):
    - historical
    - non-continuous
    - protects against large deviations 

Reducible and irreducible error:
    - EPE : expected prediction error
        * predicting y using g(x). good g will have a low expected prediction error
        * reducible error + irreducible error(comes from variance within data)
            - reducible error called mean sq error
            - irreducible is also called bayes error
        * depends on sample data

    - How do we control reducible error?
        * bias of estimator bias(b1) = E(b1) - b1
        * variance of the estimator var(b1) = E[(b1 - e[b1])^2]

    - bias-variance tradeoff:
        * the more bias in our estimation, the lesser the variance
        * less variance, more bias
        * complex models tend to be unbiased, but highly variable

    - models are variable when:
        * parametric:
            - form of the model includes too many variables
        * non-parametric:
            - model does not provide enough smoothing

    - 0 predictor model is clearly wrong and biased
        * almost no variance

    - 9 predictor model is too varied
        * low bias
        * on average this model is very good
        * correct on avg is no a sensable goal
    
    - *apply() function:
        * sapply
        * tidyverse gather 
    
    - as complexity increases, bias decreases
    - as complexity increases, variance increases
'
# This code included solely for completeness; you do not need to execute this.
x = seq(0, 100, by = 0.001)
f = function(x) {
  ((x - 50) / 50) ^ 2 + 2
}
g = function(x) {
  1 - ((x - 50) / 50)
}

par(mgp = c(1.5, 1.5, 0))
plot(x, g(x), ylim = c(0, 3), type = "l", lwd = 2,
     ylab = "Error", xlab = expression(Low %<-% Complexity %->% High),
     main = "Error versus Model Complexity", col = "darkorange",
     axes = FALSE)
grid()
axis(1, labels = FALSE)
axis(2, labels = FALSE)
box()
curve(f, lty = 6, col = "dodgerblue", lwd = 3, add = TRUE)
legend("bottomleft", c("(Expected) Test", "Train"), lty = c(6, 1), lwd = 3,
       col = c("dodgerblue", "darkorange"))


# This code included solely for completeness; you do not need to execute this.
x = seq(0.01, 0.99, length.out = 1000)

par(mfrow = c(1, 3))
par(mgp = c(1.5, 1.5, 0))

b = 0.05 / x
v = 5 * x ^ 2 + 0.5
bayes = 4
epe = b + v + bayes

plot(x, b, type = "l", ylim = c(0, 10), col = "dodgerblue", lwd = 2, lty = 3,
     xlab = "Model Complexity", ylab = "Error", axes = FALSE,
     main = "More Dominant Variance")
axis(1, labels = FALSE)
axis(2, labels = FALSE)
grid()
box()
lines(x, v, col = "darkorange", lwd = 2, lty = 4)
lines(x, epe, col = "black", lwd = 2)
abline(h = bayes, lty = 2, lwd = 2, col = "darkgrey")
abline(v = x[which.min(epe)], col = "grey", lty = 3, lwd = 2)

b = 0.05 / x
v = 5 * x ^ 4 + 0.5
bayes = 4
epe = b + v + bayes

plot(x, b, type = "l", ylim = c(0, 10), col = "dodgerblue", lwd = 2, lty = 3,
     xlab = "Model Complexity", ylab = "Error", axes = FALSE,
     main = "Decomposition of Prediction Error")
axis(1, labels = FALSE)
axis(2, labels = FALSE)
grid()
box()
lines(x, v, col = "darkorange", lwd = 2, lty = 4)
lines(x, epe, col = "black", lwd = 2)
abline(h = bayes, lty = 2, lwd = 2, col = "darkgrey")
abline(v = x[which.min(epe)], col = "grey", lty = 3, lwd = 2)

b = 6 - 6 * x ^ (1 / 4)
v = 5 * x ^ 6 + 0.5
bayes = 4
epe = b + v + bayes

plot(x, b, type = "l", ylim = c(0, 10), col = "dodgerblue", lwd = 2, lty = 3,
     xlab = "Model Complexity", ylab = "Error", axes = FALSE,
     main = "More Dominant Bias")
axis(1, labels = FALSE)
axis(2, labels = FALSE)
grid()
box()
lines(x, v, col = "darkorange", lwd = 2, lty = 4)
lines(x, epe, col = "black", lwd = 2)
abline(h = bayes, lty = 2, lwd = 2, col = "darkgrey")
abline(v = x[which.min(epe)], col = "grey", lty = 3, lwd = 2)
legend("topright", c("Squared Bias", "Variance", "Bayes", "EPE"), lty = c(3, 4, 2, 1),
       col = c("dodgerblue", "darkorange", "darkgrey", "black"), lwd = 2)


