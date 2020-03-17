library(ggplot2)
df <- read.table('ames.csv', sep = ',')

ran <- sample(1 : nrow(df), .9 * nrow(df))

nor <- function(x){
  return (x - min(x)) / (max(x) - min(x))
}
norm <- as.data.frame(lapply(df[, c(c(range(56)), c(range(58, 81)))], nor))


set.seed(123)
dat.d <- sample(1:n)