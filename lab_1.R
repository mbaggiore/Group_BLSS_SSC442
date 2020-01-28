library('tidyverse')
library('ggplot2')

# load in data
data = mpg

### Exercise 1

# scatter for hwy, displ
displ_hwy_scatter <- ggplot(data = data,
                            mapping = aes(x = displ, y = hwy)) +
  geom_point()

# yes, this scatter maps an intuitive relationship that larger cars get worse gas mileage


# scatter for class, drv
class_drv_scatter <- ggplot(data = data,
                            mapping = aes(x = class, y = drv))+
  geom_point(color='red')

# this plot is not useful because there is no relationship between these two variables that a scatter would accurately show, rather the relationship between these two variables is a classification


# this group of data points falls outside of the relationship becasue the cars are most likely hybrids

# EXERCISE 1B
displ_hwy_scatter_colored <- ggplot(data = data,
                            mapping = aes(x = displ, y = hwy, color=class)) +
  geom_point()

# we can conclude that the outlier group is in fact 2seater, and not hybrids as we may have thought


### Excercise 2

bank_data <- read.table('bank.csv')
# default = if customer has credit in default
# housing and loan describe if customer has mortgage or other loan already wiht bank

# provide two visuals for making decisions on this data set

displ_age_job <- ggplot(data = bank_data,
                mapping = aes(x = age, y = job, color=y)) +
  geom_point()


displ_age_marr<- ggplot(data = bank_data,
                        mapping = aes(x = age, y = marital, color=y)) +
  geom_point()


