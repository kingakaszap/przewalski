# binomial data

library(lme4)
library("MuMIn")
data <- read.csv("binomial.csv", header = TRUE, sep = " ")
read.csv()

# does a logistic regression model need one of the explanatory 
# variables to be continuous?

# can i just have foal/not foal
# if not, can include age as well in the model.

# logistic regression tutorial - intro2r

# response variable: only 0/1
# r treats such data as if each row came from a binomial trial
# with sample size 1. 
# If you have unique values of one or more explanatory variables 
# for each and every individual case, 
# then a model with a binary response variable will work. 
# If not, you should aggregate the data 
# until you do have such explanatory variables
# (e.g., at the level of the household, town, state, population, …).

# incidence: bird present or not
# area: size of island
# distance: dist from the mainland

mod1 <- glm(incidence~ area*distance, family = binomial, data = data)
mod2 <- glm(incidence~ area + distance, family = binomial, data = data)
anova(mod1, mod2, test = "Chi")
AICc(mod1, mod2)

summary(mod2)

# the coefficient estimates give the change in log odds (logit)

# continuous predictors: coefficients give the change
# in the log odds of the outcome for a 1 unit increase in
# the predictor variable. 
# categorical predictors: coef. estimate describes the change in the 
# log odds for each level,
# compared to the base level. 

confint(mod2)

# converting the logits to either odds ratios
# or predicted probabilities

# odds ratios
# a relative measure of effect, allowing comparisons of 2 groups
# for categorical predictors:
# >1 : a positive diff. between levels and the base level
# < 1 : negative difference
# continuous predictors: odds ratio is the odds of x+1 over x
# the odds ratio for a unit increase in the predictor

exp(coef(mod2))

# predicted probabilities

data$logit <- predict(mod2, type = "link")
data$p <- predict(mod2, type = "response")

# plotting

# 1 - using the original model
# generating seq of x's from which to make predictions
x.seq <- seq(from = 0, to = 9, by = 0.1)

xv3 <- data.frame(area = c(x.seq, rep(mean(data$area), times = length(x.seq))),
                  distance = c(rep(mean(data$distance), times = length(x.seq)),
                  x.seq))
head(xv3)

xv3$y <- predict(mod2, newdata= xv3, type = "response")
head(xv3)

# 2 - generating two separate logistic regressions

modela <- glm(incidence ~ area, family = binomial, data = data)
modeli <- glm(incidence ~ distance, family = binomial, data = data)

# create a sequence of x values  
xv <- seq(from = 0, to = 9, by = 0.01)

yv <- predict(object = modela, newdata = list(area = xv), type = "response")

# set up a two-panel plotting area
par(mfrow = c(1, 2))

## 1. area  
# plot the raw data
plot(data$area, data$incidence)

# use the lines function to add the fitted lines
lines(xv, yv)

## 2. Now, repeat for distance
xv2 <- seq(0, 10, 0.1)
yv2 <- predict(modeli, list(distance = xv2),type = "response")
plot(data$distance, data$incidence)
lines(xv2, yv2)
