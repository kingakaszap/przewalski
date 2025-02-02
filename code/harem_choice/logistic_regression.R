# 20 feb

library(tidyverse)
library(caret)

# b0 and b1 - regression beta coefficients
# positive b1: increasing x associated with increasing p
# negative b1: increasing x associated with decreasing p

# odds: likelihood that an event will occur

theme_set(theme_bw())

# lr works for data w continuous/categorical expl. variables
# expl variables normally distributed?
# remove highly correlated predictors to minimize overfitting
# -> ask about foal/no foal& pzp - surely correlated...

data("PimaIndiansDiabetes2", package = "mlbench")
PimaIndiansDiabetes2<- na.omit(PimaIndiansDiabetes2)
sample_n(PimaIndiansDiabetes2, 3)
# split data into training and test set
set.seed(123)
training.samples <- PimaIndiansDiabetes2$diabetes %>% 
  createDataPartition(p = 0.8, list = FALSE)

train_data<- PimaIndiansDiabetes2[training.samples, ]
test.data <- PimaIndiansDiabetes2[ -training.samples, ]

#general lr??
model <- glm(diabetes ~., data = train_data, family = binomial)
summary(model)
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse (probabilities >0.5, "pos", "neg")
mean(predicted.classes == test.data$diabetes)

# simple lr (?) - predict class membership based on 1 predictor variable
model <- glm( diabetes ~ glucose, data = train_data, family = binomial)
summary(model)$coef
# model predicts PROBABILITY of being diabetes positive based on
# plasma glucose concentration

newdata <- data.frame(glucose = c(20,180))
probabilities <- model %>%  predict(newdata, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
predicted.classes
# i dont get this boo

train_data %>% 
  mutate(prob = ifelse(diabetes == "pos", 1,0)) %>% 
  ggplot(aes(glucose, prob))+
  geom_point(alpha = 0.2)+
  geom_smooth(method = "glm", method.args = list(family = "binomial"))+
  labs (
  #  title = "Logistic Regression Model", 
    x = "Plasma Glucose Concentration",
    y = "Probability of being diabetes - pos")
 
# multiple logistic regression
model <- glm(diabetes ~ glucose + mass + pregnant,
             data = train_data, family = binomial)
summary(model)$coef
# what do the coefficients mean? does estimate show the direction
# of the relationship, ie whether the var increases or decreases
# probability of the response var?

model <- glm(diabetes ~., data = train_data, family = binomial) # to include all variables
summary(model)$coef
coef(model)

# odds ratio
# coef for glucose: 3.7
# 1 unit increase in glucose concentration will increase
# odds of being diabetes positive by exp(3.7) times

# selecting most significant predictors
model <- glm( diabetes ~ pregnant + glucose + pressure + mass + pedigree,
              data = train_data, family = binomial)
# make predictions using the test data to evaluate model performance
probabilities <- model %>% predict(test.data, type = "response")
head(probabilities)
# i am not sure what this is.
contrasts(test.data$diabetes)
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)
# i dont get it

# assessing model accuracy
# proportion of correctly ? classed observations
mean(predicted.classes == test.data$diabetes)
