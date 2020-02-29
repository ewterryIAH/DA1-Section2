library(tidyverse)
library(psych)

rmse <- function(error)
{
  sqrt(mean(error^2))
}

insurance <- read.csv("C:/Users/ellen/OneDrive/Documents/Spring 2019/DA1/Project/insurance.csv", stringsAsFactors = TRUE)

pairs.panels(data.matrix(insurance))
Insurance.pca <- prcomp(data.matrix(insurance[-7]), retx=TRUE, center=TRUE, scale.=TRUE) 
biplot(Insurance.pca, cex=c(.3,1.2))


insurance <- rowid_to_column(insurance, var="SampleID") # this creates a primary key for sampling
xTrain <- sample_frac(insurance, .6)
xTest <- anti_join(insurance, xTrain, by = "SampleID")

mod1 <- lm(expenses ~ age + children + bmi + sex + smoker + region,
                data = xTrain)
summary(mod1)
xTest$Pred <- predict(mod1, xTest)
# compare the correlation
cor(xTest$expenses, xTest$Pred)
rmse(mod1$residuals)
rmse(xTest$expenses - xTest$Pred)
# keep in mind that the model residuals are just an estimate!!
summary(xTrain$expenses)

# now let's try different models and see how they fare!

mod2 <- lm(expenses ~ age + bmi + sex + smoker, data = xTrain)
rmse(mod2$residuals)
mod3 <- lm(expenses ~ age + bmi + sex + smoker + bmi*smoker, data = xTrain)
rmse(mod3$residuals)
mod4 <- lm(expenses ~ age + bmi + sex + smoker + bmi*smoker + I(age^2), data = xTrain)
rmse(mod4$residuals) # not that big an improvement

ggplot(insurance, aes(age, expenses)) + geom_point() + geom_smooth()



