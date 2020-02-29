library(tidyverse)  
library(tidyr)
library(ggplot2)
library(e1071)

setwd("C:/Users/ellen/OneDrive/Documents/Spring 2019/DA1/Section I/Blackboard/Blackboard/Section2/Data")

Autos <- read_csv(file="Automobile Price Prediction.csv")
p <- ggplot(Autos, aes(x=horsepower, y=price))+geom_point() + 
  geom_smooth(method = "lm", se = F) +
  theme(panel.background = element_rect(fill = "white"))
p

model2 <- lm(price ~ horsepower + make, Autos)
summary(model2)
model2$coefficients[1]

tst <- model.matrix(price ~ horsepower + make, Autos)


Intercept <- coef(model2)["(Intercept)"] # alpha romeo, btw
BMW <- coef(model2)["makebmw"]
Honda <- coef(model2)["makehonda"]
Slope <- coef(model2)["horsepower"]

# BMW - you take the overall coefficients and add the coefficients associated with the dummy varibles
p <- ggplot(Autos, aes(x=horsepower, y=price)) + geom_point() +
  geom_abline(intercept = (Intercept + BMW) , slope = Slope, color = 'blue') +
  theme(panel.background = element_rect(fill = "white"))
p

# Honda

p <- p + geom_abline(intercept = (Intercept + Honda) , slope = Slope, color = 'red')
p

coefMat <- summary(model2)$coefficients

AllMakes # just for reference
p <- p + geom_abline(intercept = mean(coefMat[1,1] + coefMat[3:22,1]) , slope = Slope, color = 'green')
p


tst <- model.matrix(price ~ horsepower + make, Autos)


# get dummy variables - just FYI

contrasts(as.factor(Autos$make))

