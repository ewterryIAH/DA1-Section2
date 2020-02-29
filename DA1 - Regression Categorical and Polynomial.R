library(tidyverse)  
library(tidyr)
library(e1071)
library(ISLR)
library(psych)

rmse <- function(error)
{
  sqrt(mean(error^2))
}

dfMPG <- Auto

p <- ggplot(data = dfMPG) +   
  geom_point(mapping = aes(x = horsepower, y = mpg)) +  
  geom_smooth(method = lm, mapping = aes(x = horsepower, y = mpg), col = "red", se=F) +
  theme(
    panel.background = element_rect(fill = "white") # I do this so it's easier to see in  class
  )
p

# BTW, this is a slightly different dataset from the one we used in confidence intervals

# pull out the model

dfMPG <- select(dfMPG, mpg, name, horsepower)
dfMPG$make <- factor(str_split_fixed(dfMPG$name, " ", 4)[,1])
dfMPG$name <- NULL


dfMPG.pca <- prcomp(data.matrix(dfMPG[-1]), retx=TRUE, center=TRUE, scale.=TRUE) 
dfMPG.pca
plot(dfMPG.pca)
biplot(dfMPG.pca, cex=c(.3,1.2))

# we're going to simplify so we can focus on a couple of issues
# dropping all but make and horsepower

mod1 <- lm(mpg ~ make + horsepower, data = dfMPG)
summary(mod1)
rmse(mod1$residuals)
mod1$residuals

Intercept <- coef(mod1)["(Intercept)"]
Audi <- coef(mod1)["makeaudi"]
Honda <- coef(mod1)["makehonda"]
Ford <- coef(mod1)["makeford"] 
Horsepower <- coef(mod1)["horsepower"]


p <- ggplot(data = dfMPG) +   
  geom_point(mapping = aes(x = horsepower, y = mpg)) +  
  geom_abline(intercept = Intercept+Audi, slope = Horsepower) +
  geom_abline(intercept = Intercept+Honda, slope = Horsepower, color = "blue") +
  geom_abline(intercept = Intercept+Ford, slope = Horsepower, color = "red") +
  theme(
    panel.background = element_rect(fill = "white") # I do this so it's easier to see in  class
  )
p


# note how the shape of the data is non-linear and there is consistent shape
# so we're going to test a polynomial model

mod2 <- lm(mpg ~ make + horsepower + I(horsepower^2), data = dfMPG)
rmse(mod2$residuals)
summary(mod2)

dfMPG$predict <- predict(mod2, dfMPG)

p <- ggplot(data = dfMPG) +   
  geom_point(mapping = aes(x = horsepower, y = mpg)) +  
  geom_point(mapping = aes(x = horsepower, y = predict), color = "red") +
  geom_smooth(mapping = aes(x = horsepower, y = predict), color = "red", se = F) +
  theme(
    panel.background = element_rect(fill = "white") # I do this so it's easier to see in  class
  )
p

# ok let's pull the equation out and work with that


p <- ggplot(data = dfMPG) +   
  geom_point(mapping = aes(x = horsepower, y = mpg)) +  
  theme(
    panel.background = element_rect(fill = "white") # I do this so it's easier to see in  class
  )
p

Intercept <- coef(mod2)["(Intercept)"]
Audi <- coef(mod2)["makeaudi"]
Horsepower <- coef(mod2)["horsepower"]
Phorsepower <- coef(mod2)["I(horsepower^2)"]

Intercept + Audi

# here's the equation using Audi
dfMPG$pPredict <- (Intercept + Audi) + (Horsepower*dfMPG$horsepower) + (Phorsepower * (dfMPG$horsepower^2))

p <- p + geom_point(data = dfMPG, aes(x = horsepower, y = pPredict), color = "red")
p


rmse(dfMPG$mpg - dfMPG$pPredict)


# what's the value fo y @ x = 100?
x <- 100
y <- (53) + (-.4*x) + (.001 * (x^2))

