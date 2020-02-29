library(DBI)
library(tidyverse)
library(odbc)
library(stringr)
library(matrixcalc)
library(reshape2)
library(car)
library(psych)
library(ISLR)


rmse <- function(error)
{
  sqrt(mean(error^2))
}

Advertising <-  dbGetQuery(con2,"
                           SELECT 
                           [TV]
                           ,[Radio]
                           ,[Newspaper]
                           ,[Sales]
                           FROM [dbo].[Advertising]
                           ")

mFit1 <- lm(Sales ~ TV + Radio, data = Advertising)
summary(mFit1)
rmse(mFit1$residuals) # using this here will let us compare insample to outofsample rmse


# adding an interaction term

mFit2 <- lm(Sales ~ TV + Radio + (TV*Radio), data = Advertising)
summary(mFit2)
rmse(mFit2$residuals) # using this here will let us compare insample to outofsample rmse


# now using a different dataset


dfCredit <- Credit

mFit1 <- lm(Balance ~ Income + Student, data = dfCredit)

# fit model using lm 

tst <- model.matrix(Balance ~ Income + Student, data = dfCredit)

summary(mFit1)
mFit1$coefficients
rmse(mFit1$residuals) # using this here will let us compare insample to outofsample rmse

ggplot(dfCredit, aes(x = Income, y = Balance)) +
  geom_point(alpha = .5, aes(color = Student)) + 
  geom_abline(data = dfCredit, aes(intercept = mFit1$coefficients[1], slope = mFit1$coefficients[2]), color = "blue") +
  geom_abline(data = dfCredit, aes(intercept = mFit1$coefficients[1]+mFit1$coefficients[3], slope = mFit1$coefficients[2]), color = "red") +
  theme(legend.position="none")  +
  theme(panel.background = element_rect(fill = "white"))

# now with interaction effect

mFit2 <- lm(Balance ~ Income + Student + (Income * Student), data = dfCredit)
summary(mFit2)

tstng <- model.matrix(Balance ~ Income + Student + (Income * Student), data = dfCredit)


mFit2$coefficients
rmse(mFit2$residuals) # using this here will let us compare insample to outofsample rmse

Intercept <- coef(mFit2)["(Intercept)"]
Income <- coef(mFit2)["Income"]
StudentYes <- coef(mFit2)["StudentYes"]
IncomeStudentYes <- coef(mFit2)["Income:StudentYes"]

ggplot(dfCredit, aes(x = Income, y = Balance)) +
  geom_point(alpha = .5, aes(color = Student)) + 
  geom_abline(Intercept, slope = Income, color = "blue") +
  geom_abline(intercept = Intercept+StudentYes, slope = Income+IncomeStudentYes, color = "red") +
  theme(legend.position="none")  +
  theme(panel.background = element_rect(fill = "white"))


contrasts(dfCredit$Student)
