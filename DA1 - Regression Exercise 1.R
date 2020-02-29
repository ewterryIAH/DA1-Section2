library(tidyverse)

rmse <- function(error)
{
  sqrt(mean(error^2))
}

set.seed(1105)

setwd("C:/Users/ellen/OneDrive/Documents/Spring 2019/DA1/Section I/Blackboard/Blackboard/Section2/Data")
Autos <- read.csv(file="Automobile Price Prediction.csv")

# this elimiates makes with less than 2 obs - more on stratified sampling in DA2
Autos <- rowid_to_column(Autos, var="SampleID") # this creates a primary key for sampling
tst <- Autos %>% group_by(make) %>% dplyr::mutate(cnt = n())
by_MakeStyle <- Autos %>% group_by(make) %>% dplyr::mutate(cnt = n()) %>% filter(cnt > 2) 

xTrain <- sample_frac(by_MakeStyle, .8)
xTest <- anti_join(by_MakeStyle, xTrain, by = "SampleID")

filter(xTrain %>% distinct(make)) %>% anti_join(filter(xTest %>% distinct(make)), by = "make") %>% nrow()
 
# chart out the data in terms of horsepower

p <- ggplot(Autos, aes(x=horsepower, y=price))+geom_point() 
p

# run a single variable linear regression

model1 <- lm( price ~ horsepower, xTrain)
xTest$pred1 <- predict(model1, xTest)

# chart the single variable LR and summarize performance / error

p <- p + geom_point(data=xTest, aes(horsepower, pred1 ), color = "blue")  + 
  geom_smooth(data=xTest, aes(horsepower, pred1), se=FALSE, color = "blue")
p

summary(model1)
modRMSE1 <- rmse(model1$residuals)
modRMSE1

# Now use a multivariate regression model

model2 <- lm( price ~ make + horsepower, xTrain)
xTest$pred2 <- predict(model2, xTest)

# chart the MV model too, and compare statistics

p <- p + geom_point(data=xTest, aes(horsepower, pred2), color = "red") + 
  geom_smooth(data=xTest, aes(horsepower, pred2), se=FALSE, color = "red")
p

summary(model2)
modRMSE2 <- rmse(model2$residuals)
modRMSE2

Intercept <- coef(model2)["(Intercept)"]
Horsepower <- coef(model2)["horsepower"]
Honda <- coef(model2)["makehonda"]

p <- p + geom_abline(intercept = Intercept+Honda, slope = Horsepower, color = "green")
p

# model for honda:

x <- 76
y <- (2742.593 - 2743.667) + 103.8295 * x
y
# confirm

test3 <- filter(xTest, make == "honda" & horsepower == 76)

predict(model2, test3[1,])

# how much horsepower can I buy for 20,000 with a honda

y <- 20000
x <- (y - (2742.593 - 2743.667))/103.8295
x

