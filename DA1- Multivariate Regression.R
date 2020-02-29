library(dplyr)  
library(tidyr)
library(kernlab)
library(ggplot2)
library(e1071)

setwd("C:/Users/ellen/Documents/Spring 2018/DA1/Section2/Data")

Autos <- read.csv(file="Automobile Price Prediction.csv")

# set up training and test splits

indx <- sample(1:nrow(Autos), as.integer(0.6*nrow(Autos)))
indx
xTrain <- Autos[indx,]
xTest <- Autos[-indx,]

# chart out the data in terms of horsepower

p <- ggplot(Autos, aes(x=horsepower, y=price))+geom_point() 
p

# run a single variable linear regression

model1 <- lm( price ~ horsepower, xTrain)
xTest1 <-  dplyr::select(xTest, price, horsepower)
xTest1$newPrice <- predict(model1, xTest1)

# chart the single variable LR and summarize performance / error

p <- p +geom_point() + geom_point(data=xTest1, aes(horsepower, newPrice), color = "blue")  + geom_smooth(data=xTest1, aes(horsepower, newPrice), se=FALSE, color = "blue")
p

summary(model1)
rmse <- function(error)
{
  sqrt(mean(error^2))
}
error <- model1$residuals
modRMSE <- rmse(error)
modRMSE

# Now use a multivariate regression model

model3 <- lm( price ~ horsepower + engine.size, xTrain)
x3Test <- dplyr::select(xTest, price, horsepower, engine.size)
x3Test$newY2 <- predict(model3, x3Test)

# chart the MV model too, and compare statisics

p <- p + geom_point(data=x3Test, aes(horsepower, newY2), color = "green") + geom_smooth(data=x3Test, aes(horsepower, newY2), se=FALSE, color = "green")
p

summary(model3)
error <- model3$residuals
modRMSE <- rmse(error)
modRMSE

# now create a support vector machine regression and do the same

svmModel <- svm(price ~., data=xTrain, cost=100, gamma=10)
pred <- predict(svmModel, xTest)
dfPred <- data.frame(pred)
BaseTest <- Autos[tindexes, ]
dfPred <- cbind(dfPred, BaseTest)

p <- p + geom_point(data=dfPred, aes(horsepower, pred), color = "red") + geom_smooth(data=dfPred, aes(horsepower, pred), se=FALSE, color = "red")
p <- p + theme(panel.background = element_rect(fill = "white"))
p

p <- p + annotate("text", x = 230, y = 30000, label = "SVR", color="blue")
p <- p + annotate("text", x = 230, y = 23000, label = "MVR", color="green")
p <- p + annotate("text", x = 230, y = 20000, label = "SVM", color="red")
p

error <- svmModel$residuals
predictionRMSE <- rmse(error)
predictionRMSE
summary(svmModel)
svmModel$coefs

