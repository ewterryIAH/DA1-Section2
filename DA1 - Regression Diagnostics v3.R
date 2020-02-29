library(tidyverse)
library(ggplot2)
library(MASS)
library(car)

setwd('C:/Users/ellen/OneDrive/Documents/Fall 2019/DA1/Section2/Data')

Auto <- read.csv(file="Automobile Price Prediction.csv")

p <- ggplot(Auto, aes(x=horsepower, y=price))+geom_point() 
p

fit <- lm(price ~ horsepower, data = Auto)

#fit <- lm(price ~ horsepower + engine.size + highway.mpg, data = Auto)

# ----------------- test for normality ---------------------------------#

fit.res <- data.frame(resid(fit)) 
ggplot(data = fit.res, aes(resid.fit.)) +
  geom_density(bw = 5000)
result <- shapiro.test(fit.res$resid.fit.)
result$p.value

ggplot(fit.res, aes(resid.fit., ..density..)) + geom_freqpoly(binwidth = 5000)
ggplot(fit.res, aes(resid.fit., ..density..)) + geom_histogram(binwidth = 500)
ggplot(fit.res, aes(sample = resid.fit.)) + stat_qq()
ggplot(fit.res, aes(resid.fit.)) + stat_ecdf()

res <- sort(fit.res$resid.fit.)
res <- res[!is.na(res)]
n = length(res)

probabilities <- (1:n)/(n+1)

# the qnorm fuction creates the theoretical quantities 

Quantities <- qnorm(probabilities, mean(res, na.rm = T), sd(res, na.rm = T))

dfProb <- data.frame(cbind(Quantities, res))

p <- ggplot(dfProb, aes(x=Quantities, y = res)) + geom_point()
p
p <- p + geom_abline(intercept = 0, slope = 1)
p

# ------------  test outliers ---------------#

outlierTest(fit)
# remember bonferonni from the audit exercise?
outliers <- data.frame(outlierTest(fit)[2])
leveragePlots(fit)
Auto[66,]

# is this really an outlier? No!

# ------------  Influential Observations  ---------------#

avPlots(fit)
cutoff <- 4/((nrow(Auto) - length((fit$coefficients)-2)))
plot(fit, which = 4, cook.levels = cutoff)

influencePlot(fit, id.method = 'identity', main = 'Influence Plot', sub = 'Circle size is proportial to Cooks Distance')

# ------------  Non-normaility again  ---------------#


# skip the rest of this !!!


# Normality of Residuals
# qq plot for studentized resid
qqPlot(fit, main="QQ Plot")
# distribution of studentized residuals
sresid <- studres(fit) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit) 

# -----------------------  non-constant error variance ---------------#
  
# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(fit)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(fit)


# ------------------- multi-collinearity ----------------------#

# Evaluate Collinearity
vif(fit) # variance inflation factors 
sqrt(vif(fit)) > 2 # problem?

# ------------------- Nonlinearality ----------------------#

# Evaluate Nonlinearity
# component + residual plot 
crPlots(fit)

# ------------------- Non-independence of Errors ----------------------#

# Test for Autocorrelated Errors
durbinWatsonTest(fit)



