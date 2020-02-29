library(tidyverse)

set.seed(2)

popN <- 10000
popMean <- 10
popSD <- 2
x <- data.frame(x=rnorm(popN, popMean, popSD))
mean(x$x)
summary(x$x)

n <- 1000
sample <- sample_n(x, n)
sampMean <- mean(sample$x)
sampMean
sampSD <- sd(sample$x)
sampSD
summary(sample$x)

p1 <- ggplot(data = sample, aes(x=x)) + 
  geom_density(color = 'blue', bw =2)  + 
  geom_vline(xintercept = sampMean, color = "blue") +
  xlim(0, 20) +
  theme(panel.background = element_rect(fill = "white")) 
p1


# OK, now our experiment

pop2N <- 10000
pop2Mean <- 10.25
pop2SD <- 2
x2 <- data.frame(x=rnorm(pop2N, pop2Mean, pop2SD))
mean(x2$x)
summary(x2$x)

n <- 500
sample2 <- sample_n(x2, n)
sampMean2 <- mean(sample2$x)
sampMean2
sampSD2 <- sd(sample2$x)
sampSD2

Conssample <- data.frame(cbind(x1 = sample$x, x2 = sample2$x))

p1 <- p1 + geom_density(data = Conssample, aes(x=x2), color = 'red', bw = 2) +
  geom_vline(xintercept = sampMean2, color = "red") 
p1

# so did our experiment create a significant difference?
# the easy way is to compute a "p-value"
# calculate p value

z <- (sampMean2 - sampMean)/(sampSD2/sqrt(n))
z
p <- 2*pnorm(-abs(z))
p

# so the probability that these came from the same population is .6%
# since our alpha was 5%, we say they came from different populations.

# what happens if the sample means were exaclty the same?
# would would be the probability of the difference means?
# what would be the probability they came from the same population?

z <- (sampMean2 - sampMean2)/(sampSD2/sqrt(n))
z
p <- 2*pnorm(-abs(z))
p

# where does this come from? what does it mean?
# let's look at the distribution of the *differences*

sdDiff <- sqrt(4/1000 + 4/500) # add two variances divided by n
meanDiff <- sampMean2 - sampMean 
xDiff <- data.frame(x=rnorm(100, meanDiff, sdDiff)) 
# so what does this look like?


p1a <- ggplot(data = xDiff, aes(x=x)) + 
  geom_density(color = 'blue', bw =.08) +  
  xlim(-.4, .8) +
  theme(panel.background = element_rect(fill = "white")) 
p1a

pnorm(0, mean = meanDiff, sd = sdDiff)
p1a <- p1a + geom_vline(xintercept = 0)
p1a

#In other words, it's unlikely we will draw two samples with no difference from this population
#It's also unlikely we'll draw two samples with a large difference (> .5) from this population

# Or, looking at this on the ORIGINAL SCALE and using confidence intervals --------

stderr <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

er <- stderr(sample$x)
error <- qnorm(0.975)*er # compute the standard error @ 95%

left <- sampMean - error # add to both sides
right <- sampMean + error

p1 <- p1 + 
  geom_vline(xintercept = sampMean, color = "blue") +
  geom_vline(xintercept = left, color = "green") +
  geom_vline(xintercept = right, color = "green") 
p1

# zeroing in  
p1 + xlim(9, 11) 

# let's see how sample size affects p value starting with samples of 100

n <- 100
plotData <- matrix(nrow = 10, ncol = 2)
for(i in 1:10)
{
  n <- i * 100
  samplen <- sample_n(x2, n)
  sampMeann <- mean(samplen$x)
  sampSDn <- sd(sample2$x)
  z <- (sampMean - sampMean2)/(sampSD2/sqrt(n))
  p <- 2*pnorm(-abs(z))
  plotData[i,] <- c(n, p) 
}

dfPlotData <- data.frame(x = plotData[,1], y = plotData[,2])

p2 <- ggplot(data = dfPlotData, aes(x=x, y=y)) + 
  geom_smooth(se = F) +
  theme(panel.background = element_rect(fill = "white")) +
  labs(y = 'P Value', x = 'Sample Size')
p2

# so if I need a p less than .05, what size sample do I need?
# a little more deceptive, how about difference in means
# other side of the mean just for kicks

plotData <- matrix(nrow = 10, ncol = 2)
j <- 1
for(i in seq(9.55,10, by = .05))
{
  n <- 500
  sampSDn <- 2
  sampMean <- i
  p <- 2*pnorm(-abs(10 - sampMean)/(sampSDn/sqrt(n)))
  plotData[j,] <- c(i, p) 
  j <- j + 1
}

dfPlotData <- data.frame(x = plotData[,1], y = plotData[,2])

p2 <- ggplot(data = dfPlotData, aes(x=x, y=y)) + 
  geom_smooth(se = F) +
  theme(panel.background = element_rect(fill = "white")) +
  scale_x_continuous(trans='reverse') +
  labs(y = 'P Value', x = '2nd Pop Means')
p2

# so if I need a .05, what mean does my 2nd trial need to show?
# so what do you think a research who has been working on a paper for 2 years is going to do?
# that's why I put the paper on blackboard.

# let's walk through pval calculation
h0mean <- mean(x$x) # set the h0 mean
# now sample from x2 to get the hA mean

n <- 500
samplen <- sample_n(x2, n)
x2Sampmean <- mean(samplen$x)
x2SampSDn <- sd(samplen$x)
z <- (x2Sampmean - h0mean)/(x2SampSDn/sqrt(n)) 
# so the sample mean is z standard deviations from h0 mean
z # in a normal distribution, remember the 68-95-99.7 rule
p <- 2*pnorm(-abs(z)) # so the p value works out to a significant deviation from the mean
p

mean(x$x)
sd(x$x)
sample1 <- sample_n(x, 100)
mean(sample1$x)
sd(sample1$x)
sample1 <- sample_n(x, 500)
mean(sample1$x)
sd(sample1$x)







# lets try a demo taking samples from the h0 population

n <- 100
plotData <- matrix(nrow = 100, ncol = 2)
for(i in 1:100)
{
  samplen <- sample_n(x, n)
  mean <- mean(samplen$x)
  plotData[i,] <- c(1, mean) 
}

mean(x$x)
sd(x$x)
# 95% conf
sample_n(x, 500)
mean(x$x)
mean(samplen$x)
plotData
dfPlotData <- data.frame(plotData)
interval <- (x2Sampmean - h0mean)



# now let's plot out all those means from the samples
p1 <- ggplot(data = dfPlotData, aes(x=X2)) + geom_histogram(binwidth = .01)  
p1
p1 <- p1 + geom_vline(xintercept =  (h0mean + interval), color = 'red')
p1 <- p1 + geom_vline(xintercept =  (h0mean - interval), color = 'red')
p1
# take a look at how many missed the cut
tstCnt <- dfPlotData %>% filter(X2 < (h0mean - interval) | X2 > (h0mean + interval)) %>% 
  dplyr::summarise(cnt = n())

tstCnt/100

# pre- confidence interval example
# this is the range that we're 95% confident that the mean lies in


# -------------- Confidence Intervals --------------- #


n <- 100
plotData <- matrix(nrow = n, ncol = 4)
for(i in 1:100)
{
  samplen <- sample_n(x2, n)
  sampMeann <- mean(samplen$x)
  sampSDn <- sd(sample2$x)
  error1 <- qnorm(0.975)*sampSDn/sqrt(n)
  left1 <- sampMeann-error1
  right1 <- sampMeann+error1
  plotData[i,] <- c(i, sampMeann, left1, right1 ) 
}

dfPlotData2 <- data.frame(plotData)

ggplot(dfPlotData2, aes(x=X1)) + geom_point(aes(y = X2), color = 'blue')+ 
  geom_errorbar(aes(ymin=X3, ymax=X4), width=1) + 
    geom_hline(yintercept = mean(x2$x), color = 'red') +
    labs(y = 'Confidence Intervals', x = 'Sample Number')

tstCnt <- dfPlotData2 %>% filter(X3 < mean(x2$x) & X4 > mean(x2$x)) %>% summarise(cnt = n())
tstCnt/n

n <- 20
plotData <- matrix(nrow = n, ncol = 4)
for(i in 1:20)
{
  n <- i
  samplen <- sample_n(x2, n)
  sampMeann <- mean(samplen$x)
  sampSDn <- sd(sample2$x)
  error1 <- qnorm(0.975)*sampSDn/sqrt(n)
  left1 <- sampMeann-error1
  right1 <- sampMeann+error1
  plotData[i,] <- c(i, sampMeann, left1, right1 ) 
}

dfPlotData2 <- data.frame(plotData)

ggplot(dfPlotData2, aes(x=X1)) + geom_point(aes(y = X2), color = 'blue')+ 
  geom_errorbar(aes(ymin=X3, ymax=X4), width=1) + 
  geom_hline(yintercept = mean(x2$x), color = 'red') + 
  labs(y = 'Confidence Intervals', x = 'SampleSize')

