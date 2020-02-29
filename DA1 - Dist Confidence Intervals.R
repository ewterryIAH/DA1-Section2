x2 <- data.frame(x=rnorm(10000, 10, 1))
n <- 100
plotData <- matrix(nrow = n, ncol = 4)
for(i in 1:100)
{
  samplen <- sample_n(x2, n)
  sampMeann <- mean(samplen$x)
  sampSDn <- sd(samplen$x)
  error1 <- qnorm(0.975)*sampSDn/sqrt(n)
  left1 <- sampMeann-error1
  right1 <- sampMeann+error1
  plotData[i,] <- c(i, sampMeann, left1, right1 ) 
}

dfPlotData2 <- data.frame(plotData)

ggplot(dfPlotData2, aes(x=X2)) + geom_point(aes(y = X1), color = 'blue') +
  geom_errorbarh(aes(xmin=X3, xmax=X4, y = X1), height = 1, color = "red") +
  labs(x = 'Confidence Intervals', y = 'Sample Number')

n <- 5
samplen <- sample_n(x2, n)
sampleMin <- min(samplen$x)
sampleMax <- max(samplen$x)
sampMeann <- mean(samplen$x)
sampSDn <- sd(samplen$x)
error1 <- qnorm(0.025)*sampSDn/sqrt(n)
left1 <- sampMeann-error1
right1 <- sampMeann+error1
ggplot(samplen, aes(x=samplen$x)) + geom_density(bw = .5) +
geom_errorbarh(aes(xmin=left1, xmax=right1,height = .02, y = 0), color = "red") 
t.test(samplen$x)  


# bayes

df2 <- density(samplen$x)
x2 <- data.frame(x = df2$x, y = df2$y)
ggplot(x2, aes(x,y)) + geom_line()

capMu <- matrix( nrow = 0, ncol = 3)

NLL <- function(theta,data) { 
  mu = theta[1] 
  sigma = theta[2] 
  n = length(data)
  t <- n
  NLL = -(n/2)*log(2*pi) - (n/2)*log(sigma**2) 
  tmp = 0 
  for (i in 1:n) { 
    tmp = tmp + (data[i]-mu)**2 
  }
  NLL = NLL + -(1/(2*(sigma**2)))*tmp 
  capMu <<- rbind(capMu, c(NLL, mu, sigma))
  -NLL 
}

sampleMn <- mean(samplen$x)
sampleSd <- sd(samplen$x)

out = optim(par=c(sampleMn,sampleSd), fn=NLL, data=samplen$x) 
out$par

