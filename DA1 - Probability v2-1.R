library(tidyverse)
library(stringr)
library(Lahman)
library(fitdistrplus)
library(plotly) # this is a new graphics capability in R - very cool stuff
library(stats4)
library(moments)
library(VGAM)
library(cowplot)

#setwd('~/DA1/Section II/Data')
setwd('C:/Users/ellen/OneDrive/Documents/Fall 2019/DA1/Section2/Data')


# ---------------- Probability of Finite Spaces ------------#

outcomes <- read_csv("outcomes.csv")
# prob of any outcome 

outcomes$Prob <- 1/nrow(outcomes)
outcomes$Prob
ggplot(outcomes, aes(x = X1, y = Prob)) + geom_bar(stat = 'identity')
sum(outcomes$Prob)

outcomes$outcome <- outcomes$X1_1 + outcomes$X2
filter(outcomes, outcome == 11)

n <- sum(outcomes$X1_1+outcomes$X2==11, na.rm=TRUE)/nrow(outcomes)
n

# ------------------------- iid ----------------------------#

s1 <- 5
s2 <- 4
n <- 10
A <- sample(1:n,s1,rep=F) 
B <- sample(1:n,s2,rep=F) 
PA <- s1/n
PB <- s2/n
PA*PB

n2 <- 5000
PC <- matrix(NA, nrow = n2, ncol = 1)
for(i in 1:n2)
{
  A <- sample(1:n,s1,rep=F) 
  B <- sample(1:n,s2,rep=F) 
  length(A[A %in% B]) 
  PC[i,1]<- length(A[A %in% B])/n 
}
dfPC <- data.frame(PC)
colnames(dfPC)[1] <- 'x'
mean(dfPC$x)
ggplot(data = dfPC) + 
  geom_density(aes(x = x), bw=.06, fill = 'blue', alpha = .1)

# look at a histogram

ggplot(data = dfPC) + geom_histogram(aes(x = x), bins = 5)

# show counts by result

dfPC %>% group_by(x) %>% summarize(cnt = n())

# ----------------------DISTRIBUTIONS -  Uniform Discrete -------------------#

x <- seq(1, 10, 1)
y <- sample(1:10,length(x),rep=F)
myPlot <- data.frame(x = x, y = y)
ggplot(myPlot,aes(x))+geom_histogram(fill="blue")

den <- cbind(x, data.frame(y = dunif(x, 0, 10)))
ggplot(den, aes(x=x, y=y))+ geom_area(fill="blue")

denCk <- max(den$y)*max(den$x)
denCk
ggplot(myPlot, aes(y)) + stat_ecdf() 


# ----------------------- Binomial Discrete -------------------#

n <- 10
# can't make this too big or you'll have to scale the p's
# just to play with

p <- .5
h <- 5 # sum(y)
h
x <- factorial(n)/(factorial(h)*factorial(n-h))
x*(p)^h*(1-p)^(n-h)


# now let's use dbinom (instead of the formula) 
# and plot a range of probabilities for observing h successes

h <- seq(1,n,by=1)
y <- sample(0:1,length(h),rep=T) 
bdata <- data.frame(x=h, y=y)

py <- dbinom(h,n,.5) # computes the PMF of x vector for n trials with .5 probability of success on each trial
pb <- data.frame(x=h, y = py)
ggplot(data = bdata)+ geom_histogram(aes(x = x, y=y), stat = 'identity')
ggplot(data = pb)+ geom_histogram(aes(x = x, y=y), stat = 'identity')
ggplot(pb, aes(x)) + stat_ecdf() 

# show some binomials with increasing sample size just to get a feel for the CLT

binom5 <- data.frame(Successes=rbinom(n = 10000, size = 5, prob =.8), Size = 5)
binom10 <- data.frame( Successes = rbinom( n = 10000, size = 10, prob = 0.8), Size = 10)
binom100 <-data.frame( Successes = rbinom(n = 10000, size = 100, prob =0.8), Size = 100)
binom1000 <- data.frame( Successes = rbinom( n = 10000, size = 1000, prob = 0.8), Size = 1000)
binomAll <- rbind( binom5, binom10, binom100, binom1000)  
ggplot(binomAll, aes( x = Successes)) + geom_histogram(bins = 30) + facet_wrap(~Size, scales = "free")


# so now let's go back to our pmf and plot it out for a range of h's (# successes)

n <- 10
p <- .5
h <- seq(1, 10, 1)
h
x <- factorial(n)/(factorial(h)*factorial(n-h))
l <- x*(p)^h*(1-p)^(n-h)
ProbMatrix <- data.frame(h, l)
ProbMatrix
ggplot(data = ProbMatrix)+ geom_histogram(aes(x = h, y=l), stat = 'identity')


# reviewing binomial one more time (important)
# now let's run the same with chaning the p instead of h

n <- 10
h <- 5 # sum(success)
x <- factorial(n)/(factorial(h)*factorial(n-h))
p <- seq(.1, 1, .1)
l <- x*(p)^h*(1-p)^(n-h)
ProbMatrix <- data.frame(p, l)
ProbMatrix
ggplot(data = ProbMatrix)+ geom_histogram(aes(x = p, y=l), stat = 'identity')



# we can also drop the constant and generate the matrix again
# the values will be different, but the proportions will be the same

p <- seq(.1, 1, .1)
l <- (p)^h*(1-p)^(n-h)
ProbMatrix <- data.frame(p, l)
ggplot(data = ProbMatrix)+ geom_histogram(aes(x = p, y=l), stat = 'identity')
ProbMatrix
# and if I just want the maximum, it's:
MaxProb <- ProbMatrix[which(ProbMatrix$l==max(ProbMatrix$l)),]
MaxProb

# if you want the probability, you can also multiply times the constant
x*MaxProb


# -------------------- Continuous Distributions --------------------#


career <- Batting %>%
  dplyr::filter(AB > 0) %>%
  anti_join(Pitching, by = "playerID") %>%
  group_by(playerID) %>%
  summarise(H = sum(H), AB = sum(AB)) %>%
  mutate(average = H / AB)

# NOTE replace summarize with summarise (package conflics)!!!!

career <- Master %>%
  tbl_df() %>%
  dplyr::select(playerID, nameFirst, nameLast) %>%
  unite(name, nameFirst, nameLast, sep = " ") %>%
  inner_join(career, by = "playerID") 

career <- mutate(career, BA = round(average*1000, 0))

pBA <- dplyr::filter(career, BA > 100 & BA < 400 )

binwd <- 5

p <- ggplot( data = pBA) + geom_point( aes(x = BA, y = playerID), color = 'blue', alpha = 0.1)
p <- p + theme(axis.text.y=element_blank(), axis.ticks=element_blank(),
               axis.title.y=element_blank())
p

# showing bins with random data

xVec <- seq(min(pBA$BA), max(pBA$BA), by = binwd)
p <- p + geom_vline(xintercept = (xVec))
p

# showing histogram with same bins

p <- ggplot( data = pBA) + geom_histogram( mapping = aes( x = BA), binwidth = binwd, fill = 'blue', alpha = 0.1, col = 'black')
p <- p + theme(axis.text.y=element_blank(),axis.ticks=element_blank(),
               panel.background = element_rect(fill = "white"), 
               axis.title.y=element_blank())
p

# density plot

p <- ggplot( data = pBA) + geom_density(mapping = aes( x = BA))
p

# is this a normal distribution? (note this is plotted against sample values)

ggplot(pBA, aes(sample = BA)) + 
  stat_qq() 

# lets stop and compare density, qq and cdf

p1 <- ggplot(pBA, aes(BA, ..density..)) + geom_freqpoly(binwidth = 5)
p2 <- ggplot(pBA, aes(BA)) + geom_density(bw = 10)
p3 <- ggplot(pBA, aes(sample = BA)) + stat_qq()
p4 <- ggplot(pBA, aes(BA)) + stat_ecdf() # notice how cdf and qq reflect (btw, these are monotonic functions)

plot_grid(p1, p2, p3, p4, align = 'v')


# interpolate density function (i.e., get the estimated function)

#df <- approxfun(density(pBA$BA))

df2 <- density(pBA$BA)
x2 <- data.frame(x = df2$x, y = df2$y)
ggplot(x2, aes(x,y)) + geom_line()

# note that we've now shown a number of ways to get density

# interpolate density function and integrate area for all batters and top batters (over the mean)

allbat <- integrate(approxfun(x2$x,x2$y), lower = 100, upper = 400)
topbat <- integrate(approxfun(x2$x,x2$y), lower = mean(x2$x), upper = 400)

# reality check that it's ~ 50%
topbat$value

# set up data for mapping and show probability
topBatters <- dplyr::filter(x2, x > mean(x)) %>% arrange(x)
topBatters2 <- rbind( c( min( topBatters $ x), 0), 
                      + topBatters, 
                      + c( max( topBatters $ x), 0))

p <- p + geom_polygon( data = topBatters2, aes( x = x, y = y), fill = 'blue', alpha = 0.1)
p <- p + theme(axis.text.y=element_blank(),axis.ticks=element_blank(),
               panel.background = element_rect(fill = "white"), 
               axis.title.y=element_blank())
p

df <- approxfun(x2$x,x2$y)
xnew <- c(200,250,300)
dfPts <- data.frame(pts = df(xnew), x = xnew)

p <- p + geom_point( data = dfPts, aes( x = x, y = pts), color = 'red')
p

# cumulative distribution function

ggplot(pBA, aes(BA)) + stat_ecdf(geom = "step")
P = ecdf(pBA$BA)    
P(250)         
# cdf again
ggplot(pBA, aes(BA)) + stat_ecdf() # notice how it crosses .5 @ 250

set.seed(1)
shapiro.test(rnorm(5000)) # cannot reject null that distribution is normal

shapiro.test(sample(pBA$BA, 50))# cannot reject null that distribution is normal
shapiro.test(sample(pBA$BA, 500))

# BUT, if you increase sample size, then the test will be different
# and normal hypothesis is rejected
# small deviaitions from large samples will throw it off

# ----------- continuous distribution analysis #2 - skewed -----------#


# recall from DA1


UN <- read_csv("UN.csv")

im <- UN$infant.mortality
im <- im[!is.na(im)]
im <- data.frame(im)
ggplot(im, aes(im, ..density..)) + geom_freqpoly(binwidth = 25)
ggplot(im, aes(sample = im)) + stat_qq()
ggplot(im, aes(im)) + stat_ecdf()

dfIM <- UN
USim <- as.numeric( dplyr::filter(dfIM, country == 'United.States') %>% dplyr::select(infant.mortality))
countriesExceeding <- dplyr::filter(dfIM, infant.mortality > USim)
nrow(countriesExceeding)/ nrow(dfIM)


#df3 <- approxfun(density(im$im))
df4 <- density(im$im)
x2 <- data.frame(x = df4$x, y = df4$y)
ggplot(x2, aes(x,y)) + geom_line()

p <- ggplot(x2, aes(x,y)) + geom_line()
p


overUS <- integrate(approxfun(x2$x,x2$y), lower = USim, upper = max(x2$x))
overUS$value

# set up data for mapping and show probability

ce <- dplyr::filter(x2, x > USim) %>% arrange(x)

ce2 <- rbind( c( min( ce $ x), 0), 
              + ce, 
              + c( max( ce $ x), 0))

p <- p + geom_polygon( data = ce2, aes( x = x, y = y), fill = 'blue', alpha = 0.1)
p <- p + theme(axis.text.y=element_blank(),axis.ticks=element_blank(),
               panel.background = element_rect(fill = "white"), 
               axis.title.y=element_blank())
p

# probability of % deaths exceeding US

overUS$value

nrow(countriesExceeding)/ nrow(dfIM) # pretty good crude estimate

# another way to do this (better way)
# estimate using the skew normal package

library(sn)
UNestMod <- sn.mple(y = im$im, opt.method = "nlminb")$cp
UNestParam <- cp2dp(UNestMod, family = "SN")

exi <- UNestParam[1]
eomega <- UNestParam[2]
ealpha <- UNestParam[3]

simUN <- data.frame(im = rsn(nrow(im), xi= exi, omega = eomega, alpha = ealpha ))
ggplot(simUN, aes(im)) + geom_density(bw=20)

# prob of deaths over US (using psn, just like pnorm)
# remember the mode issues we had earlier
# here we have a tie with the US
1 -psn(USim, xi= exi, omega = eomega, alpha = ealpha )
count(filter(simUN, im >= 7))/nrow(simUN) # include countries TIED


# ---- Moments, Maximum Likelihood and Distributin Fitting ---------------#

tstDist <- data.frame(x = rnorm(10000))
p <- ggplot( data = tstDist) + geom_density(mapping = aes( x = x))
p
all.moments(tstDist,order.max=4)

p <- ggplot( data = im) + geom_density(mapping = aes( x = im))
p

all.moments(im,order.max=4)


# ------------------ Distribution Fitting -------------------#

normTst <- data.frame(x = rnorm(1000))
gammaTst<-data.frame(x = rgamma(1000,shape=2, rate=.001)) 


descdist(normTst$x, boot = 1000)

descdist(pBA$BA, boot = 1000)

descdist(gammaTst$x, boot = 1000)

descdist(im$im, boot = 1000)

fw <- fitdist(im$im, "weibull")
fw

fn <- fitdist(normTst$x, "norm")
fn
# -------------------   Maximum Likelihood -------------------------------#

# recall from above how a binomial distribution works

# read logmodels2 in blackboard
# then go to the Probability and Statistics Cheatsheet 
# and get the equations for binomial and normal distributions 
# pmf for binomial is: p^k (1-p)^(n-k)
# therefore, the log of the pmf is: k*log(p) + (n-k)*log(1-p)

N = 10 
K = 4 

# The likelihood function 
L = function(p,k,n) p^k*(1-p)^(n-k) 
# The log-likelihood function 
l = function(p,k,n) k*log(p) + (n-k)*log(1-p) 

mu = seq(0,1,0.001) 

# Plotting the Likelihood function 
bNomLikePlot <- dLogL2<-data.frame(mu, L(mu, K, N))
p <- plot_ly (x = ~ bNomLikePlot$mu, y = ~ bNomLikePlot$L.mu..K..N., type = 'scatter') %>% layout(xaxis = list(title = "mu"), yaxis = list(title = "Likelihood"))
p

# Plotting the Log Likelihood function 
bNomLogLikePlot <- dLogL3<-data.frame(mu, l(mu, K, N))
p <- plot_ly (x = ~ bNomLogLikePlot$mu, y = ~ bNomLogLikePlot$l.mu..K..N., type = 'scatter') %>% layout(xaxis = list(title = "mu"), yaxis = list(title = "Log Likelihood"))
p

# The optimization functions in R finds the minimum, not the maximum. We  
# therefor must create new functions that return the negavive likelihood and  
# log-likelihood, and then minimize these:  
# Minus likelihood:

mL = function(p,k,n) -p^k*(1-p)^(n-k) 

# minus log-likelihood: 

ml = function(p,k,n) -(k*log(p) + (n-k)*log(1-p)) 

# Using 'optimize' 
#  simpler version of optim for one parameter. 
#  we will use optim in the next exercise

mLO <- optimize(mL, interval = c(0,1), k=K, n=N) 
mlO <- optimize(ml, interval = c(0,1), k=K, n=N) 

mLO$minimum
mlO$minimum

# ------------normal distr 2 parameters

x <- rnorm(200, 10, 2)
df2 <- density(x)
x2 <- data.frame(x = df2$x, y = df2$y)
ggplot(x2, aes(x,y)) + geom_line()


# pulling this out for testing -----------------

capMu <- matrix( nrow = 0, ncol = 3)

sigma = 2
n = 200
data = x


NLL = -(n/2)*log(2*pi) - (n/2)*log(sigma**2) 

NLL = -(length(x)/2 * log(2 * pi) - length(x)/2 * log(sigma**2))


# NLL = -(len(x)/2 * np.log(2 * np.pi) - len(x)/2 * np.log(sigma**2))


tmp = 0 
for (i in 1:n) { 
  tmp = tmp + (data[i]-mu)**2 
}
NLL = NLL + -(1/(2*(sigma**2)))*tmp 

#-----

# capMu <- matrix( nrow = 0, ncol = 3)

NLL1 <- function(theta,data) { 
  mu = theta[1] 
  sigma = theta[2] 
#  n = length(data)
#  t <- n
  NLL = -(length(x)/2 * log(2 * pi) - length(x)/2 * log(sigma**2))
#  NLL = -(n/2)*log(2*pi) - (n/2)*log(sigma**2) 
  tmp = 0 
  for (i in 1:length(x)) { 
    tmp = tmp + (data[i]-mu)**2 
  }
  NLL = NLL + -(1/(2*(sigma**2)))*tmp 
#  capMu <<- rbind(capMu, c(NLL, mu, sigma))
  -NLL 
}

theta = c(9,1)
data = rnorm(200, 10, 2)
capMu <- matrix( nrow = 0, ncol = 3)

NLL2 <- function(theta,data) { 
  mu = theta[1] 
  sigma = theta[2] 
#  n = length(data)
  t <- n
  NLL = -(length(data)/2)*log(2*pi) - (length(data)/2)*log(sigma**2) 
  print(NLL)
  tmp = 0 
  for (i in 1:length(data)) { 
    tmp = tmp + (data[i]-mu)**2 
  }
  print(tmp)
  NLL = NLL + -(1/(2*(sigma**2)))*tmp 
  print(NLL)
  # the <<<- operator lets us use an object outside the loop
  capMu <<- rbind(capMu, c(NLL, mu, sigma))
  -NLL 
}

NLL2(c(9, 1), data)

data = sort(data)

for (i in 1:length(data)) { 
  print(tmp)
  tmp = tmp + (data[i]-mu)**2 
}


out = optim(par=c(8,1), fn=NLL2, data=x) 
out$par

a = sum(tmp)

data = x
for (i in 1:length(data)) { 
  tmp = tmp + (data[i]-mu)**2 
}

b = sum(tmp)


x <- rnorm(200, 10, 2)
theta = c(10, 2)
NLL1(theta, x)
NLL2(theta, x)

out = optim(par=c(8,1), fn=NLL1, data=x) 
out$par

out = optim(par=c(8,1), fn=NLL2, data=x) 
out$par

# --------------------- visual on MLE - need to reformat list

x <- rnorm(100, 10, 2)
mu1 = seq(8, 12, length.out = 10)
sigma1 = seq(1, 3, length.out = 10)

minLL = matrix( nrow = 0, ncol = 3)
length(mu1) * length(sigma1)
for (i in 1:length(mu1)) { 
  mu = mu1[i]
  for(j in 1:length(sigma1)){
    sigma = sigma1[j]
    ml = NLL(c(mu, sigma), x)
    minLL <<- rbind(minLL, c(mu, sigma, ml))
    }
  }

dfSurface <- data.frame(minLL)
colnames(dfSurface)[1] <- 'mu'
colnames(dfSurface)[2] <- 'sigma'
colnames(dfSurface)[3] <- 'ml'

#zM <- as.matrix(dfSurface)

zM <- as.matrix(dfSurface$ml, nrow = 10, ncol = 10 )

tst = matrix(dfSurface$ml, nrow = 10)

p <- plot_ly ( z = tst) %>% add_surface() %>%
  layout(
    title = "Minimum Log Likelihood",
    scene = list(
      xaxis = list(title = "mu"),
      yaxis = list(title = "sigma"),
      zaxis = list(title = "ml")
    ))
p

dfSurface$ml


x <- mu1
y <- sigma1
plot_ly() %>% add_surface(x = ~x, y = ~y, z = ~tst) %>%
layout(
  title = "Minimum Log Likelihood",
  scene = list(
    xaxis = list(title = "mu"),
    yaxis = list(title = "sigma"),
    zaxis = list(title = "ml")
  ))




x <- seq_len(nrow(volcano)) + 100
y <- seq_len(ncol(volcano)) + 500
plot_ly() %>% add_surface(x = ~x, y = ~y, z = ~volcano)

r <- array(dfSurface, c(365, 6, 4))




mymat = matrix(dfSurface$ml,ncol=10000, nrow = 10000)


v = volcano

max.row <- row.names(dfSurface)[(which(dfSurface$logL==max(dfSurface$logL)))]
maxRow <- dfSurface[max.row,]
round(maxRow$mu,0)
round(maxRow$sigma,0)



# ------------------- original

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
out = optim(par=c(8,1), fn=NLL, data=x) 
out$par


dfSurface <- data.frame(capMu)
colnames(dfSurface)[1] <- 'logL'
colnames(dfSurface)[2] <- 'mu'
colnames(dfSurface)[3] <- 'sigma'

zM <- as.matrix(dfSurface)

p <- plot_ly ( z = zM) %>% add_surface() %>%
  layout(
    title = "Maximum Likelihood",
    scene = list(
      xaxis = list(title = "sigma"),
      yaxis = list(title = "mu"),
      zaxis = list(title = "logL")
    ))
p

max.row <- row.names(dfSurface)[(which(dfSurface$logL==max(dfSurface$logL)))]
maxRow <- dfSurface[max.row,]
round(maxRow$mu,0)
round(maxRow$sigma,0)


# -------------- mle applied to regression -----------------#

# linear likelihood function
linear.lik <- function(theta, y, X){
  n      <- nrow(X)
  k      <- ncol(X)
  beta   <- theta[1:k]
  sigma2 <- theta[k+1]^2
  e      <- y - X%*%beta
  logl   <- -.5*n*log(2*pi)-.5*n*log(sigma2) - ( (t(e) %*% e)/ (2*sigma2) )
  return(-logl)
}

# create some linear data

data.x <- rnorm(n = 100, mean = 10, sd = 2)

a.true <- 3
b.true <- 8

true.y <- data.x * a.true + b.true

err.sd.true <- 1  # Set noise sd
noise <- rnorm(n = 100, mean = 0, sd = 2)  # Generate noise

data.y <- true.y + noise  # Add noise to true (latent) responses

data <- data.frame(cbind(x = data.x, y = data.y))

lmData <- data
mod <- lm(data = lmData, y ~ x)
summary(mod)

ggplot(lmData, aes(x = lmData$x, y = lmData$y)) + geom_point() + geom_smooth(method = 'lm')

linear.MLE <- optim(fn=linear.lik, par=c(1,1,1), lower = c(-Inf, -Inf, 1e-8), 
                    upper = c(Inf, Inf, Inf), hessian=TRUE, 
                    y=data$y, X=cbind(1, data$x), method = "L-BFGS-B")



# Compare lm with MLE
linear.MLE$par[1]
mod$coefficients[1]
linear.MLE$par[2]
mod$coefficients[2]



# ------------- conditional probability - sampling without replacement ------#


S <- read_csv("Cards.csv")
A <- nrow(dplyr::filter(S, rank == 'A'))/nrow(S)
A
B <- (nrow(dplyr::filter(S, rank == 'A'))-1)/(nrow(S)-1)
B
A*B


# ----------------------- BAYES ------------------#

# Probability of making quota in Territory 4

Bayes <- ((18/73)*(.75))/((20/73)*(.8)+(25/73)*(.9)+(10/73)*(.95)+(18/73)*(.75))
round(Bayes,2)


prior <- c(0.6, 0.3, 0.1) 
likelihood <- c(0.003, 0.007, 0.01)
posterior <- (prior * likelihood) / sum(prior * likelihood)
round(posterior ,2)

# updating the priors

prior <- posterior
likelihood <- c(0.005, 0.004, 0.006)
posterior <- (prior * likelihood) / sum(prior * likelihood)
round(posterior ,2)


