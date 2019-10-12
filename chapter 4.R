#1
Myst <- readRDS("~/Desktop/stat 4600/biostatistics/data/Myst.rds")
yvar = Myst$yvar
ggplot(tibble(yvar), aes(x = yvar)) + geom_histogram(binwidth=0.025)
str(yvar)

# This assigns random probability of membership

# pA = probability of coming from micture A
pA = runif(length(yvar)) 

# pB = complimentary probability
pB = 1 - pA

# maxiter: Maximum number of iterations.

# minprior: Minimum prior probability of clusters, components falling below this threshold are
# removed during the iteration.

# tolerance: The EM algorithm is stopped when the (relative) change of log-likelihood is smaller
# than tolerance.

iter = 0
loglik = -Inf
delta = +Inf
tolerance = 1e-3
miniter = 50; maxiter = 1000

while((delta > tolerance) && (iter <= maxiter) || (iter < miniter)) {
  lambda = mean(pA)
  muA = weighted.mean(yvar, pA) # M-Step
  muB = weighted.mean(yvar, pB) # M-Step
  sdA = sqrt(weighted.mean((yvar - muA)^2, pA)) # M-Step
  sdB = sqrt(weighted.mean((yvar - muB)^2, pB)) # M-Step
  
  phiA = dnorm(yvar, mean = muA, sd = sdA) # E-Step
  phiB = dnorm(yvar, mean = muB, sd = sdB) # E-Step
  pA   = lambda * phiA
  pB   = (1 - lambda) * phiB
  ptot = pA + pB # E-Step (Normalizer)
  pA   = pA / ptot # E-Step (Post)
  pB   = pB / ptot # E-Step (Post)
  
  loglikOld = loglik
  loglik = sum(log(pA))
  delta = abs(loglikOld - loglik) #loglik used here. To calculate delta?
  iter = iter + 1
}
param = tibble(group = c("A","B"), mean = c(muA,muB), sd = c(sdA,sdB))
param

# Check this out to better understand E-Step and M-Step (Maybe im just dumb)
# http://tinyheero.github.io/2016/01/03/gmm-em.html#expectation-calculating-the-soft-labels-of-each-data-point-e-step

# These parameter-estimates are then used to determine the distribution of the latent variables in the next E step.

#2
#question worded weird.. i undertsnd it as using the values in of it$par to use in a qqplot? 
library("vcd")
lambda = rgamma(10000, shape = 10, rate = 3/2)
gp = rpois(length(lambda), lambda = lambda)
ggplot(tibble(x = gp), aes(x = x)) +
  geom_histogram(bins = 100, fill= "purple")

it = goodfit(gp, "poisson")
plot(it, xlab = "")
#rootogram(it, xlab = "", rect_gp = gpar(fill = "chartreuse4"))
it$par
lambda = it$par[[1]]

#simbat : the output of the lapply loop is a list of tibbles, one for each value of lambdas. 
lambdas = seq(100, 900, by = 100)
simdat = lapply(lambdas, function(l)
  tibble(y = rpois(n = 40, lambda=l), lambda = l)
) %>% bind_rows

library("ggbeeswarm")
#using the lambda we found in it$par
ggplot(simdat, aes(x = lambda, y = y)) +
  geom_beeswarm(alpha = 0.6, color = "purple")
ggplot(simdat, aes(x = lambda, y = sqrt(y))) +
  geom_beeswarm(alpha = 0.6, color = "purple")
#i think this is what we have to do..? not using a qqplot tho.. so idk
#qq plot used to test  heterogenous, so could be kind of the same idea..? 
qqplot(simdat, lambda, type = "l", asp = 1)
abline(a = 0, b = 1, col = "blue")

#3
library("flexmix")
data(NPreg)
names(NPreg)

#A)
ggplot(NPreg %>% filter(class == "1"), aes(x = x)) +
  geom_histogram(binwidth = 0.5, fill = "forestgreen")

ggplot(NPreg %>% filter(class == "2"), aes(x = x)) +
  geom_histogram(binwidth = 0.5, fill = "blue")

# I think they were plotted by classes, class 1 and class 2

#B) Fit a two component mixture model using the commands
m1 = flexmix(yn ~ x + I(x^2), data = NPreg, k = 2)
m1
#not too sure what Fit a two component mixture model using the commands means, i think maybe something like this? or do i have to graph two histoograms one ontop of another. 
matplot(NPreg$x, fitted(m1), pch = 16, type="p")
points(NPreg$x, NPreg$yn)

#C)
#use table function

#D)


#4 possible papers
# https://rss.onlinelibrary.wiley.com/doi/abs/10.1111/j.1467-9868.2010.00756.x 
# http://users.umiacs.umd.edu/~hal/docs/daume08ihfrm.pdf 
# http://www.stats.ox.ac.uk/~teh/research/npbayes/HelTehGor2009a.pdf 
# https://ieeexplore.ieee.org/abstract/document/4407680
# https://projecteuclid.org/euclid.ba/1340370725