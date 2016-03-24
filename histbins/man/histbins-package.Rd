\name{histbins-package}
\alias{histbins-package}
\alias{histbins}
\docType{package}
\title{
Histogram bin-count optimiser
}
\description{
Computes number of histogram bins by minimising the cross-validation risk estimator.
}
\details{
\tabular{ll}{
Package: \tab histbins\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-12-05\cr
License: \tab UKG\cr
}
The main function is \code{\link{hist.bins}}, which for a 1-dimensional data set computes a number of bins to be used as argument 'breaks' or 'nclass' in the hist() function. It does this by finding a minimum of the cross-validation estimate of MISE, the mean integrated square error of the histogram step function relative to the population density that the histogram is trying to model. (See \code{\link{cv.risk.estimate}}.)

Note that this minimum is problematic for three reasons: (1) the population density is not known, and the cross-validation estimate is only as good as the data it's given. (2) The CV estimate is noisy, so the histbins function smooths it using a moving average before finding the minimum. (3) Both the (smoothed) estimate, and the actual MISE function (when this is known) usually have a long shallow minimum, so that the number of bins achieving the minimum is unstable, and can vary a lot with the data sample.

The histbins choice of number of bins tends to work better than R's defaults for some distributions (e.g. Gaussian mixtures), but less well for others (e.g. chi-squared).
}
\author{
Bill Oxbury

Maintainer: <wmoxbur@gchq>
}
\references{
Larry Wasserman, All of Statistics (Springer 2004), chap 20
}
% ~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R ~~
% ~~ documentation directory ~~
\keyword{ package }
\seealso{
% ~~ Optional links to other man pages, e.g. ~~
% ~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
## Generate some GMM data:

n <- 1000
p <- c(0.3, 0.2, 0.5)
mu <- c(1,2,3)
sigma <- c(0.3,0.3,0.3)

input <- gmm(n,p,mu,sigma)
dat <- input$data
f.true <- input$density

mn <- min(dat)
mx <- max(dat)
x <- seq(mn,mx, 0.01)      # our x-axis throughout
y.true <- sapply(x,f.true) # true density

## R's default histogram:

hist(dat, probability=TRUE)
rug(dat, col='red')
points(y.true ~ x, type='l', col='red')

## Histogram using nr bins from histbins:

( nbins <- hist.bins(dat) )
hist(dat, breaks=nbins[1], probability=TRUE)
rug(dat, col='red')
points(y.true ~ x, type='l', col='red')

## Algorithms available in R for choosing the number of bins:

nclass.Sturges(dat) 
# = ceiling(log(n,2) + 1), default for hist()
nclass.scott(dat)   
# uses bin width 3.5*sd(dat)*n^(-1/3)
nclass.FD(dat)      
# uses bin width 2*R*n^(-1/3) where R = interquartile range

## How should one think about this?
## View the histogram as a step function that's trying to 
## approximate the population density of the data:

for(nbins in c(3,10,20,40,80,200)){
  f.est <- hist.step.function(dat, nbins)
  y.est <- sapply(x, f.est)
  plot(x,y.est, type='l', col='blue', main=sprintf("Nr bins = \%d", nbins))
  points(x,y.true, type='l', col='red')
  rug(dat, col='red')
}

## Note that as the number of bins increases, the bias of this 
## estimate goes to zero, but the variance increases. We see this 
## averaging over multiple samples for each bin size:

nexpts <- 100
for(nbins in c(3,10,20,40,80,200)){
  
  obs <- matrix(nrow=0, ncol=length(x))
  # repeat the estimator nexpts times:
  for(expt in 1:nexpts){
    tmp <- gmm(n,p,mu,sigma)$data
    f.est <- hist.step.function(tmp, nbins)
    y.est <- sapply(x, f.est)
    obs <- rbind(obs, y.est)
  }
  y.mean <- sapply(1:length(x), function(i){ mean(obs[,i]) })
  y.sd <- sapply(1:length(x), function(i){ sd(obs[,i]) })
  y.lower <- sapply(y.mean - y.sd, function(x) max(x,0))
  y.upper <- y.mean + y.sd

  plot(y.upper ~ x, type='n', main=sprintf("Nr bins = \%d", nbins))
  points(y.mean ~ x, type='l', col='blue', lwd=2)
  points(y.upper ~ x, type='l', col='grey')
  points(y.lower ~ x, type='l', col='grey')
  points(y.true ~ x, type='l', col='red')
}

## Which is resolution best? Ideally, we'd like to minimise 
## the mean integrated squared error (MISE) of the histogram step
## function compared with the population density.
## However, in general we don't know the latter (blue plot below) 
## -- but cross-validation gives us a way to estimate it (red curve):

mise <- c()
b.set <- 5:100
for(breaks in b.set){
  f.est <- hist.step.function(dat, breaks)
  sq.error <- function(x) (f.true(x) - f.est(x))^2
  mise <- c(mise, area(sq.error, mn, mx) )
}
mise.est <- sapply(b.set, function(b){cv.risk.estimate(dat, b)})
mise.est.sm <- ma.smooth(mise.est, span=3)

plot(mise ~ b.set, type='b', col='blue', 
     xlab="Nr bins", ylab="MISE")
plot(mise.est.sm ~ b.set, type='l', col='red', 
     xlab="Nr bins", ylab="CV estimate of MISE")
choice <- which(mise.est.sm == min(mise.est.sm))
points(b.set[choice], mise.est.sm[choice], pch=19, col='blue')
text(b.set[choice], mise.est.sm[choice] + 0.01, col='blue', labels=b.set[choice])

## The hist.bis() function chooses argmin of this smoothed MISE estimate:
( nbins <- hist.bins(dat) )


}
