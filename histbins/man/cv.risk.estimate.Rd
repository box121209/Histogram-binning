\name{cv.risk.estimate}
\alias{cv.risk.estimate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cross validation risk estimate
}
\description{
Given a vector of data points and a number of (equal-sized) bins for a histogram, estimates the mean integrated square error (MISE) for the histogram, compared to the population density.
}
\usage{
cv.risk.estimate(dat, nbins)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
Numeric data points
  }
  \item{nbins}{
Number of bins
  }
}
\details{
Uses the cross-validation formula 

  ( 2 * nbins - (n + 1) * nbins * sum{p.hat^2} ) / (n - 1), 

where p.hat[j] = (nr data points in j-th bin)/(nr data points). Here's briefly where the formula comes from. Let f be the density we're trying to estimate, and f.hat the step function corresponding to histogram with bin-width h. The loss function is then (where int{} means integral)

  L(h) = int{ (f.hat - f)^2 }
       = int{ f^2 } + J(h)
       
where int{f^2} is some constant we don't care about, and

  J(h) = int{ f.hat^2 } - 2 * int{ f * f.hat }.
  
What we want to minimise is the expectation E(L(h)), which is the same as minimising E(J(h)). But we can show that an unbiased estimator of J(h) is

  J.hat(h) = int{ f.hat^2 } - (2/n) * sum{ f.hat_{-i}(x_i) }.
  
The notation here is: x_i is the i-th data point, and f.hat_{-i} is the histogram step function (bin width h) got by leaving out x_i. Note that the integral in the first term is easy to compute, since f.hat is a step function that we know.

So we just need to minimise, over h, the function J.hat(h). The second claim (straightforward to verify) is that this can be re-written as (where n = nr data points)

  J.hat(h) = 2/(h*(n-1)) - (n+1)/(h*(n-1)) * sum{ p.hat_j^2 }

where j ranges over the bins and p.hat_j is the proportion of data points in the j-th bin. The cross-validation formula follows from this.
}
\value{
Numeric. This estimates MISE up to an additive constant (with respect to nbins).
}
\references{
Larry Wasserman, All of Statistics (Springer 2004), chap 20
}
\author{
Bill Oxbury
}
\note{
%%  ~~further notes~~
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
\code{\link{hist.bins}}
}
\examples{
input <- gmm(1000, c(0.25, 0.2, 0.5), c(1,2,3), c(0.3,0.4,0.2))
dat <- input$data
f.true <- input$density
mn <- min(dat); mx <- max(dat)
x <- seq(mn,mx, 0.01)      # our x-axis throughout
y.true <- sapply(x,f.true) # true density

b.set <- 5:100
mise <- c()
for(nbins in b.set){
  f.est <- hist.step.function(dat, nbins)
  sq.error <- function(x) (f.true(x) - f.est(x))^2
  mise <- c(mise, area(sq.error, mn, mx) )
}
mise.est <- sapply(b.set, function(b){ cv.risk.estimate(dat, b) })

plot(mise ~ b.set, type='b', col='blue', 
     xlab="Nr bins", ylab="MISE")
plot(mise.est ~ b.set, type='l', col='red', 
     xlab="Nr bins", ylab="CV estimate of MISE")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
