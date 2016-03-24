\name{hist.step.function}
\alias{hist.step.function}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Histogram step function
}
\description{
Returns the step function f:R -> R corresponding to a histogram.
}
\usage{
hist.step.function(dat, breaks)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
Numeric vector of data points
}
  \item{breaks}{
Sames as breaks argument to the R hist() function
}
}
\details{
Does what it says on the tin.
}
\value{
Function: numeric -> numeric
}
\references{
%% ~put references to the literature/web site here ~
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

for(breaks in c(4,10,20,40,80,200,400)){
  f.est <- hist.step.function(dat, breaks)
  y.est <- sapply(x, f.est)
  plot(x,y.est, type='l', col='blue', main=sprintf("Breaks = \%d", breaks))
  points(x,y.true, type='l', col='red')
  rug(dat, col='red')
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
