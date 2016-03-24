\name{hist.bins}
\alias{hist.bins}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Number of histogram bins
}
\description{
Estimates number of histogram bins to minimise MISE (mean integrated squared error) using cross-validation.
}
\usage{
hist.bins(dat, smoother = 3, range = 3:100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
Array of data points to histogram
}
  \item{smoother}{
Span for moving average (MA) smoothing of the cross-validation sequence
}
  \item{range}{
Nr bins is chosen from this range  
  }
}
\details{
The choice of number of bins for a histogram is a bias-variance trade-off: low number gives a histogram (viewed as estimator of the population density) which has bias but low variance, while higher resolution gives low bias but high variance.

The optimal choice is one that minimises the integral of the squared error (MISE) from the population density. Since we don't know the latter, we use \code{\link{cv.risk.estimate}} as an estimator for it, and minimise this instead. Since this in turn is noisy, we first smooth it. The moving average span is the 'smoother' parameter. 

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
An integer from range
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
 \code{\link{cv.risk.estimate}}, \code{\link{ma.smooth}}
}
\examples{
main <- c("Sturges (default)","Scott","Freedman-Diaconis","histbins")

## ex 1

n <- 1000
p <- c(0.25, 0.25, 0.5)
mu <- c(1,2,3)
sigma <- c(0.3,0.4,0.2)

input <- gmm(n,p,mu,sigma)
dat <- input$data
f.true <- input$density

mn <- min(dat)
mx <- max(dat)
x <- seq(mn,mx, 0.01)      # our x-axis throughout
y.true <- sapply(x,f.true) # true density

nbins <- c(
  nclass.Sturges(dat),  # = ceiling(log(n,2) + 1), default for hist()
  nclass.scott(dat),   # uses bin width 3.5*sd(dat)*n^(-1/3)
  nclass.FD(dat),      # uses bin width 2*R*n^(-1/3) where R = interquartile range
  hist.bins(dat, smoother=3) 
  )
  
for(i in 1:length(nbins)){
  hist(dat, nclass=nbins[i], probability=TRUE, 
    main=sprintf("\%s \%d bins", main[i], nbins[i]))
  rug(dat, col='red')
  points(y.true ~ x, col='red', type='l')
}


## ex 2

df <- 1
dat <- rchisq(1000, df)

nbins <- c(
  nclass.Sturges(dat),  # = ceiling(log(n,2) + 1), default for hist()
  nclass.scott(dat),   # uses bin width 3.5*sd(dat)*n^(-1/3)
  nclass.FD(dat),      # uses bin width 2*R*n^(-1/3) where R = interquartile range
  hist.bins(dat, smoother=3) 
  )
  
for(i in 1:length(nbins)){
  hist(dat, nclass=nbins[i], probability=TRUE, 
    main=sprintf("\%s \%d bins", main[i], nbins[i]))
  rug(dat, col='red')
  curve(dchisq(x, df), col='red', add=TRUE)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
