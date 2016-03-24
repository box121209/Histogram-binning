\name{gmm}
\alias{gmm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gaussian mixture model
}
\description{
Given arrays of mu and sigma values, and mixing probability vector, generates a 1-dimensional GMM density and a sample from it. 
}
\usage{
gmm(n, p, mu, sigma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
Sample size (integer)
}
  \item{p}{
Numeric vector specifying the mixing probabilities up to scale
}
  \item{mu}{
Vector of Gaussian means
}
  \item{sigma}{
Vector of Gaussian standard deviations
}
}
\details{
Does what it says on the tin -- for constructing examples for hist.bins
}
\value{
  \item{data}{Sample of size n (numeric vector)}
 \item{density}{Function R -> R, the GMM density}
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
 \code{\link{hist.bins}}, \code{\link{hist.step.function}}
}
\examples{
n <- 1000
p <- c(1,2)
mu <- c(-1,1)
sigma <- c(0.3,0.3)
input <- gmm(n,p,mu,sigma)
hist(input$dat, main="Default histogram")
rug(input$dat, col='red')
hist(input$dat, breaks=hist.bins(input$dat), main="'histbins' histogram")
rug(input$dat, col='red')

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
