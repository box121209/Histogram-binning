\name{ma.smooth}
\alias{ma.smooth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Moving average smoother
}
\description{
Replaces each point in a sequence by the average of its neighbours k to left and right, with itself as midpoint. 
}
\usage{
ma.smooth(x, span = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Numeric sequence
}
  \item{span}{
Distance (to left and right) to average over
}
}
\details{
Deals with the end points by first repeating them 'span' times beyond the end of the sequence.
}
\value{
Numeric sequence
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Bill Oxbury}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{cv.risk.estimate}}
}
\examples{
dat <- rnorm(100)
col <- c('yellow','orange','green','red','blue')
plot(dat, type='l', col='grey', frame.plot=0)
for(i in 1:5)
  points(ma.smooth(dat, span=i), type='l', col=col[i], lwd=i)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
