\name{area}
\alias{area}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Area under a curve
}
\description{
Recursive numerical integration of a single-variable function on an interval [a,b].
}
\usage{
area(f, a, b, epsilon = 1e-06, niters = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{
Function R -> R to be integrated
}
  \item{a}{
Left-hand limit
}
  \item{b}{
Right-hand limit
}
  \item{epsilon}{
Convergence threshold
}
  \item{niters}{
Maximum number of iterations
}
}
\details{
Recursive divide-and-conquer approximation by trapesiods. The implementation is a nice example of functional programming.
}
\value{
Numeric: integral of f(x) from a to b
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## ex 1
area(sin, 0,pi/2) 

## ex 2
area(function(x) 1/x, 1, exp(1))

## ex 3
sapply(1:12, function(i){ area(function(x) 1/x, 1, exp(i)) })

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
