gmm <-
function(n, p,mu,sigma){
  k <- length(p)
  f <- function(x){
    sum( sapply(1:k, function(i) p[i] * dnorm(x, mean=mu[i], sd=sigma[i]) ) )
  }
  mix = sample(1:k, n, prob=p, replace=TRUE)
  dat = sapply(1:n, function(i){ rnorm(1,mean=mu[mix[i]], sd=sigma[mix[i]]) })
  list(
    data=dat,
    density=f
    )
}
