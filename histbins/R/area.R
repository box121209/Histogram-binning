area <-
function(f, a, b, epsilon = 1.0e-06, niters = 10) 
{
  fun.outer <- function(f,a,b,fa,fb,est0,epsilon,niters,fun.inner){
    
    d <- (a + b)/2
    h <- (b - a)/4
    fd <- f(d)
    est1 <- h * (fa + fd)
    est2 <- h * (fd + fb)
    if(abs(est0 - est1 - est2) < epsilon || niters == 0)
      est1 + est2
    else 
      fun.inner(f,a,d,fa,fd,est1,epsilon,niters-1,fun.inner) +
                fun.inner(f,d,b,fd,fb,est2,epsilon,niters-1,fun.inner)
  }
  fa <- f(a)
  fb <- f(b)
  est0 <- (fa + fb) * (b - a)/2
  fun.outer(f, a, b, fa, fb, est0, epsilon, niters, fun.outer)
}
