hist.step.function <-
function(dat, breaks)
{
  h <- hist(dat, breaks=breaks, plot=FALSE)
  function(xx){ 
    i <- sum(h$breaks < xx)
    if(i<1 | i>length(h$density)) 0 else h$density[i]
  }
}
