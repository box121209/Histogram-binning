cv.risk.estimate <-
function(dat, nbins){
  n <- length(dat)
  h <- 1/nbins
  mn <- min(dat)
  mx <- max(dat)
  p.hat <- sapply(1:nbins, function(j){ 
    sum(dat >= mn + (j-1)*(mx - mn)/nbins & dat < mn + j*(mx - mn)/nbins) 
    })
  p.hat[nbins] <- p.hat[nbins] + 1
  p.hat <- p.hat/n
  # return:
  (2 * nbins - (n + 1) * nbins * sum(p.hat^2))/(n - 1)
}
