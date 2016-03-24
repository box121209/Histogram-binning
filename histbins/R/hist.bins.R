hist.bins <-
function(dat, smoother=3, range=3:100){
  mise.est <- sapply(range, function(b){cv.risk.estimate(dat, b)})
  mise.est.sm <- ma.smooth(mise.est, span=smoother)
  range[ which(mise.est.sm == min(mise.est.sm)) ][1]
}
