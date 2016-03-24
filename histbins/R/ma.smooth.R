ma.smooth <-
function(x, span=1){
  xx <- c(rep(x[1], span), x, rep(x[length(x)], span))
  sapply(1:length(x), function(i){ mean(xx[i:(i+2*span)]) })
}
