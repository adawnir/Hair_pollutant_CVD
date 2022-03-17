
rtruncnorm = function(n, mean = 0, sd = 1, min = -Inf, max = Inf){
  if (min > max) stop('Error: Truncation range is empty')
  x = runif(n, pnorm(min, mean, sd), pnorm(max, mean, sd))
  qnorm(x, mean, sd)
}
