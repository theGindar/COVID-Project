# x: input data as list
# n: moving average window size
moving_average <- function(x, n) {
  return(stats::filter(x, rep(1 / n, n), sides = 2))
}
