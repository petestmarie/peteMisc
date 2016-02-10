# Xiaofeng Steven Liu (2012)
# Implications of statistical power for confidence intervals
# British Journal of Mathematical and Statistical Psychology (2012), 65, 427–437
#' \code{fwr} power and omega from Lui(2012)
#'
#' @param n Sample Size (can be a range).
#' @param d Delta/Sigma.
#' @param ud U/Sigma.
#' @param ttail 2-sided upper tail (defaults to 0.975)
#' @return Power and Omega.
#' @export
#' @examples
#' fwr(n = 70, d = 0.5, ud = 0.35, ttail = 0.975)
#' fwr(n = seq(65, 75, by=1), d = 0.5, ud = 0.35, ttail = 0.975)
#' 
fwr <- function(n, d, ud, ttail = 0.975) {
  v      <- 2 * n - 2
  lambda <- sqrt(n / 2) * d
  t0     <- qt(ttail, v)
  upper  <- (ud * sqrt(n * v / 2) / t0) ^ 2
  
  pwr1 <- vector('numeric', length = length(n))
  pwr2 <- vector('numeric', length = length(n))
  for(i in 1:length(n)) {
    pwr1[i] <- integrate(function(xx3) {
      (1 - pnorm(t0[i] * sqrt(xx3 / v[i]) - lambda[i])) * dchisq(xx3, v[i])
    }, 0, upper)$val
  }
  
  for(i in 1:length(n)) {
    pwr2[i] <-   integrate(function(xx4) {
      pnorm(-t0[i] * sqrt(xx4 / v[i]) - lambda[i]) * dchisq(xx4, v[i])
    }, 0, upper)$val
  }
  
  pwr    <- pwr1 + pwr2
  power  <- pt(-t0, v, lambda) + 1 - pt(t0, v, lambda)
  omega  <- pwr / power
  
  df <- data.frame(n, power, omega)
  
  knitr::kable(df)
}
