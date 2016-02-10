# Xiaofeng Steven Liu (2012)
# Implications of statistical power for confidence intervals
# British Journal of Mathematical and Statistical Psychology (2012), 65, 427–437
#' \code{fwr} power and omega from Lui(2012)
#'
#' @param n Sample Size.
#' @param d Delta/Sigma.
#' @param ud U/Sigma.
#' @param ttail 2-sided upper tail (defaults to 0.975)
#' @return Power and Omega.
#' @export
#' @examples
#' fwr(n = 70, d = 0.5, ud = 0.35, ttail = 0.975)

fwr <- function(n, d, ud, ttail = 0.975) {
  v      <- 2 * n - 2
  lambda <- sqrt(n / 2) * d
  t0     <- qt(ttail, v)
  upper  <- (ud * sqrt(n * v / 2) / t0) ^ 2
  pwr1   <-
    integrate(function(xx3) {
      (1 - pnorm(t0 * sqrt(xx3 / v) - lambda)) * dchisq(xx3, v)
    }, 0, upper)$val
  pwr2   <-
    integrate(function(xx4) {
      pnorm(-t0 * sqrt(xx4 / v) - lambda) * dchisq(xx4, v)
    }, 0, upper)$val
  pwr    <- pwr1 + pwr2
  power  <- pt(-t0, v, lambda) + 1 - pt(t0, v, lambda)
  omega  <- pwr / power
  cat("n", " \t", "power", " \t", "omega", " \n")
  cat(n, " \t", power, " \t", omega, " \n")
}

