
#' Discrete Uniform Distribution U(a,b)
#' @family discrete-distributions
#' @description PMF for a discrete uniform distribution
#' on integers from a to b (inclusive).
#' @param k Integer value(s) at which to evaluate the PMF.
#' @param a Integer lower bound.
#' @param b Integer upper bound.
#' @param q Numeric threshold(s) for the CDF.
#' @param n Non-negative integer sample size for RNG.
#' @return A numeric vector of probabilities.
#' @examples
#' discrete_uniform_pmf(1:6, 1, 6)
#' @export

discrete_uniform_pmf <- function(k, a, b) {
  if (any(!is.finite(c(a, b))) || a %% 1 != 0 ||
      b %% 1 != 0 || b < a) {
    stop("'a' and 'b' must be finite integers with b >= a.")
  }
  
  k <- as.numeric(k)
  ok <- (k %% 1 == 0) & (k >= a) & (k <= b)
  out <- numeric(length(k))
  out[ok] <- 1 / (b - a + 1)
  out
}


#' @rdname discrete_uniform_pmf
#' @export
discrete_uniform_cdf <- function(q, a, b) {
  if (any(!is.finite(c(a, b))) || a %% 1 != 0 || b %% 1 != 0 || b < a) {
    stop("'a' and 'b' must be finite integers with b >= a.")
  }
  q <- as.numeric(q)
  Fq <- numeric(length(q))
  Fq[q < a] <- 0
  Fq[q >= b] <- 1
  inside <- (q >= a) & (q < b)
  Fq[inside] <- (floor(q[inside]) - a + 1) / (b - a + 1)
  Fq
}


#' @rdname discrete_uniform_pmf
#' @export
discrete_uniform_rnd <- function(n, a, b) {
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 0 || n %% 1 != 0) {
    stop("'n' must be a non-negative integer.")
  }
  if (any(!is.finite(c(a, b))) || a %% 1 != 0 || b %% 1 != 0 || b < a) {
    stop("'a' and 'b' must be finite integers with b >= a.")
  }
  as.integer(a - 1 + sample.int(b - a + 1, size = n, replace = TRUE))
}
