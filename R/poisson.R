
#' Poisson Probability Mass Function
#'
#' Calculate the probability of observing exactly k events.
#'
#' @param k Number of events (non-negative integer).
#' @param lambda Expected number of events (positive number).
#' @return Probability of observing exactly k events.
#' @examples
#' poisson_pmf(3, 2)
#' @export
poisson_pmf <- function(k, lambda) {
  stats::dpois(k, lambda = lambda)
}


#' Poisson Cumulative Distribution Function
#'
#' Calculate the probability of observing at most q events.
#'
#' @param q Maximum number of events.
#' @param lambda Expected number of events (positive number).
#' @return Cumulative probability.
#' @examples
#' poisson_cdf(3, 2)
#' @export
poisson_cdf <- function(q, lambda) {
  stats::ppois(q, lambda = lambda)
}


#' Generate Random Poisson Values
#'
#' Generate random observations from a Poisson distribution.
#'
#' @param n Number of observations.
#' @param lambda Expected number of events (positive number).
#' @return A vector of random Poisson values.
#' @examples
#' poisson_rnd(10, 3)
#' @export
poisson_rnd <- function(n, lambda) {
  stats::rpois(n, lambda = lambda)
}
