
#' Poisson Distribution Moments
#'
#' Calculate the theoretical mean and variance
#' of a Poisson distribution.
#'
#' @param lambda Expected number of events (positive number).
#' @return A named list containing mean and variance.
#' @examples
#' poisson_moments(3)
#' @export
poisson_moments <- function(lambda) {
  if (!is.numeric(lambda) || length(lambda) != 1 ||
      is.na(lambda) || !is.finite(lambda) || lambda <= 0) {
    stop("lambda must be a positive number.")
  }
  
  list(
    mean = lambda,
    variance = lambda
  )
}
