
#' Calculate Mean
#' @param x Numeric vector.
#' @return Mean of x, ignoring missing values.
#' @examples
#' calculate_mean(1:5)
#' @export
calculate_mean <- function(x) {
  stopifnot(is.numeric(x))
  mean(x, na.rm = TRUE)
}


#' Calculate Variance
#' @param x Numeric vector.
#' @return Sample variance, ignoring missing values.
#' @examples
#' calculate_variance(1:5)
#' @export
calculate_variance <- function(x) {
  stopifnot(is.numeric(x))
  stats::var(x, na.rm = TRUE)
}


#' Calculate Standard Deviation
#' @param x Numeric vector.
#' @return Sample standard deviation, ignoring missing values.
#' @examples
#' calculate_sd(1:5)
#' @export
calculate_sd <- function(x) {
  stopifnot(is.numeric(x))
  stats::sd(x, na.rm = TRUE)
}


#' Calculate Quantiles
#' @param x Numeric vector.
#' @param probs Probabilities between 0 and 1.
#' @return Quantiles of x.
#' @examples
#' calculate_quantiles(1:9)
#' @export
calculate_quantiles <- function(x, probs = c(0.25, 0.5, 0.75)) {
  stopifnot(is.numeric(x))
  stats::quantile(x, probs = probs, na.rm = TRUE, names = TRUE)
}


#' Normalize Data
#' @param x Numeric vector.
#' @return Values scaled between 0 and 1.
#' If minimum equals maximum, returns zeros.
#' @examples
#' normalize_data(1:10)
#' @export
normalize_data <- function(x) {
  stopifnot(is.numeric(x))
  rng <- range(x, na.rm = TRUE)
  if (isTRUE(all.equal(rng[1], rng[2]))) {
    return(ifelse(is.na(x), NA_real_, 0))
  }
  (x - rng[1]) / (rng[2] - rng[1])
}


#' Standardize Data
#' @param x Numeric vector.
#' @return Standardized values using z-scores.
#' @examples
#' standardize_data(1:10)
#' @export
standardize_data <- function(x) {
  stopifnot(is.numeric(x))
  m <- mean(x, na.rm = TRUE)
  s <- stats::sd(x, na.rm = TRUE)
  if (isTRUE(all.equal(s, 0))) {
    return(ifelse(is.na(x), NA_real_, 0))
  }
  (x - m) / s
}


#' Calculate Correlation
#' @param x First numeric vector.
#' @param y Second numeric vector.
#' @param method Correlation method: pearson, spearman or kendall.
#' @return Correlation coefficient.
#' @examples
#' calculate_correlation(1:10, 1:10)
#' @export
calculate_correlation <- function(x, y,
                                  method = c("pearson", "spearman", "kendall")) {
  method <- match.arg(method)
  stopifnot(is.numeric(x), is.numeric(y), length(x) == length(y))
  stats::cor(x, y, use = "complete.obs", method = method)
}


#' Simple Linear Regression
#' @param x Numeric predictor.
#' @param y Numeric response.
#' @return Linear regression model.
#' @examples
#' linear_regression(1:10, 2 * (1:10) + 1)
#' @export
linear_regression <- function(x, y) {
  stopifnot(is.numeric(x), is.numeric(y), length(x) == length(y))
  stats::lm(y ~ x)
}


#' Plot Data
#' @param x Numeric vector.
#' @param y Optional second numeric vector.
#' @param type Plot type, such as p for points or l for lines.
#' @return A plot.
#' @examples
#' plot_data(1:10, (1:10)^2)
#' @export
plot_data <- function(x, y = NULL, type = "p") {
  if (is.null(y)) {
    graphics::hist(x, main = "Histogram",
                   xlab = deparse(substitute(x)))
  } else {
    graphics::plot(x, y, type = type,
                   xlab = deparse(substitute(x)),
                   ylab = deparse(substitute(y)))
  }
}


#' Plot Regression Residuals
#' @param model Linear regression model.
#' @return Diagnostic plots.
#' @examples
#' model <- stats::lm(mpg ~ wt, data = mtcars)
#' plot_residuals(model)
#' @export
plot_residuals <- function(model) {
  stopifnot(inherits(model, "lm"))
  op <- graphics::par(mfrow = c(1, 2))
  on.exit(graphics::par(op), add = TRUE)
  
  graphics::plot(stats::fitted(model), stats::residuals(model),
                 xlab = "Fitted Values",
                 ylab = "Residuals",
                 main = "Residuals vs Fitted")
  
  graphics::abline(h = 0, lty = 2)
  stats::qqnorm(stats::rstandard(model),
                main = "Q-Q Plot of Standardized Residuals")
  stats::qqline(stats::rstandard(model))
}


#' Generate Sample Data
#' @param n Number of observations.
#' @param seed Optional random seed.
#' @return Data frame with x and y columns.
#' @examples
#' generate_sample_data(20, seed = 123)
#' @export
generate_sample_data <- function(n = 50, seed = NULL) {
  stopifnot(is.numeric(n), length(n) == 1, n > 1)
  if (!is.null(seed)) set.seed(seed)
  x <- seq_len(n)
  y <- 3 + 1.5 * x + stats::rnorm(n, sd = stats::sd(x) / 2)
  data.frame(x = x, y = y)
}
