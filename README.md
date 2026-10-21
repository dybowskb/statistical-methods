
# Statistical Methods

An R package developed for the Statistical Methods course at the University of Malaga (UMA).

The package contains functions for descriptive statistics, data analysis, and probability distributions.

## Installation

Install the package from GitHub:

```r
install.packages("remotes")
remotes::install_github("dybowskb/statistical-methods")
```

Load the package:

```r
library(statisticalmethods)
```

## Basic Statistical Functions

The package provides the following functions:

- `calculate_mean()` - Calculate the mean.
- `calculate_variance()` - Calculate the sample variance.
- `calculate_sd()` - Calculate the standard deviation.
- `calculate_quantiles()` - Calculate quantiles.
- `normalize_data()` - Normalize values between 0 and 1.
- `standardize_data()` - Standardize values using z-scores.
- `calculate_correlation()` - Calculate correlation.
- `linear_regression()` - Fit a simple linear regression model.
- `plot_data()` - Create plots.
- `plot_residuals()` - Plot regression residuals.
- `generate_sample_data()` - Generate sample data.

### Example

```r
calculate_mean(c(2, 4, 6, 8))
calculate_variance(c(2, 4, 6, 8))
normalize_data(c(10, 20, 30))
```

## Discrete Uniform Distribution

The package implements the discrete uniform distribution on the integers from a to b.

Available functions:

- `discrete_uniform_pmf(k, a, b)` - Probability mass function.
- `discrete_uniform_cdf(q, a, b)` - Cumulative distribution function.
- `discrete_uniform_rnd(n, a, b)` - Random number generation.
- `discrete_uniform_moments(a, b)` - Mean and variance.

### Example

```r
discrete_uniform_pmf(4, 1, 6)
discrete_uniform_cdf(3, 1, 6)
discrete_uniform_rnd(10, 1, 6)
discrete_uniform_moments(1, 6)
```

## Testing

Run the automated tests:

```r
devtools::test()
```

Check the complete package:

```r
devtools::check()
```

## Development

This project uses Git and GitHub for version control.

New probability distributions are developed in separate feature branches, tested, documented, and merged into the main branch using pull requests.
