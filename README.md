# babebi

**Bayesian estimation and validation for small-N designs with rater bias**

`babebi` implements an approximate Bayesian workflow for fully observed repeated-measures designs with **two time points and two raters**. Pre–post difference scores are analysed using a linear model with a rater indicator; a Monte Carlo procedure evaluates performance under simulation settings calibrated from the observed design.

**Version:** 0.1.0 · **R:** ≥ 4.1.0 · **Licence:** GPL-3

## Installation

```r
install.packages("babebi")
library(babebi)
```

CRAN: https://CRAN.R-project.org/package=babebi

## Statistical approach

The model analyses pre–post differences while adjusting for an additive rater effect. Posterior summaries for regression coefficients use a large-sample normal approximation centred at the least-squares estimator with a plug-in covariance matrix, under a flat improper prior. The comparison of models with and without a pre–post effect uses a **BIC-based Bayes-factor approximation**. These are approximations; the package does not perform exact posterior integration.

The optional Monte Carlo validation uses design quantities obtained from the input data, including sample size, mean pre–post change and the additive discrepancy of the second rater. Its reported summaries include bias, RMSE, credible-interval coverage, posterior tail probabilities and mean Bayes-factor values.

## Example

```r
dat <- data.frame(
  id = rep(1:4, each = 4),
  time = rep(c("pre", "pre", "post", "post"), times = 4),
  rater = rep(c("r1", "r2"), times = 8),
  y = c(
    3.0, 3.2, 3.8, 4.0,
    2.9, 3.1, 3.5, 3.7,
    3.4, 3.6, 4.0, 4.1,
    3.1, 3.3, 3.9, 4.0
  )
)

result <- babebi(
  data = dat,
  id = id,
  time = time,
  rater = rater,
  outcome = y,
  time_order = c("pre", "post"),
  rater_order = c("r1", "r2"),
  validate = TRUE,
  R = 100,
  n_draws = 500,
  seed = 123
)

print(result$fit)
summary(result$fit)
plot(result$fit)
print(result$validation)
```

The example uses a constructed dataset solely to demonstrate the software interface. A larger number of Monte Carlo repetitions and posterior draws may be needed for the intended analysis.

## Main functions

- `babebi()`: fit the model and optionally run Monte Carlo validation.
- `fit_model()`: obtain the approximate posterior regression summaries.
- `montecarlo_from_data()`: run Monte Carlo validation using quantities estimated from supplied data, with optional parameter overrides.
- `summary()` and `plot()`: inspect model results.

## Scope and interpretation

The documented model is restricted to a fully observed **2 × 2 time-by-rater design**. The posterior and Bayes-factor calculations rely on stated approximations, which warrant particular care when the sample is small. The Monte Carlo outcomes describe performance under the calibrated simulation conditions and should not be interpreted as independent validation of modelling assumptions or as causal evidence for an intervention effect.

## Documentation

```r
?babebi
?fit_model
?montecarlo_from_data
```

**Authors:** Irene Gianeselli, Andrea Bosco and Demis Basso. **Maintainer:** Irene Gianeselli. See the CRAN package record for citation and publication metadata.
