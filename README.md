
# babebi

**Bayesian Estimation and Validation for Small-N Designs with Rater Bias**

`babebi` is an R package for approximate Bayesian inference and Monte Carlo validation in small-sample repeated-measures designs involving two measurement occasions and two raters.

The package addresses settings in which observed pre–post differences may be affected by systematic differences between raters. It combines regression-based estimation of pre–post change, approximate posterior inference, and simulation-based assessment of inferential performance.

Version: **0.1.0**

CRAN: https://cran.r-project.org/package=babebi

## Installation

Install the released version from CRAN:

```r
install.packages("babebi")
library(babebi)
```

The package requires R version 4.1.0 or later.

## Statistical framework

The analysis is defined for a fully observed 2 × 2 repeated-measures design comprising:

- Two measurement occasions (pre and post).
- Two raters assessing the same participants at both occasions.
- A continuous numerical outcome.

For each participant and rater, pre–post difference scores are analysed using a linear regression model with a rater indicator as a covariate.

Posterior summaries of the regression coefficients are obtained from a large-sample normal approximation centred at the least-squares estimates, with plug-in covariance under an improper flat prior.

Evidence concerning the pre–post effect, adjusted for differences between raters, is summarised using a BIC-based approximation to the Bayes factor comparing models with and without the pre–post effect.

These procedures provide approximate inference. They do not constitute exact posterior computation or direct numerical integration of marginal likelihoods.

## Main functions

| Function | Description |
|---|---|
| `babebi()` | Runs the model-fitting workflow and optionally performs Monte Carlo validation. |
| `fit_model()` | Fits the regression model and computes approximate posterior summaries. |
| `montecarlo_from_data()` | Performs Monte Carlo validation using design quantities estimated from the observed data. |
| `summary()` | Returns summaries of fitted results. |
| `plot()` | Produces graphical summaries of a fitted model. |

## Example

The following constructed dataset contains four participants, two measurement occasions, and two raters.

```r
library(babebi)

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
```

### Model fitting and validation

```r
res <- babebi(
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

print(res$fit)
summary(res$fit)
plot(res$fit)

print(res$validation)
```

The fitted model and Monte Carlo validation results are stored separately in `res$fit` and `res$validation`.

### Model fitting without validation

```r
fit <- fit_model(
  data = dat,
  id = id,
  time = time,
  rater = rater,
  outcome = y,
  time_order = c("pre", "post"),
  rater_order = c("r1", "r2"),
  n_draws = 500,
  seed = 123
)

summary(fit)
```

### Monte Carlo validation

Monte Carlo validation can be performed separately using `montecarlo_from_data()`.

```r
mc <- montecarlo_from_data(
  data = dat,
  id = id,
  time = time,
  rater = rater,
  outcome = y,
  time_order = c("pre", "post"),
  rater_order = c("r1", "r2"),
  R = 100,
  seed = 123
)

print(mc)
```

The validation procedure uses design quantities estimated from the supplied data, including sample size, mean pre–post change, and the additive discrepancy associated with the second rater.

The Monte Carlo analysis summarises inferential performance using bias, root mean square error, credible interval coverage, posterior tail probabilities, and mean approximate Bayes factor values.

The parameters `N`, `delta`, and `bias_r2` allow the corresponding design quantities to be specified explicitly.

## Interpretation and limitations

`babebi` is intended for small-sample repeated-measures settings in which rater-specific differences may affect the interpretation of observed pre–post change.

Several restrictions govern the interpretation of its results.

1. The model assumes two measurement occasions and two raters, with complete observations required for the fitted design.
2. Posterior summaries rely on a large-sample normal approximation. Their accuracy is not guaranteed by a small sample size.
3. The Bayes factor is approximated using BIC and depends on the assumptions underlying that approximation.
4. Monte Carlo validation is calibrated using quantities obtained from the observed design or specified by the user. Its results are conditional on the simulation assumptions and parameter values.
5. Adjustment for rater differences does not, by itself, establish intervention efficacy, causal effects, or measurement validity.

Simulation-based validation describes the performance of the specified inferential procedure under the evaluated conditions. Its interpretation should remain linked to the statistical model and the data-generating assumptions.

## Reproducibility

The package supports reproducible posterior sampling and Monte Carlo validation through the `seed` argument.

For research applications, the package version, R version, number of posterior draws, simulation replications, and parameter settings should be reported.

```r
packageVersion("babebi")
sessionInfo()
```

## Documentation

Function-level documentation is available in R:

```r
help("babebi", package = "babebi")
help("fit_model", package = "babebi")
help("montecarlo_from_data", package = "babebi")
```

The reference manual and package documentation are available on [CRAN](https://cran.r-project.org/package=babebi).


## Citation

Gianeselli, I., Bosco, A., & Basso, D. (2026). babebi: An R package for Bayesian estimation and validation in small-N two-rater pre–post designs. *Applied Psychological Measurement*. https://doi.org/10.1177/01466216261465754

## Authors

Irene Gianeselli, Andrea Bosco, and Demis Basso.

**Maintainer:** Irene Gianeselli.

## Licence

GPL-3.
