# Basic babebi workflow
# ---------------------
# This script illustrates the recommended user workflow:
# 1. organise data in long format
# 2. run the full babebi workflow
# 3. inspect model fit results
# 4. inspect Monte Carlo validation results

# Example data in long format:
# one row = one subject x time x rater observation
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

# Inspect the input structure
dat

# Run the full babebi workflow:
# - fit the analytical Bayesian model
# - run Monte Carlo validation calibrated on the observed design
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

# The returned object is a list with:
# - res$fit
# - res$validation
# - res$call
names(res)

# ------------------------------------------------------------------
# 1. Inspect model fit results
# ------------------------------------------------------------------
print(res$fit)
summary(res$fit)
plot(res$fit)

# Access individual fit components directly if needed
res$fit$beta_hat
res$fit$gamma_hat
res$fit$bf10

# ------------------------------------------------------------------
# 2. Inspect Monte Carlo validation results
# ------------------------------------------------------------------
print(res$validation)

# Access validation metrics directly if needed
res$validation$bias
res$validation$rmse
res$validation$coverage
res$validation$ppos_mean
res$validation$bf_mean

# ------------------------------------------------------------------
# 3. Optional: run only the analytical model, without validation
# ------------------------------------------------------------------
res_fit_only <- babebi(
  data = dat,
  id = id,
  time = time,
  rater = rater,
  outcome = y,
  time_order = c("pre", "post"),
  rater_order = c("r1", "r2"),
  validate = FALSE,
  n_draws = 500,
  seed = 123
)

print(res_fit_only$fit)
res_fit_only$validation
