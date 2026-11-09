test_that("babebi validation component is coherent with direct montecarlo_from_data output", {
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

  res <- babebi(
    data = dat,
    id = id,
    time = time,
    rater = rater,
    outcome = y,
    time_order = c("pre", "post"),
    rater_order = c("r1", "r2"),
    validate = TRUE,
    R = 50,
    n_draws = 500,
    seed = 123
  )

  mc <- montecarlo_from_data(
    data = dat,
    id = id,
    time = time,
    rater = rater,
    outcome = y,
    time_order = c("pre", "post"),
    rater_order = c("r1", "r2"),
    R = 50,
    seed = 123
  )

  expect_equal(
    unclass(res$validation),
    unclass(mc),
    ignore_attr = TRUE
  )

  expect_equal(
    attr(res$validation, "calibrated_design"),
    attr(mc, "calibrated_design")
  )
})
