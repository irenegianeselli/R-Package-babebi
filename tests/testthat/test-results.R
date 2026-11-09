library(testthat)
library(babebi)

test_that("print.babebi_fit returns the object invisibly", {
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

  fit <- fit_model(
    data = dat,
    id = id,
    time = time,
    rater = rater,
    outcome = y,
    time_order = c("pre", "post"),
    rater_order = c("r1", "r2"),
    n_draws = 200,
    seed = 123
  )

  expect_invisible(print(fit))
})

test_that("summary.babebi_fit returns a summary object with expected structure", {
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

  fit <- fit_model(
    data = dat,
    id = id,
    time = time,
    rater = rater,
    outcome = y,
    time_order = c("pre", "post"),
    rater_order = c("r1", "r2"),
    n_draws = 200,
    seed = 123
  )

  s <- summary(fit)

  expect_s3_class(s, "summary.babebi_fit")
  expect_identical(
    names(s),
    c("call", "coefficients", "beta_interval", "evidence", "design", "digits")
  )
  expect_s3_class(s$coefficients, "data.frame")
  expect_s3_class(s$beta_interval, "data.frame")
  expect_s3_class(s$evidence, "data.frame")
})

test_that("print.summary.babebi_fit returns the summary object invisibly", {
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

  fit <- fit_model(
    data = dat,
    id = id,
    time = time,
    rater = rater,
    outcome = y,
    time_order = c("pre", "post"),
    rater_order = c("r1", "r2"),
    n_draws = 200,
    seed = 123
  )

  s <- summary(fit)

  expect_invisible(print(s))
})

test_that("plot.babebi_fit runs without error on valid posterior draws", {
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

  fit <- fit_model(
    data = dat,
    id = id,
    time = time,
    rater = rater,
    outcome = y,
    time_order = c("pre", "post"),
    rater_order = c("r1", "r2"),
    n_draws = 200,
    seed = 123
  )

  expect_no_error(plot(fit))
})

test_that("plot.babebi_fit fails when posterior draws are invalid", {
  bad_fit <- list(
    posterior_draws = data.frame(beta = rep(1, 10), gamma = rep(0, 10)),
    ci_low = 1,
    ci_high = 1,
    beta_hat = 1
  )
  class(bad_fit) <- "babebi_fit"

  expect_error(
    plot(bad_fit),
    "Posterior draws have insufficient variability for density estimation"
  )
})

test_that("print.babebi_validation returns the object invisibly", {
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

  mc <- montecarlo_from_data(
    data = dat,
    id = id,
    time = time,
    rater = rater,
    outcome = y,
    time_order = c("pre", "post"),
    rater_order = c("r1", "r2"),
    R = 25,
    seed = 123
  )

  expect_invisible(print(mc))
})

test_that("print methods fail on wrong object classes", {
  expect_error(
    print.babebi_fit(list()),
    "`x` must be an object of class 'babebi_fit'"
  )

  expect_error(
    summary.babebi_fit(list()),
    "`object` must be an object of class 'babebi_fit'"
  )

  expect_error(
    print.summary.babebi_fit(list()),
    "`x` must be an object of class 'summary.babebi_fit'"
  )

  expect_error(
    plot.babebi_fit(list()),
    "`x` must be an object of class 'babebi_fit'"
  )

  expect_error(
    print.babebi_validation(list()),
    "`x` must be an object of class 'babebi_validation'"
  )
})
