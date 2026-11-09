library(testthat)
library(babebi)

test_that("montecarlo_from_data returns a babebi_validation object with expected structure", {
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
    R = 100,
    seed = 123
  )

  expect_s3_class(mc, "babebi_validation")
  expect_s3_class(mc, "data.frame")
  expect_equal(nrow(mc), 1)

  expect_identical(
    names(mc),
    c("N", "delta", "bias_r2", "R",
      "bias", "rmse", "coverage", "ppos_mean", "bf_mean")
  )

  expect_equal(mc$N, 4)
  expect_equal(mc$R, 100)

  expect_true(is.numeric(mc$delta))
  expect_true(is.numeric(mc$bias_r2))
  expect_true(is.numeric(mc$bias))
  expect_true(is.numeric(mc$rmse))
  expect_true(is.numeric(mc$coverage))
  expect_true(is.numeric(mc$ppos_mean))
  expect_true(is.numeric(mc$bf_mean))

  expect_true(!is.null(attr(mc, "calibrated_design")))
  expect_true(!is.null(attr(mc, "call")))

  design <- attr(mc, "calibrated_design")
  expect_type(design, "list")
  expect_identical(
    names(design),
    c("N", "delta", "bias_r2", "time_levels", "rater_levels")
  )
})

test_that("montecarlo_from_data returns coherent calibrated design values", {
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
    R = 50,
    seed = 123
  )

  design <- attr(mc, "calibrated_design")

  expect_equal(design$N, 4)
  expect_equal(design$delta, 0.675, tolerance = 1e-10)
  expect_equal(design$bias_r2, 0.175, tolerance = 1e-10)
  expect_identical(design$time_levels, c("pre", "post"))
  expect_identical(design$rater_levels, c("r1", "r2"))
})

test_that("montecarlo_from_data respects user overrides for N, delta, and bias_r2", {
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
    R = 20,
    N = 10,
    delta = 0.50,
    bias_r2 = 0.25,
    seed = 123
  )

  expect_equal(mc$N, 10)
  expect_equal(mc$delta, 0.50, tolerance = 1e-10)
  expect_equal(mc$bias_r2, 0.25, tolerance = 1e-10)

  design <- attr(mc, "calibrated_design")
  expect_equal(design$N, 4)
  expect_equal(design$delta, 0.675, tolerance = 1e-10)
  expect_equal(design$bias_r2, 0.175, tolerance = 1e-10)
})

test_that("montecarlo_from_data is reproducible when seed is fixed", {
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

  mc1 <- montecarlo_from_data(
    data = dat,
    id = id,
    time = time,
    rater = rater,
    outcome = y,
    time_order = c("pre", "post"),
    rater_order = c("r1", "r2"),
    R = 50,
    seed = 999
  )

  mc2 <- montecarlo_from_data(
    data = dat,
    id = id,
    time = time,
    rater = rater,
    outcome = y,
    time_order = c("pre", "post"),
    rater_order = c("r1", "r2"),
    R = 50,
    seed = 999
  )

  expect_equal(mc1, mc2)
  expect_equal(attr(mc1, "calibrated_design"), attr(mc2, "calibrated_design"))
})

test_that("montecarlo_from_data accepts character column names", {
  dat <- data.frame(
    subj = rep(1:4, each = 4),
    occasion = rep(c("pre", "pre", "post", "post"), times = 4),
    judge = rep(c("r1", "r2"), times = 8),
    score = c(
      3.0, 3.2, 3.8, 4.0,
      2.9, 3.1, 3.5, 3.7,
      3.4, 3.6, 4.0, 4.1,
      3.1, 3.3, 3.9, 4.0
    )
  )

  mc <- montecarlo_from_data(
    data = dat,
    id = "subj",
    time = "occasion",
    rater = "judge",
    outcome = "score",
    time_order = c("pre", "post"),
    rater_order = c("r1", "r2"),
    R = 25,
    seed = 123
  )

  expect_s3_class(mc, "babebi_validation")
  expect_equal(nrow(mc), 1)
  expect_equal(mc$R, 25)
})

test_that("montecarlo_from_data fails when R is not a positive integer", {
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

  expect_error(
    montecarlo_from_data(
      data = dat,
      id = id,
      time = time,
      rater = rater,
      outcome = y,
      R = 0
    ),
    "`R` must be a positive integer"
  )

  expect_error(
    montecarlo_from_data(
      data = dat,
      id = id,
      time = time,
      rater = rater,
      outcome = y,
      R = 10.5
    ),
    "`R` must be a positive integer"
  )
})

test_that("montecarlo_from_data fails when N is not a valid integer greater than 1", {
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

  expect_error(
    montecarlo_from_data(
      data = dat,
      id = id,
      time = time,
      rater = rater,
      outcome = y,
      N = 1
    ),
    "`N` must be a single integer greater than 1"
  )

  expect_error(
    montecarlo_from_data(
      data = dat,
      id = id,
      time = time,
      rater = rater,
      outcome = y,
      N = 2.5
    ),
    "`N` must be a single integer greater than 1"
  )
})

test_that("montecarlo_from_data fails when delta is not a single numeric value", {
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

  expect_error(
    montecarlo_from_data(
      data = dat,
      id = id,
      time = time,
      rater = rater,
      outcome = y,
      delta = c(0.1, 0.2)
    ),
    "`delta` must be a single numeric value"
  )
})

test_that("montecarlo_from_data fails when bias_r2 is not a single numeric value", {
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

  expect_error(
    montecarlo_from_data(
      data = dat,
      id = id,
      time = time,
      rater = rater,
      outcome = y,
      bias_r2 = c(0.1, 0.2)
    ),
    "`bias_r2` must be a single numeric value"
  )
})

test_that("montecarlo_from_data fails when seed is not an integer", {
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

  expect_error(
    montecarlo_from_data(
      data = dat,
      id = id,
      time = time,
      rater = rater,
      outcome = y,
      seed = 1.5
    ),
    "`seed` must be a single integer value"
  )
})
