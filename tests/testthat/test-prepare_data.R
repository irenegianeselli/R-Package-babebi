library(testthat)
library(babebi)

test_that("prepare_data standardises a valid dataset correctly", {
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

  out <- babebi:::prepare_data(
    data = dat,
    id = id,
    time = time,
    rater = rater,
    outcome = y,
    time_order = c("pre", "post"),
    rater_order = c("r1", "r2")
  )

  expect_s3_class(out, "data.frame")
  expect_identical(names(out), c("id", "time", "rater", "y"))
  expect_true(is.factor(out$id))
  expect_true(is.factor(out$time))
  expect_true(is.factor(out$rater))
  expect_identical(levels(out$time), c("pre", "post"))
  expect_identical(levels(out$rater), c("r1", "r2"))
  expect_true(is.numeric(out$y))
  expect_equal(nrow(out), 16)
})

test_that("prepare_data fails when required columns are missing", {
  dat <- data.frame(
    id = rep(1:2, each = 4),
    time = rep(c("pre", "pre", "post", "post"), times = 2),
    y = c(1, 2, 3, 4, 5, 6, 7, 8)
  )

  expect_error(
    babebi:::prepare_data(
      data = dat,
      id = id,
      time = time,
      rater = rater,
      outcome = y
    ),
    "Missing required columns"
  )
})

test_that("prepare_data fails when outcome is not numeric", {
  dat <- data.frame(
    id = rep(1:2, each = 4),
    time = rep(c("pre", "pre", "post", "post"), times = 2),
    rater = rep(c("r1", "r2"), times = 4),
    y = as.character(1:8)
  )

  expect_error(
    babebi:::prepare_data(
      data = dat,
      id = id,
      time = time,
      rater = rater,
      outcome = y
    ),
    "`outcome` must be numeric"
  )
})

test_that("prepare_data fails when time has more than two distinct values", {
  dat <- data.frame(
    id = c(1, 1, 1, 2, 2, 2),
    time = c("pre", "mid", "post", "pre", "mid", "post"),
    rater = c("r1", "r1", "r1", "r2", "r2", "r2"),
    y = c(1, 2, 3, 4, 5, 6)
  )

  expect_error(
    babebi:::prepare_data(
      data = dat,
      id = id,
      time = time,
      rater = rater,
      outcome = y
    ),
    "`time` must contain exactly two distinct values"
  )
})

test_that("prepare_data fails when the design is not fully observed", {
  dat <- data.frame(
    id = c(1, 1, 1, 1, 2, 2, 2),
    time = c("pre", "pre", "post", "post", "pre", "pre", "post"),
    rater = c("r1", "r2", "r1", "r2", "r1", "r2", "r1"),
    y = c(1, 2, 3, 4, 5, 6, 7)
  )

  expect_error(
    babebi:::prepare_data(
      data = dat,
      id = id,
      time = time,
      rater = rater,
      outcome = y,
      time_order = c("pre", "post"),
      rater_order = c("r1", "r2")
    ),
    "Each subject must have exactly one observation"
  )
})
