library(testthat)
library(babebi)

test_that("extract_design returns the expected structure on valid standardised data", {
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

  dat_std <- babebi:::prepare_data(
    data = dat,
    id = id,
    time = time,
    rater = rater,
    outcome = y,
    time_order = c("pre", "post"),
    rater_order = c("r1", "r2")
  )

  out <- babebi:::extract_design(dat_std)

  expect_type(out, "list")
  expect_identical(
    names(out),
    c("N", "delta", "bias_r2", "time_levels", "rater_levels")
  )
  expect_equal(out$N, 4)
  expect_true(is.numeric(out$delta))
  expect_length(out$delta, 1)
  expect_true(is.numeric(out$bias_r2))
  expect_length(out$bias_r2, 1)
  expect_identical(out$time_levels, c("pre", "post"))
  expect_identical(out$rater_levels, c("r1", "r2"))
})

test_that("extract_design computes expected delta and bias_r2 values", {
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

  dat_std <- babebi:::prepare_data(
    data = dat,
    id = id,
    time = time,
    rater = rater,
    outcome = y,
    time_order = c("pre", "post"),
    rater_order = c("r1", "r2")
  )

  out <- babebi:::extract_design(dat_std)

  expect_equal(out$delta, 0.675, tolerance = 1e-10)
  expect_equal(out$bias_r2, 0.175, tolerance = 1e-10)
})

test_that("extract_design fails when required columns are missing", {
  bad_dat <- data.frame(
    id = factor(c(1, 1, 2, 2)),
    time = factor(c("pre", "post", "pre", "post")),
    y = c(1, 2, 3, 4)
  )

  expect_error(
    babebi:::extract_design(bad_dat),
    "`data` must contain the columns"
  )
})

test_that("extract_design fails when time is not a factor", {
  bad_dat <- data.frame(
    id = factor(rep(1:2, each = 4)),
    time = c("pre", "pre", "post", "post", "pre", "pre", "post", "post"),
    rater = factor(rep(c("r1", "r2"), times = 4)),
    y = c(1, 2, 3, 4, 5, 6, 7, 8)
  )

  expect_error(
    babebi:::extract_design(bad_dat),
    "`time` must be a factor with exactly two levels"
  )
})

test_that("extract_design fails when rater is not a factor", {
  bad_dat <- data.frame(
    id = factor(rep(1:2, each = 4)),
    time = factor(rep(c("pre", "pre", "post", "post"), times = 2)),
    rater = c("r1", "r2", "r1", "r2", "r1", "r2", "r1", "r2"),
    y = c(1, 2, 3, 4, 5, 6, 7, 8)
  )

  expect_error(
    babebi:::extract_design(bad_dat),
    "`rater` must be a factor with exactly two levels"
  )
})

test_that("extract_design fails when time does not have exactly two levels", {
  bad_dat <- data.frame(
    id = factor(c(1, 1, 1, 2, 2, 2)),
    time = factor(c("pre", "mid", "post", "pre", "mid", "post")),
    rater = factor(c("r1", "r1", "r1", "r2", "r2", "r2")),
    y = c(1, 2, 3, 4, 5, 6)
  )

  expect_error(
    babebi:::extract_design(bad_dat),
    "`time` must be a factor with exactly two levels"
  )
})

test_that("extract_design fails when rater does not have exactly two levels", {
  bad_dat <- data.frame(
    id = factor(c(1, 1, 1, 2, 2, 2)),
    time = factor(c("pre", "pre", "post", "pre", "pre", "post")),
    rater = factor(c("r1", "r2", "r3", "r1", "r2", "r3")),
    y = c(1, 2, 3, 4, 5, 6)
  )

  expect_error(
    babebi:::extract_design(bad_dat),
    "`rater` must be a factor with exactly two levels"
  )
})

test_that("extract_design fails when outcome is not numeric", {
  bad_dat <- data.frame(
    id = factor(rep(1:2, each = 4)),
    time = factor(rep(c("pre", "pre", "post", "post"), times = 2)),
    rater = factor(rep(c("r1", "r2"), times = 4)),
    y = as.character(1:8)
  )

  expect_error(
    babebi:::extract_design(bad_dat),
    "`y` must be numeric"
  )
})

test_that("extract_design fails when reshaped pre-post data are incomplete", {
  dat <- data.frame(
    id = factor(c(1, 1, 1, 2, 2, 2)),
    time = factor(c("pre", "pre", "post", "pre", "pre", "post"),
                  levels = c("pre", "post")),
    rater = factor(c("r1", "r2", "r1", "r1", "r2", "r1"),
                   levels = c("r1", "r2")),
    y = c(1, 2, 3, 4, 5, 6)
  )

  expect_error(
    babebi:::extract_design(dat),
    "Incomplete pre-post data detected after reshaping by subject and rater"
  )
})
