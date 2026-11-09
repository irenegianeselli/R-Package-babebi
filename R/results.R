#' Print a babebi fit object
#'
#' @param x An object of class `babebi_fit`.
#' @param digits Number of digits to print.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return The input object, invisibly.
#' @export
print.babebi_fit <- function(x, digits = 3, ...) {

  if (!inherits(x, "babebi_fit")) {
    stop("`x` must be an object of class 'babebi_fit'.", call. = FALSE)
  }

  fmt <- function(z) {
    ifelse(is.finite(z), formatC(z, digits = digits, format = "f"), NA_character_)
  }

  logbf <- if (is.finite(x$bf10) && x$bf10 > 0) log(x$bf10) else NA_real_

  cat("\n")
  cat("babebi model fit\n")
  cat("----------------\n")
  cat(sprintf("beta (pre-post, ref rater): %s\n", fmt(x$beta_hat)))
  cat(sprintf("gamma (rater discrepancy): %s\n", fmt(x$gamma_hat)))
  cat(sprintf("95%% CI for beta: [%s, %s]\n", fmt(x$ci_low), fmt(x$ci_high)))
  cat(sprintf("Pr(beta > 0): %s\n", fmt(x$p_pos)))
  cat(sprintf("BF10 (BIC): %s\n", fmt(x$bf10)))
  cat(sprintf("log(BF10): %s\n", fmt(logbf)))

  if (!is.null(x$design)) {
    cat("\n")
    cat("Calibrated design\n")
    cat("-----------------\n")
    cat(sprintf("N: %d\n", as.integer(x$design$N)))
    cat(sprintf("delta: %s\n", fmt(x$design$delta)))
    cat(sprintf("bias_r2: %s\n", fmt(x$design$bias_r2)))
  }

  invisible(x)
}


#' Plot a babebi fit object
#'
#' @param x An object of class `babebi_fit`.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return No return value, called for its side effects.
#' @export
plot.babebi_fit <- function(x, ...) {

  if (!inherits(x, "babebi_fit")) {
    stop("`x` must be an object of class 'babebi_fit'.", call. = FALSE)
  }

  if (is.null(x$posterior_draws) ||
      !all(c("beta", "gamma") %in% names(x$posterior_draws))) {
    stop("`x$posterior_draws` must contain columns `beta` and `gamma`.", call. = FALSE)
  }

  beta_draws <- x$posterior_draws$beta

  if (length(beta_draws) < 2) {
    stop("Posterior draws are insufficient for density estimation.", call. = FALSE)
  }

  v <- stats::var(beta_draws)

  if (!is.finite(v) || v < .Machine$double.eps) {
    stop("Posterior draws have insufficient variability for density estimation.", call. = FALSE)
  }

  dens <- stats::density(beta_draws)

  graphics::plot(
    dens,
    main = "Posterior distribution of beta",
    xlab = expression(beta),
    ylab = "Density",
    ...
  )

  graphics::abline(v = x$ci_low, lty = 2)
  graphics::abline(v = x$ci_high, lty = 2)
  graphics::abline(v = 0, lty = 3)
  graphics::abline(v = x$beta_hat, lwd = 2)
}


#' Summarise a babebi fit object
#'
#' @param object An object of class `babebi_fit`.
#' @param digits Number of digits to print.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return An object of class `summary.babebi_fit`.
#' @export
summary.babebi_fit <- function(object, digits = 3, ...) {

  if (!inherits(object, "babebi_fit")) {
    stop("`object` must be an object of class 'babebi_fit'.", call. = FALSE)
  }

  logbf <- if (is.finite(object$bf10) && object$bf10 > 0) log(object$bf10) else NA_real_

  out <- list(
    call = match.call(),
    coefficients = data.frame(
      parameter = c("beta", "gamma"),
      estimate = c(object$beta_hat, object$gamma_hat)
    ),
    beta_interval = data.frame(
      parameter = "beta",
      ci_low = object$ci_low,
      ci_high = object$ci_high,
      p_pos = object$p_pos
    ),
    evidence = data.frame(
      bf10 = object$bf10,
      log_bf10 = logbf
    ),
    design = object$design,
    digits = digits
  )

  class(out) <- "summary.babebi_fit"
  out
}


#' Print a summary of a babebi fit object
#'
#' @param x An object of class `summary.babebi_fit`.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return The input object, invisibly.
#' @export
print.summary.babebi_fit <- function(x, ...) {

  if (!inherits(x, "summary.babebi_fit")) {
    stop("`x` must be an object of class 'summary.babebi_fit'.", call. = FALSE)
  }

  digits <- x$digits

  fmt <- function(z) {
    ifelse(is.finite(z), formatC(z, digits = digits, format = "f"), NA_character_)
  }

  cat("\n")
  cat("Summary of babebi model fit\n")
  cat("---------------------------\n")

  coef_tab <- x$coefficients
  coef_tab$estimate <- fmt(coef_tab$estimate)
  print(coef_tab, row.names = FALSE)

  cat("\n")
  int_tab <- x$beta_interval
  int_tab$ci_low <- fmt(int_tab$ci_low)
  int_tab$ci_high <- fmt(int_tab$ci_high)
  int_tab$p_pos <- fmt(int_tab$p_pos)
  print(int_tab, row.names = FALSE)

  cat("\n")
  ev_tab <- x$evidence
  ev_tab$bf10 <- fmt(ev_tab$bf10)
  ev_tab$log_bf10 <- fmt(ev_tab$log_bf10)
  print(ev_tab, row.names = FALSE)

  if (!is.null(x$design)) {
    cat("\n")
    des_tab <- data.frame(
      N = as.integer(x$design$N),
      delta = fmt(x$design$delta),
      bias_r2 = fmt(x$design$bias_r2)
    )
    print(des_tab, row.names = FALSE)
  }

  invisible(x)
}


#' Print a babebi validation object
#'
#' @param x An object of class `babebi_validation`.
#' @param digits Number of digits to print.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return The input object, invisibly.
#' @export
print.babebi_validation <- function(x, digits = 3, ...) {

  if (!inherits(x, "babebi_validation")) {
    stop("`x` must be an object of class 'babebi_validation'.", call. = FALSE)
  }

  if (!is.data.frame(x) || nrow(x) != 1L) {
    stop("`x` must be a one-row data frame.", call. = FALSE)
  }

  fmt <- function(z) {
    ifelse(is.finite(z), formatC(z, digits = digits, format = "f"), NA_character_)
  }

  bf_val <- x$bf_mean[1]
  logbf <- if (is.finite(bf_val) && bf_val > 0) log(bf_val) else NA_real_

  cat("\n")
  cat("babebi Monte Carlo validation\n")
  cat("-----------------------------\n")
  cat(sprintf("N: %d\n", as.integer(x$N[1])))
  cat(sprintf("delta: %s\n", fmt(x$delta[1])))
  cat(sprintf("bias_r2: %s\n", fmt(x$bias_r2[1])))
  cat(sprintf("R: %d\n", as.integer(x$R[1])))

  cat("\n")
  cat(sprintf("bias: %s\n", fmt(x$bias[1])))
  cat(sprintf("RMSE: %s\n", fmt(x$rmse[1])))
  cat(sprintf("coverage: %s\n", fmt(x$coverage[1])))
  cat(sprintf("mean Pr(beta > 0): %s\n", fmt(x$ppos_mean[1])))
  cat(sprintf("mean BF10: %s\n", fmt(bf_val)))
  cat(sprintf("log(mean BF10): %s\n", fmt(logbf)))

  design <- attr(x, "calibrated_design")

  if (!is.null(design)) {
    cat("\n")
    cat("Calibrated design from observed data\n")
    cat("------------------------------------\n")
    cat(sprintf("N: %d\n", as.integer(design$N)))
    cat(sprintf("delta: %s\n", fmt(design$delta)))
    cat(sprintf("bias_r2: %s\n", fmt(design$bias_r2)))
  }

  invisible(x)
}
