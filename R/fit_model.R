#' Fit the babebi model to observed data
#'
#' Fits the current babebi model to a fully observed 2 (time) x 2 (rater)
#' repeated-measures dataset. Estimation is based on difference scores and a
#' linear model including a rater indicator as covariate.
#'
#' Posterior draws are obtained from a large-sample normal approximation to
#' the posterior of the regression coefficients under an improper flat prior.
#'
#' @param data A data frame in long format.
#' @param id Unquoted column name identifying subjects or a character string.
#' @param time Unquoted column name identifying time points or a character string.
#' @param rater Unquoted column name identifying raters or a character string.
#' @param outcome Unquoted column name containing the observed score or a
#'   character string.
#' @param time_order Optional character vector of length 2 giving the
#'   order of the time factor: baseline first, follow-up second.
#' @param rater_order Optional character vector of length 2 giving the
#'   order of the rater factor: reference rater first, second rater second.
#' @param n_draws Number of posterior draws used for approximation.
#' @param seed Optional integer seed used to initialise the random-number
#'   generator for posterior sampling.
#'
#' @return A named list of class `babebi_fit`.
#'
#' @examples
#' dat <- data.frame(
#'   id = rep(1:4, each = 4),
#'   time = rep(c("pre", "pre", "post", "post"), times = 4),
#'   rater = rep(c("r1", "r2"), times = 8),
#'   y = c(
#'     3.0, 3.2, 3.8, 4.0,
#'     2.9, 3.1, 3.5, 3.7,
#'     3.4, 3.6, 4.0, 4.1,
#'     3.1, 3.3, 3.9, 4.0
#'   )
#' )
#'
#' fit <- fit_model(
#'   data = dat,
#'   id = id,
#'   time = time,
#'   rater = rater,
#'   outcome = y,
#'   time_order = c("pre", "post"),
#'   rater_order = c("r1", "r2"),
#'   n_draws = 500,
#'   seed = 123
#' )
#'
#' print(fit)
#' summary(fit)
#' plot(fit)
#'
#' @export
fit_model <- function(data,
                      id,
                      time,
                      rater,
                      outcome,
                      time_order = NULL,
                      rater_order = NULL,
                      n_draws = 4000,
                      seed = NULL) {

  id_name <- resolve_colname(substitute(id), parent.frame())
  time_name <- resolve_colname(substitute(time), parent.frame())
  rater_name <- resolve_colname(substitute(rater), parent.frame())
  outcome_name <- resolve_colname(substitute(outcome), parent.frame())

  if (!is.numeric(n_draws) || length(n_draws) != 1L || is.na(n_draws) ||
      n_draws <= 0 || n_draws %% 1 != 0) {
    stop("`n_draws` must be a positive integer.", call. = FALSE)
  }

  if (!is.null(seed) &&
      (!is.numeric(seed) || length(seed) != 1L || is.na(seed) ||
       seed %% 1 != 0)) {
    stop("`seed` must be a single integer value.", call. = FALSE)
  }

  if (!is.null(seed)) {
    set.seed(as.integer(seed))
  }

  dat <- prepare_data(
    data = data,
    id = id_name,
    time = time_name,
    rater = rater_name,
    outcome = outcome_name,
    time_order = time_order,
    rater_order = rater_order
  )

  design <- extract_design(dat)

  time_levels <- levels(dat$time)
  rater_levels <- levels(dat$rater)

  baseline <- time_levels[1]
  followup <- time_levels[2]
  second_rater <- rater_levels[2]

  wide_time <- stats::reshape(
    dat,
    idvar = c("id", "rater"),
    timevar = "time",
    direction = "wide"
  )

  base_col <- make.names(paste0("y.", baseline))
  follow_col <- make.names(paste0("y.", followup))

  if (!all(c(base_col, follow_col) %in% names(wide_time))) {
    stop("Failed to reconstruct pre-post outcome columns after reshaping.", call. = FALSE)
  }

  y_base <- wide_time[[base_col]]
  y_follow <- wide_time[[follow_col]]

  if (anyNA(y_base) || anyNA(y_follow)) {
    stop("Incomplete pre-post data detected after reshaping.", call. = FALSE)
  }

  wide_time$d <- y_follow - y_base
  wide_time$r2 <- as.numeric(wide_time$rater == second_rater)

  X <- cbind(Intercept = 1, r2 = wide_time$r2)
  y <- wide_time$d

  XtX <- crossprod(X)
  XtY <- crossprod(X, y)

  XtX_inv <- solve(XtX)
  bh <- as.vector(XtX_inv %*% XtY)

  resid <- y - as.vector(X %*% bh)
  n <- nrow(X)
  p <- ncol(X)

  df_resid <- n - p
  if (df_resid <= 0) {
    stop("Insufficient degrees of freedom for variance estimation.", call. = FALSE)
  }

  sigma2_hat <- sum(resid^2) / df_resid
  V_beta <- sigma2_hat * XtX_inv

  eps <- 1e-10
  diag(V_beta) <- diag(V_beta) + eps

  L <- chol(V_beta)
  Z <- matrix(stats::rnorm(as.integer(n_draws) * p), ncol = p)
  theta_draws <- sweep(Z %*% t(L), 2, bh, `+`)

  beta_draws <- theta_draws[, 1]
  gamma_draws <- theta_draws[, 2]

  ci <- stats::quantile(beta_draws, c(0.025, 0.975), names = FALSE)
  p_pos <- mean(beta_draws > 0)

  k_alt <- 2
  rss_alt <- max(sum(resid^2), .Machine$double.eps)
  bic_alt <- n * log(rss_alt / n) + k_alt * log(n)

  X0 <- X[, "r2", drop = FALSE]
  XtX0 <- crossprod(X0)

  if (as.numeric(XtX0) < .Machine$double.eps) {
    stop("Degenerate design: rater indicator has no variability.", call. = FALSE)
  }

  bh0 <- as.numeric(crossprod(X0, y) / XtX0)
  resid0 <- y - X0 %*% bh0

  k_null <- 1
  rss_null <- max(sum(resid0^2), .Machine$double.eps)
  bic_null <- n * log(rss_null / n) + k_null * log(n)

  bf10 <- exp((bic_null - bic_alt) / 2)

  res <- list(
    beta_hat = unname(bh[1]),
    gamma_hat = unname(bh[2]),
    ci_low = ci[1],
    ci_high = ci[2],
    p_pos = p_pos,
    bf10 = bf10,
    posterior_draws = data.frame(
      beta = as.numeric(beta_draws),
      gamma = as.numeric(gamma_draws)
    ),
    design = design
  )

  class(res) <- "babebi_fit"

  res
}
