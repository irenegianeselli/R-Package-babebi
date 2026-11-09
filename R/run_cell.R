#' Run one Monte Carlo validation cell
#'
#' Internal helper used to evaluate the analytical babebi model under a single
#' simulation condition defined by sample size, pre-post effect, and rater bias.
#'
#' The current implementation simulates pre-post difference scores directly for
#' the reference rater and the second rater, then applies the same analytical
#' linear-model logic used in `fit_model()`.
#'
#' @param N Integer sample size.
#' @param delta Numeric pre-post effect for the reference rater.
#' @param bias_r2 Numeric additive discrepancy for the second rater.
#' @param R Integer number of Monte Carlo repetitions.
#'
#' @return A one-row data frame with columns `N`, `delta`, `bias_r2`, `bias`,
#'   `rmse`, `coverage`, `ppos_mean`, and `bf_mean`.
#'
#' @keywords internal
run_cell <- function(N, delta, bias_r2, R) {
  
  if (!is.numeric(N) || length(N) != 1L || is.na(N) || N <= 1 || N %% 1 != 0) {
    stop("`N` must be a single integer greater than 1.", call. = FALSE)
  }
  
  if (!is.numeric(delta) || length(delta) != 1L || is.na(delta)) {
    stop("`delta` must be a single numeric value.", call. = FALSE)
  }
  
  if (!is.numeric(bias_r2) || length(bias_r2) != 1L || is.na(bias_r2)) {
    stop("`bias_r2` must be a single numeric value.", call. = FALSE)
  }
  
  if (!is.numeric(R) || length(R) != 1L || is.na(R) || R <= 0 || R %% 1 != 0) {
    stop("`R` must be a positive integer.", call. = FALSE)
  }
  
  beta_hat <- numeric(R)
  covered <- logical(R)
  ppos <- numeric(R)
  bf10 <- numeric(R)
  
  for (r in seq_len(R)) {
    d_ref <- stats::rnorm(n = N, mean = delta, sd = 1)
    d_r2 <- stats::rnorm(n = N, mean = delta + bias_r2, sd = 1)
    
    y <- c(d_ref, d_r2)
    X <- cbind(
      Intercept = 1,
      r2 = c(rep(0, N), rep(1, N))
    )
    
    XtX <- crossprod(X)
    XtY <- crossprod(X, y)
    
    XtX_inv <- solve(XtX)
    bh <- as.vector(XtX_inv %*% XtY)
    
    resid <- y - as.vector(X %*% bh)
    n_obs <- nrow(X)
    p <- ncol(X)
    
    df_resid <- n_obs - p
    sigma2_hat <- sum(resid^2) / df_resid
    V_beta <- sigma2_hat * XtX_inv
    
    eps <- 1e-10
    diag(V_beta) <- diag(V_beta) + eps
    
    se_beta <- sqrt(V_beta[1, 1])
    ci_low <- bh[1] - stats::qnorm(0.975) * se_beta
    ci_high <- bh[1] + stats::qnorm(0.975) * se_beta
    
    beta_hat[r] <- bh[1]
    covered[r] <- (ci_low <= delta) && (delta <= ci_high)
    ppos[r] <- 1 - stats::pnorm(0, mean = bh[1], sd = se_beta)
    
    k_alt <- 2
    rss_alt <- max(sum(resid^2), .Machine$double.eps)
    bic_alt <- n_obs * log(rss_alt / n_obs) + k_alt * log(n_obs)
    
    X0 <- X[, "r2", drop = FALSE]
    XtX0 <- crossprod(X0)
    
    if (as.numeric(XtX0) < .Machine$double.eps) {
      bf10[r] <- NA_real_
    } else {
      bh0 <- as.numeric(crossprod(X0, y) / XtX0)
      resid0 <- y - X0 %*% bh0
      
      k_null <- 1
      rss_null <- max(sum(resid0^2), .Machine$double.eps)
      bic_null <- n_obs * log(rss_null / n_obs) + k_null * log(n_obs)
      
      bf10[r] <- exp((bic_null - bic_alt) / 2)
    }
  }
  
  data.frame(
    N = as.integer(N),
    delta = as.numeric(delta),
    bias_r2 = as.numeric(bias_r2),
    bias = mean(beta_hat - delta),
    rmse = sqrt(mean((beta_hat - delta)^2)),
    coverage = mean(covered),
    ppos_mean = mean(ppos),
    bf_mean = mean(bf10, na.rm = TRUE)
  )
}