#' Run the babebi workflow on user-supplied data
#'
#' Fits the babebi model to observed data and, optionally, performs Monte Carlo
#' validation calibrated on the observed design.
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
#' @param validate Logical; if `TRUE`, run Monte Carlo validation after model fitting.
#' @param R Number of Monte Carlo repetitions when `validate = TRUE`.
#' @param N Optional override for the sample size used in validation.
#' @param delta Optional override for the pre-post effect used in validation.
#' @param bias_r2 Optional override for the additive bias of the second rater
#'   used in validation.
#' @param n_draws Number of posterior draws used in model fitting.
#' @param seed Optional integer seed for reproducibility.
#'
#' @return A named list containing:
#' \describe{
#'   \item{fit}{A `babebi_fit` object returned by `fit_model()`.}
#'   \item{validation}{A `babebi_validation` object returned by
#'   `montecarlo_from_data()` when `validate = TRUE`, otherwise `NULL`.}
#'   \item{call}{The original function call.}
#' }
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
#' res <- babebi(
#'   data = dat,
#'   id = id,
#'   time = time,
#'   rater = rater,
#'   outcome = y,
#'   time_order = c("pre", "post"),
#'   rater_order = c("r1", "r2"),
#'   validate = TRUE,
#'   R = 100,
#'   n_draws = 500,
#'   seed = 123
#' )
#'
#' print(res$fit)
#' summary(res$fit)
#' plot(res$fit)
#' print(res$validation)
#'
#' @export
babebi <- function(data,
                   id,
                   time,
                   rater,
                   outcome,
                   time_order = NULL,
                   rater_order = NULL,
                   validate = TRUE,
                   R = 1000,
                   N = NULL,
                   delta = NULL,
                   bias_r2 = NULL,
                   n_draws = 4000,
                   seed = NULL) {

  id_name <- resolve_colname(substitute(id), parent.frame())
  time_name <- resolve_colname(substitute(time), parent.frame())
  rater_name <- resolve_colname(substitute(rater), parent.frame())
  outcome_name <- resolve_colname(substitute(outcome), parent.frame())

  if (!is.logical(validate) || length(validate) != 1L || is.na(validate)) {
    stop("`validate` must be a single TRUE or FALSE value.", call. = FALSE)
  }

  fit <- fit_model(
    data = data,
    id = id_name,
    time = time_name,
    rater = rater_name,
    outcome = outcome_name,
    time_order = time_order,
    rater_order = rater_order,
    n_draws = n_draws,
    seed = seed
  )

  validation <- NULL

  if (isTRUE(validate)) {
    validation <- montecarlo_from_data(
      data = data,
      id = id_name,
      time = time_name,
      rater = rater_name,
      outcome = outcome_name,
      time_order = time_order,
      rater_order = rater_order,
      R = R,
      N = N,
      delta = delta,
      bias_r2 = bias_r2,
      seed = seed
    )
  }

  list(
    fit = fit,
    validation = validation,
    call = match.call()
  )
}
