#' Run Monte Carlo calibration from user-supplied data
#'
#' Uses observed data to estimate the sample size, mean pre-post change,
#' and mean additive discrepancy of the second rater for the Monte Carlo
#' engine, then runs a single Monte Carlo validation cell.
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
#' @param R Number of Monte Carlo repetitions.
#' @param N Optional override for the sample size used in the simulation.
#'   Defaults to the observed number of subjects.
#' @param delta Optional override for the pre-post effect used in the
#'   simulation. Defaults to the value estimated from the data.
#' @param bias_r2 Optional override for the additive bias of the second
#'   rater. Defaults to the value estimated from the data.
#' @param seed Optional integer seed used to initialise the random-number
#'   generator for simulation.
#'
#' @return A one-row data frame of class `babebi_validation` with Monte Carlo
#'   performance summaries. The attributes `calibrated_design` and `call`
#'   store the design estimated from the observed data and the original call.
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
#' mc <- montecarlo_from_data(
#'   data = dat,
#'   id = id,
#'   time = time,
#'   rater = rater,
#'   outcome = y,
#'   time_order = c("pre", "post"),
#'   rater_order = c("r1", "r2"),
#'   R = 100,
#'   seed = 123
#' )
#'
#' print(mc)
#'
#' @export
montecarlo_from_data <- function(data,
                                 id,
                                 time,
                                 rater,
                                 outcome,
                                 time_order = NULL,
                                 rater_order = NULL,
                                 R = 1000,
                                 N = NULL,
                                 delta = NULL,
                                 bias_r2 = NULL,
                                 seed = NULL) {

  id_name <- resolve_colname(substitute(id), parent.frame())
  time_name <- resolve_colname(substitute(time), parent.frame())
  rater_name <- resolve_colname(substitute(rater), parent.frame())
  outcome_name <- resolve_colname(substitute(outcome), parent.frame())

  if (!is.numeric(R) || length(R) != 1L || is.na(R) || R <= 0 || R %% 1 != 0) {
    stop("`R` must be a positive integer.", call. = FALSE)
  }

  if (!is.null(N) &&
      (!is.numeric(N) || length(N) != 1L || is.na(N) ||
       N <= 1 || N %% 1 != 0)) {
    stop("`N` must be a single integer greater than 1.", call. = FALSE)
  }

  if (!is.null(delta) &&
      (!is.numeric(delta) || length(delta) != 1L || is.na(delta))) {
    stop("`delta` must be a single numeric value.", call. = FALSE)
  }

  if (!is.null(bias_r2) &&
      (!is.numeric(bias_r2) || length(bias_r2) != 1L || is.na(bias_r2))) {
    stop("`bias_r2` must be a single numeric value.", call. = FALSE)
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

  sim_N <- if (is.null(N)) design$N else as.integer(N)
  sim_delta <- if (is.null(delta)) design$delta else as.numeric(delta)
  sim_bias_r2 <- if (is.null(bias_r2)) design$bias_r2 else as.numeric(bias_r2)

  res <- run_cell(
    N = sim_N,
    delta = sim_delta,
    bias_r2 = sim_bias_r2,
    R = as.integer(R)
  )

  res <- as.data.frame(res)
  res$R <- as.integer(R)

  wanted <- c("N", "delta", "bias_r2", "R",
              "bias", "rmse", "coverage", "ppos_mean", "bf_mean")

  missing <- setdiff(wanted, names(res))
  if (length(missing) > 0) {
    stop(
      "Monte Carlo output is missing required fields: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  res <- res[, wanted, drop = FALSE]

  attr(res, "calibrated_design") <- design
  attr(res, "call") <- match.call()

  class(res) <- c("babebi_validation", "data.frame")

  res
}
