#' Standardise user data for babebi
#'
#' Internal helper used to convert user-supplied data into the format
#' required by the estimation and Monte Carlo engine.
#'
#' The function assumes a fully observed 2 (time) × 2 (rater) repeated-measures
#' design. Each subject must contribute exactly one observation for each
#' combination of time point and rater.
#'
#' If `time_order` or `rater_order` are not provided, the ordering of factor
#' levels follows the order of appearance in the data. Users are encouraged
#' to specify these arguments explicitly when the ordering carries substantive
#' meaning (e.g., pre vs post).
#'
#' @param data A data frame in long format.
#' @param id Unquoted column name identifying subjects or a character string.
#' @param time Unquoted column name identifying time points or a character string.
#' @param rater Unquoted column name identifying raters or a character string.
#' @param outcome Unquoted column name containing the observed score or a character string.
#' @param time_order Optional character vector of length 2 specifying the
#'   order of the time factor (e.g., `c("pre", "post")`).
#' @param rater_order Optional character vector of length 2 specifying the
#'   order of the rater factor (reference rater first).
#'
#' @return A data frame with four standardised columns: `id`, `time`,
#'   `rater`, and `y`.
#'
#' @keywords internal
prepare_data <- function(data,
                         id,
                         time,
                         rater,
                         outcome,
                         time_order = NULL,
                         rater_order = NULL) {

  id_name <- resolve_colname(substitute(id), parent.frame())
  time_name <- resolve_colname(substitute(time), parent.frame())
  rater_name <- resolve_colname(substitute(rater), parent.frame())
  outcome_name <- resolve_colname(substitute(outcome), parent.frame())

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  required_cols <- c(id_name, time_name, rater_name, outcome_name)
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  dat <- data[, required_cols, drop = FALSE]
  names(dat) <- c("id", "time", "rater", "y")

  if (anyNA(dat)) {
    stop(
      "Missing values in `id`, `time`, `rater`, or `outcome` are not supported.",
      call. = FALSE
    )
  }

  if (!is.numeric(dat$y)) {
    stop("`outcome` must be numeric.", call. = FALSE)
  }

  dat$id <- as.factor(dat$id)
  dat$time <- as.character(dat$time)
  dat$rater <- as.character(dat$rater)

  observed_time <- unique(dat$time)
  observed_rater <- unique(dat$rater)

  if (length(observed_time) != 2) {
    stop(
      "`time` must contain exactly two distinct values. Found: ",
      paste(observed_time, collapse = ", "),
      call. = FALSE
    )
  }

  if (length(observed_rater) != 2) {
    stop(
      "`rater` must contain exactly two distinct values. Found: ",
      paste(observed_rater, collapse = ", "),
      call. = FALSE
    )
  }

  if (is.null(time_order)) {
    time_order <- observed_time
  }

  if (is.null(rater_order)) {
    rater_order <- observed_rater
  }

  if (length(time_order) != 2) {
    stop("`time_order` must have length 2.", call. = FALSE)
  }

  if (length(rater_order) != 2) {
    stop("`rater_order` must have length 2.", call. = FALSE)
  }

  if (!setequal(time_order, observed_time)) {
    stop(
      "`time_order` must contain exactly the same values as the observed `time` levels. Observed: ",
      paste(observed_time, collapse = ", "),
      call. = FALSE
    )
  }

  if (!setequal(rater_order, observed_rater)) {
    stop(
      "`rater_order` must contain exactly the same values as the observed `rater` levels. Observed: ",
      paste(observed_rater, collapse = ", "),
      call. = FALSE
    )
  }

  dat$time <- factor(dat$time, levels = time_order)
  dat$rater <- factor(dat$rater, levels = rater_order)

  counts <- stats::xtabs(~ id + time + rater, data = dat)

  if (any(counts != 1L)) {
    stop(
      "Each subject must have exactly one observation for each combination of time and rater.",
      call. = FALSE
    )
  }

  dat
}
