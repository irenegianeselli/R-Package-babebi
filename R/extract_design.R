#' Extract design parameters for Monte Carlo calibration
#'
#' Internal helper used to estimate sample size, mean pre-post change,
#' and second-rater additive discrepancy from standardised user-supplied
#' data for Monte Carlo calibration.
#'
#' @param data A data frame returned by `prepare_data`, with columns
#'   `id`, `time`, `rater`, and `y`.
#'
#' @return A named list containing:
#' \describe{
#'   \item{N}{Number of unique subjects.}
#'   \item{delta}{Estimated mean pre-post change, averaged across raters.}
#'   \item{bias_r2}{Estimated mean additive discrepancy of the second rater
#'   relative to the reference rater, averaged across subjects and time points.}
#'   \item{time_levels}{The two ordered time levels used in the data.}
#'   \item{rater_levels}{The two ordered rater levels used in the data.}
#' }
#'
#' @keywords internal
extract_design <- function(data) {

  required_cols <- c("id", "time", "rater", "y")

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (!all(required_cols %in% names(data))) {
    stop(
      "`data` must contain the columns: ",
      paste(required_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.factor(data$id)) {
    data$id <- factor(data$id)
  }
  data$id <- droplevels(data$id)

  if (!is.factor(data$time)) {
    stop("`time` must be a factor with exactly two levels.", call. = FALSE)
  }
  data$time <- droplevels(data$time)
  if (nlevels(data$time) != 2) {
    stop("`time` must be a factor with exactly two levels.", call. = FALSE)
  }

  if (!is.factor(data$rater)) {
    stop("`rater` must be a factor with exactly two levels.", call. = FALSE)
  }
  data$rater <- droplevels(data$rater)
  if (nlevels(data$rater) != 2) {
    stop("`rater` must be a factor with exactly two levels.", call. = FALSE)
  }

  if (!is.numeric(data$y)) {
    stop("`y` must be numeric.", call. = FALSE)
  }

  time_levels <- levels(data$time)
  rater_levels <- levels(data$rater)

  baseline <- time_levels[1]
  followup <- time_levels[2]
  ref_rater <- rater_levels[1]
  second_rater <- rater_levels[2]

  N <- nlevels(data$id)

  wide_time <- stats::reshape(
    data,
    idvar = c("id", "rater"),
    timevar = "time",
    direction = "wide"
  )

  base_col <- make.names(paste0("y.", baseline))
  follow_col <- make.names(paste0("y.", followup))

  if (!all(c(base_col, follow_col) %in% names(wide_time))) {
    stop(
      "Failed to reconstruct pre-post outcome columns after reshaping. Check time levels and naming consistency.",
      call. = FALSE
    )
  }

  y_base <- wide_time[[base_col]]
  y_follow <- wide_time[[follow_col]]

  if (anyNA(y_base) || anyNA(y_follow)) {
    stop(
      "Incomplete pre-post data detected after reshaping by subject and rater.",
      call. = FALSE
    )
  }

  change_scores <- y_follow - y_base
  delta <- mean(change_scores)

  wide_rater <- stats::reshape(
    data,
    idvar = c("id", "time"),
    timevar = "rater",
    direction = "wide"
  )

  ref_col <- make.names(paste0("y.", ref_rater))
  r2_col <- make.names(paste0("y.", second_rater))

  if (!all(c(ref_col, r2_col) %in% names(wide_rater))) {
    stop(
      "Failed to reconstruct rater-specific outcome columns after reshaping. Check rater levels and naming consistency.",
      call. = FALSE
    )
  }

  y_ref <- wide_rater[[ref_col]]
  y_r2 <- wide_rater[[r2_col]]

  if (anyNA(y_ref) || anyNA(y_r2)) {
    stop(
      "Incomplete rater data detected after reshaping by subject and time.",
      call. = FALSE
    )
  }

  rater_discrepancy <- y_r2 - y_ref
  bias_r2 <- mean(rater_discrepancy)

  list(
    N = N,
    delta = as.numeric(delta),
    bias_r2 = as.numeric(bias_r2),
    time_levels = time_levels,
    rater_levels = rater_levels
  )
}
