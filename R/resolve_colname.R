#' Resolve a column name supplied as symbol or character string
#'
#' Internal helper for functions that accept either unquoted column names
#' or character strings.
#'
#' @param expr A captured expression representing a column name.
#' @param env The environment in which the expression should be resolved.
#'
#' @return A character string of length 1.
#'
#' @keywords internal
resolve_colname <- function(expr, env = parent.frame()) {

  if (is.character(expr)) {
    return(as.character(expr))
  }

  if (is.symbol(expr)) {
    nm <- as.character(expr)

    if (exists(nm, envir = env, inherits = TRUE)) {
      val <- get(nm, envir = env, inherits = TRUE)

      if (is.character(val) && length(val) == 1L) {
        return(val)
      }
    }

    return(nm)
  }

  deparse(expr)
}
