#' Log removal of observations
#'
#' \code{log_removal} updates the pupil preprocessing log with the number of
#' observations that were removed.
#'
#' @param data A data frame.
#' @param left_eye A numeric column in 'data' containing the left eye
#' observations.
#' @param right_eye The name of the column in 'data' containing the right eye
#' observations
#' @param left_eye_old An optional column specifying the reference column for
#' the left eye
#' @param right_eye_old An optional column specifying the reference column for
#' the right eye
#' @param log_file A character string specifying the path to the log file.
#'
#' @examples
#'
#' @export
log_removal <- function(data, left_eye = NULL, right_eye = NULL,
  left_eye_old = NULL, right_eye_old = NULL, log_file = NULL, message = NULL) {

  # Find the log file
  if (is.null(log_file)) {
    log_file <- find_log()
  } else {
    if (!file.exists(log_file)) {
      stop("Log not found.")
    }
  }

  # Left eye
  if (!missing(left_eye)) {
    left_eye <- enquo(left_eye)

    left_eye_values <- pull(data, !!left_eye)

    n <- length(left_eye_values)
    missing_n <- sum(is.na(left_eye_values))

    if (missing(left_eye_old)) {
      missing_pct <- round(missing_n/n*100, digits = 2)
    } else {
      left_eye_old <- enquo(left_eye_old)
      left_eye_old_values <- pull(data, !!left_eye_old)

      missing_n_old <- sum(is.na(left_eye_old_values))
      missing_n <- missing_n - missing_n_old
      missing_pct <- round(missing_n/n*100, digits = 2)
    }

    text <- paste0("Removed ", missing_n, " observations (", missing_pct,
      "%) from ", quo_name(left_eye))
    write(text, file = log_file, append = TRUE)
  }

  # Right eye
  if (!missing(right_eye)) {
    right_eye <- enquo(right_eye)

    right_eye_values <- pull(data, !!right_eye)

    n <- length(right_eye_values)
    missing_n <- sum(is.na(right_eye_values))

    if (missing(right_eye_old)) {
      missing_pct <- round(missing_n/n*100, digits = 2)
    } else {
      right_eye_old <- enquo(right_eye_old)
      right_eye_old_values <- pull(data, !!right_eye_old)

      missing_n_old <- sum(is.na(right_eye_old_values))
      missing_n <- missing_n - missing_n_old
      missing_pct <- round(missing_n/n*100, digits = 2)
    }

    text <- paste0("Removed ", missing_n, " observations (", missing_pct,
      "%) from ", quo_name(right_eye))
    write(text, file = log_file, append = TRUE)
  }
}

