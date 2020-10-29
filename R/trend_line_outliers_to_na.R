#' Trend line outliers to missing.
#'
#' \code{trend_line_outliers_to_na} determines outliers based on deviations from
#' a smooth trend line and sets the outliers to missing (NA).
#'
#' @param pupil A numeric vector of pupil size measurements.
#' @param time A vector containing the timestamps associated with the
#' pupil size measurements.
#' @param span A numeric value specifying the amount of smoothing.
#' @param constant A numeric value specifying the threshold for outlier removal.
#' @param log A logical value. Should the action and results be logged?
#' @param log_file A character string specifying the path to the log file.
#'
#' @examples
#' library(dplyr)
#'
#' blink <- mutate(blink,
#'     pupil_left = trend_line_outliers_to_na(pupil_left, timestamp),
#'     pupil_right = trend_line_outliers_to_na(pupil_right, timestamp)
#'   )
#'
#' @export
trend_line_outliers_to_na <- function(pupil, time = NULL, constant = 10,
    method = "low pass", sampling_frequency = 60, cutoff_frequency = 10,
    padding = 50, interpolation = "linear", log = FALSE,
    log_file = NULL
  ) {

  # Smoothen the pupil data
  if (method == "low pass") {
    # Check if there are missing values
    if (sum(is.na(pupil)) > 0) {
      pupil_smooth <- interpolate_gaps(pupil, type = interpolation)

      # Check if that worked
      if (sum(is.na(pupil_smooth)) > 0) {
        warning("Missing data could not be interpolated; no outliers were removed.")
        return(pupil)
      }
    } else {
      pupil_smooth <- pupil
    }

    pupil_smooth <- smoothen(pupil_smooth, method = "low pass",
      sampling_frequency = sampling_frequency,
      cutoff_frequency = cutoff_frequency, padding = padding)
  }

  # Calculate absolute deviations
  d <- abs(pupil - pupil_smooth)

  # Determine cut-off threshold
  MAD <- median(abs(d - median(d, na.rm = TRUE)), na.rm = TRUE)
  threshold <- median(d, na.rm = TRUE) + constant * MAD

  output <- ifelse(d > threshold, NA, pupil)

  # Log
  if (log) {
    # Find the log file if it is not specified
    if (is.null(log_file)) {
      log_file <- find_log()
    }

    # Determine values to report
    n <- length(pupil[!is.na(pupil)])
    missing <- sum(is.na(output)) - sum(is.na(pupil))
    missing_pct <- round(missing / n * 100, 2)

    text <- paste0("Removed ", missing, " outliers (", missing_pct,
      "%) from '", deparse(substitute(pupil)), "' based on dilation speed",
      " (constant = ", constant, ")")
    write(text, file = log_file, append = TRUE)
  }

  return(output)
}
