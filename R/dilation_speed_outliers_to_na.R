#' Set dilation speed outliers to missing.
#'
#' \code{dilation_speed_outliers_to_na} calculates the dilation speed based on
#' timestamps and pupil size measurements and then sets outliers to missing
#' (NA).
#'
#' @param pupil A numeric vector of pupil size measurements.
#' @param time A vector containing the timestamps associated with the
#' pupil size measurements.

#' @param constant A numeric value specifying the threshold for outlier removal.
#' @param log A logical value. Should the action and results be logged?
#' @param log_file A character string specifying the path to the log file.
#'
#' @details The exact method of calculating the dilation speed is taken from
#' Kret & Sjak-Shie (2018).
#'
#' @examples
#' library(dplyr)
#'
#' blink <- mutate(blink,
#'     pupil_left = dilation_speed_outliers_to_na(pupil_left, timestamp),
#'     pupil_right = dilation_speed_outliers_to_na(pupil_right, timestamp)
#'   )
#'
#' @importFrom magrittr %>%
#'
#' @export
#'

#TODO: Check if dilation speed outliers remove observations surrounding gaps

dilation_speed_outliers_to_na <- function(pupil, time, constant = 10,
  log = FALSE) {

  # Combine the time and the pupil measurements into a data frame
  df <- tibble::tibble(
    time = time,
    pupil = pupil
  )

  # Fill missing pupil values with last known value
  df <- tidyr::fill(df, pupil)

  # Calculate dilation speeds
  df <- dplyr::mutate(df,
    d1 = abs((pupil - dplyr::lag(pupil)) / (time - dplyr::lag(time))),
    d2 = abs((dplyr::lead(pupil) - pupil) / (dplyr::lead(time) - time)),
    d = dplyr::case_when(
      is.na(d1) ~ d2,
      is.na(d2) ~ d1,
      d1 > d2 ~ d1,
      TRUE ~ d2
    )
  )

  x <- dplyr::pull(df, d)

  # Determine cut-off threshold
  MAD <- median(abs(x - median(x, na.rm = TRUE)), na.rm = TRUE)
  threshold <- median(x, na.rm = TRUE) + constant * MAD

  output <- dplyr::if_else(x > threshold, NA_real_, pupil)

  # Log
  if (log) {
    # Determine values to report
    n <- length(pupil[!is.na(pupil)])
    missing <- sum(is.na(output)) - sum(is.na(pupil))
    missing_pct <- round(missing / n * 100, 2)

    text <- paste0("Removed ", missing, " outliers (", missing_pct,
      "%) from '", deparse(substitute(pupil)), "' based on dilation speed",
      " (constant = ", constant, ")")
  }

  return(output)
}

