#' Calculate dilation speed
#'
#' \code{dilation_speed} calculates the dilation speed based on timestamps and
#' pupil size measurements.
#'
#' @param pupil A numeric vector of pupil size measurements.
#' @param time A vector containing the timestamps associated with the
#' pupil size measurements.
#'
#' @details The exact method of calculating the dilation speed is taken from
#' Kret & Sjak-Shie (2018).
#'
#' @examples
#' library(dplyr)
#'
#' blink <- mutate(blink,
#'     pupil_left_d = dilation_speed(pupil_left, timestamp),
#'     pupil_right_d = dilation_speed(pupil_right, timestamp)
#'   )
#'
#' @importFrom magrittr %>%
#'
#' @export
dilation_speed <- function(pupil, time) {

  # Check whether pupil and time are numeric
  if (!is.numeric(pupil)) {
    stop("pupil must be numeric.")
  }

  if (!is.numeric(time)) {
    stop("time must be numeric.")
  }

  # Check whether the vectors for pupil and time are equally long
  if (length(pupil) != length(time)) {
    stop("The length of pupil and time do not match up.")
  }

  # Combine the time and the pupil measurements into a data frame
  df <- tibble::tibble(
    time = time,
    pupil = pupil
  )

  # Determine cut-off threshold
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

  return(dplyr::pull(df, d))
}
