#' Set dilation speed outliers to missing
#'
#' \code{dilation_speed_outliers_to_na} calculates the dilation speed based on
#' timestamps and pupil size measurements and then sets outliers to missing
#' (NA).
#'
#' @param pupil A numeric vector of pupil size measurements.
#' @param time A vector containing the timestamps associated with the
#' pupil size measurements.
#' @param constant A numeric value specifying the threshold for outlier removal.
#' The default is 10.
#'
#' @details The exact method of calculating the dilation speed is taken from
#' Kret & Sjak-Shie (2018).
#'
#' @examples
#' # Load the "dplyr", "tidyr", and "ggplot2" packages:
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#'
#' # Example 1: Artificial data
#' # Create some artificial data:
#' data <- tibble(
#'   time = 0:5,
#'   pupil_left = c(3.11, 3.13, 3.17, 3.7, 3.16, 3.12),
#'   pupil_right = c(2.92, 2.95, 2.99, 3.5, 2.97, 2.95)
#' )
#' data
#'
#' # Remove outliers:
#' mutate(data,
#'   pupil_left = dilation_speed_outliers_to_na(pupil_left, time,
#'     constant = 10),
#'   pupil_right = dilation_speed_outliers_to_na(pupil_right, time,
#'     constant = 10)
#' )
#'
#' Example 2: Realistic data
#' blink
#'
#' # Remove outliers:
#' blink <- mutate(blink,
#'   pupil_left_new = dilation_speed_outliers_to_na(pupil_left, timestamp,
#'     constant = 10),
#'   pupil_right_new = dilation_speed_outliers_to_na(pupil_right, timestamp,
#'     constant = 10)
#' )
#'
#' # Restructure the data and plot the results to compare the pupil measurements
#' # before the outlier removal and after the outlier removal:
#' blink %>%
#'   rename(
#'     pupil_left_old = pupil_left,
#'     pupil_right_old = pupil_right
#'   ) %>%
#'   pivot_longer(
#'     cols = starts_with("pupil"),
#'     names_to = c("pupil", "status"),
#'     names_pattern = "(left|right)_(old|new)",
#'     values_to = "pupil_size"
#'  ) %>%
#'  ggplot(aes(x = timestamp, y = pupil_size, color = status)) +
#'    geom_point() +
#'    facet_wrap(~ pupil)
#'
#' @export
dilation_speed_outliers_to_na <- function(pupil, time, constant = 10) {

  # Check whether the "pupil" argument is numeric:
  if (!is.numeric(pupil)) {
    stop("'pupil' must be a vector of numeric values.")
  }

  # Check whether the "time" argument is numeric:
  if (!is.numeric(time)) {
    stop("'time' must be a vector of numeric values.")
  }

  # Check whether "pupil" and "time" have the same number of values:
  if (length(pupil) != length(time)) {
    stop("'pupil' and 'time' must have the same number of values.")
  }

  # Check whether the "constant" argument is numeric:
  if (!is.numeric(constant)) {
    stop("'constant' must be a numeric value.")
  }

  # Determine dilation speeds:
  d1 = abs((pupil - dplyr::lag(pupil)) / (time - dplyr::lag(time)))
  d2 = abs((dplyr::lead(pupil) - pupil) / (dplyr::lead(time) - time))
  d = dplyr::case_when(
    is.na(d1) ~ d2,
    is.na(d2) ~ d1,
    d1 > d2 ~ d2,
    TRUE ~ d1
  )

  # Determine the cut-off threshold:
  MAD <- median(abs(d - median(d, na.rm = TRUE)), na.rm = TRUE)
  threshold <- median(d, na.rm = TRUE) + constant * MAD

  # Set values exceeding the threshold to NA:
  output <- dplyr::if_else(d > threshold, NA_real_, pupil)

  return(output)
}
