#' Pad gaps
#'
#' \code{pad_gaps} removes pupil size measurements that border gaps in the data.
#'
#' @param pupil A numeric vector of pupil size measurements.
#' @param time A vector containing the timestamps associated with the
#' pupil size measurements.
#' @param gap_minimum A numeric value describing the minimum gap duration.
#' @param padding A numeric value describing by how much gaps should be padded.
#' @param padding_before A numeric value describing by how much gaps should be
#' padded before a gap.
#' @param padding_after A numeric value describing by how much gaps should be
#' padded after a gap.
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
#'   time = 1:8,
#'   pupil_left = c(3.11, 3.13, 3.16, NA, NA, NA, 3.16, 3.12),
#'   pupil_right = c(2.92, 2.95, 2.98, NA, NA, NA, 2.97, 2.95)
#' )
#' data
#'
#' # Pad gaps of the left eye:
#' mutate(data,
#'   pupil_left = pad_gaps(pupil_left, time, gap_minimum = 1, padding = 1)
#' )
#'
#' # Example 2: Realistic data
#' gaps
#'
#' # Pad gaps of the left eye:
#' gaps <- mutate(gaps,
#'   pupil_left_new = pad_gaps(pupil_left, timestamp, gap_minimum = 10,
#'     padding = 4)
#'   )
#'
#' # Restructure the data and plot the results to compare the pupil measurements
#' # before and after padding the gaps:
#' gaps %>%
#'   rename(pupil_left_old = pupil_left) %>%
#'   pivot_longer(
#'     cols = c(pupil_left_old, pupil_left_new),
#'     names_to = "status",
#'     names_pattern = "(old|new)",
#'     values_to = "pupil_size"
#'  ) %>%
#'  ggplot(aes(x = timestamp, y = pupil_size, color = status)) +
#'    geom_point()
#'
#' @importFrom magrittr %>%
#'
#' @export
pad_gaps <- function(pupil, time, gap_minimum = 75, padding = 50,
  padding_before = NULL, padding_after = NULL) {

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

  # Check whether the "gap_minimum" argument is numeric:
  if (!is.numeric(gap_minimum)) {
    stop("'gap_minimum' must be a numeric value.")
  }

  # Check whether the "gap_minimum" argument is larger than 0:
  if (gap_minimum <= 0) {
    stop("'gap_minimum' must be larger than 0.")
  }

  # Check whether the padding arguments are numeric and larger than 0:
  if (!is.numeric(padding)) {
    stop("'padding' must be a numeric value.")
  }
  if (padding <= 0) {
    stop("'padding' must be larger than 0.")
  }
  if (!is.null(padding_before)) {
    if (!is.numeric(padding_before)) {
      stop("'padding_before' must be a numeric value.")
    }
    if (padding_before <= 0) {
      stop("'padding_before' must be larger than 0.")
    }
  }
  if (!is.null(padding_after)) {
    if (!is.numeric(padding_after)) {
      stop("'padding_after' must be a numeric value.")
    }
    if (padding_after <= 0) {
      stop("'padding_after' must be larger than 0.")
    }
  }

  # Determine gaps:
  gaps <- is.na(pupil)

  # Determine padding:
  if (is.null(padding_before)) {
     padding_before <- padding
  }
  if (is.null(padding_after)) {
     padding_after <- padding
  }

  # Loop over the gaps and pad where necessary:
  gap <- FALSE
  gap_duration <- 0

  for (i in 1:length(gaps)) {
    if (gaps[i]) {
      if (gap == FALSE) {
        gap_start <- time[i]
      }
      gap <- TRUE
    } else {
      if (gap == TRUE) {
        gap_end <- time[i]
        gap_duration <- gap_end - gap_start

        if (gap_duration > gap_minimum) {
          pupil[time >= (gap_start - padding_before) & time < gap_start] <- NA
          pupil[time > gap_end & time <= (gap_end + padding_after)] <- NA
        }
      }
      gap <- FALSE
    }
  }

  return(pupil)
}
