#' Interpolate missing data between gaps
#'
#' \code{interpolate_gaps} returns a vector with the (originally) missing data
#' points interpolated according to the specified method.
#'
#' @param pupil A numeric vector of pupil size measurements.
#' @param type The type of interpolation to be used. Possible values are
#' 'linear' (default) or 'spline'. Note that the spline interpolation does not
#' yet function as intended.
#'
#' @details Returns a vector equal in length to \code{pupil}, with all missing
#' data points interpolated according to the specified method.
#'
#' Note that the default value for the rule argument in \code{approx()} is 2
#' rather than 1.
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
#'   pupil_left = c(3.11, 3.13, NA, NA, NA, NA, NA, 3.12),
#'   pupil_right = c(2.92, 2.95, NA, NA, NA, NA, NA, 2.95)
#' )
#' data
#'
#' # Linearly interpolate values of the left eye:
#' mutate(data,
#'   pupil_left = interpolate_gaps(pupil_left, type = "linear")
#' )
#'
#' # Example 2: Realistic data
#' blink
#'
#' # First remove outliers:
#' blink <- mutate(blink,
#'   pupil_left = dilation_speed_outliers_to_na(pupil_left, timestamp,
#'     constant = 10),
#'   pupil_right = dilation_speed_outliers_to_na(pupil_right, timestamp,
#'     constant = 10)
#' )
#'
#' # Then linearly interpolate gaps in pupil measurements of the left eye and
#' # perform a spline interpolation on the right eye:
#' blink <- mutate(blink,
#'     pupil_left = interpolate_gaps(pupil_left, type = "linear"),
#'     pupil_right = interpolate_gaps(pupil_right, type = "spline")
#'   )
#'
#' # Restructure the data and plot the results to compare the pupil measurements
#' # before the interpolation and after the interpolation:
#' blink %>%
#'   pivot_longer(
#'     cols = starts_with("pupil"),
#'     names_to = "pupil",
#'     names_pattern = "(left|right)",
#'     values_to = "pupil_size"
#'  ) %>%
#'  ggplot(aes(x = timestamp, y = pupil_size)) +
#'    geom_point() +
#'    facet_wrap(~ pupil)
#'
#' @export
interpolate_gaps <- function(pupil, type = "linear", rule = 2, method = "fmm") {

  #TODO: Check the spline interpolation and make it behave better, probably by
  # setting which points it should use to calculate the spline.

  # Check whether the "pupil" argument is numeric:
  if (!is.numeric(pupil)) {
    stop("'pupil' must be a vector of numeric values.")
  }

  # Check if there are missing values; if not, simply return the original
  # pupil measurements:
  if (!anyNA(pupil)) {
    return(pupil)
  }

  # Check if there are only missing values; if so, return the missing values and
  # display a warning:
  if (sum(is.na(pupil)) == length(pupil)) {
    warning("No pupil measurements found; returning only missing values.")
    return(pupil)
  }

  # Check if there is only one non-missing value; if so, return the original
  # pupil measurements and display a warning:
  if (sum(is.na(pupil)) == length(pupil) - 1) {
    warning("Only 1 non-missing value found; returning original values.")
    return(pupil)
  }

  # Check if a supported type of interpolation is requested:
  if (type != "linear" & type != "spline") {
    stop("'type' must be 'linear' or 'spline'.")
  }

  # Perform interpolation:
  n <- length(pupil)
  i <- 1:n
  NAs <- is.na(pupil)

  if (type == "linear") {
    res <- stats::approx(x = i[!NAs], y = pupil[!NAs], 1:n, rule = rule)$y
  }
  else if (type == "spline") {
    res <- stats::spline(x = i[!NAs], y = pupil[!NAs], n = n, method = method)$y
  }

  # Merge interpolated values back into the original vector:
  pupil[NAs] <- res[NAs]

  return(pupil)
}
