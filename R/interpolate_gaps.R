#' Interpolate missing data between gaps
#'
#' \code{interpolate_gaps} returns a vector with the (originally) missing data
#' points interpolated according to the specified method.
#'
#' @param x A numeric vector.
#' @param type The type of interpolation to be used. Possible values are
#' 'linear' (default) or 'spline'.
#' @param log A logical value. Should the action and results be logged?
#' @param log_file A character string specifying the path to the log file.
#'
#' @details Returns a vector equal in length to \code{x}, with all missing data
#' points interpolated according to the specified method.
#'
#' See \code{?approx} and \code{?spline} for more available arguments.
#'
#' Note that the default value for the rule argument in \code{approx()} has
#' changed from 1 to 2.
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' # Linear interpolation
#' gaps <- mutate(gaps, pupil_left_LI = interpolate_gaps(pupil_left))
#'
#' ggplot(gaps, aes(x = timestamp)) +
#'  geom_point(aes(y = pupil_left_LI), color = "red") +
#'  geom_point(aes(y = pupil_left))
#'
#' # Spline interpolation
#' gaps <- mutate(gaps, pupil_left_spline = interpolate_gaps(pupil_left,
#'   type = "spline"))
#'
#' ggplot(gaps, aes(x = timestamp)) +
#'  geom_point(aes(y = pupil_left_spline), color = "red") +
#'  geom_point(aes(y = pupil_left))
#'
#' @export
interpolate_gaps <- function(x, type = "linear", rule = 2, log = FALSE,
  log_file = NULL, ...) {

  # Check whether x is numeric
  if(!is.numeric(x)) {
    stop("x is not numeric.")
  }

  # Check if there are missing values; if not, simply end the function
  if (!anyNA(x)) {
    return(x)
  }

  # Check if there are only missing values, if so, end the function
  if (sum(is.na(x)) == length(x)) {
    return(x)
  }

  # Check if there is only one non-missing value
  if (sum(is.na(x)) == length(x) - 1) {
    return(x)
  }

  # Check if a supported type of interpolation is requested
  if (type != "linear" & type != "spline") {
    stop("Method must be 'linear' or 'spline'.")
  }

  # Perform interpolation
  n <- length(x)
  i <- 1:n
  NAs <- is.na(x)

  if (type == "linear") {
    res <- stats::approx(x = i[!NAs], y = x[!NAs], 1:n, rule = rule, ...)$y
  }
  else if (type == "spline") {
    res <- stats::spline(x = i[!NAs], y = x[!NAs], n = n, ...)$y
  }

  # Merge interpolated values back into the original vector
  x[NAs] <- res[NAs]

  # Log
  if (log) {
    # Find the log file if it is not specified
    if (is.null(log_file)) {
      log_file <- find_log()
    }

    # Log the smooth step
    if (type == "linear") {
      text <- paste0("Applied linear interpolation")
    } else {
      text <- "Applied spline interpolation"
    }

    write(text, file = log_file, append = TRUE)
  }

  return(x)
}
