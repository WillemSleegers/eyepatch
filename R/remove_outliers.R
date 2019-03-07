#' Remove outliers
#'
#' \code{remove_outliers} removes pupil size outliers based on the median
#' absolute deviation (MAD).
#'
#' @param x A numeric vector containing the outlier data.
#' @param pupil An optional numeric vector of pupil size measurements. If
#' specified, the function will return a new vector of pupil sizes, but with
#' outliers removed.
#' @param constant An integer that raises or lowers the cutoff threshold.
#'
#' @details See Leys, Ley, Klein, Bernard, & Licata (2013) for details regarding
#' why the MAD should be used.
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' # Example 1: Outlier removal based on pupil dilation speed
#' blink <- mutate(blink,
#'     pupil_left_d = dilation_speed(pupil_left, timestamp),
#'     pupil_right_d = dilation_speed(pupil_right, timestamp),
#'     pupil_left_clean = remove_outliers(pupil_left_d, pupil_left),
#'     pupil_right_clean = remove_outliers(pupil_right_d, pupil_right)
#'   )
#'
#' ggplot(blink, aes(x = timestamp)) +
#'   geom_point(aes(y = pupil_left), color = "red") +
#'   geom_point(aes(y = pupil_left_clean))
#'
#' @export
remove_outliers <- function(x, pupil = NULL, constant = 10) {

  # Check whether x and pupil are numeric (if specified)
  if (!is.numeric(x)) {
    stop("x must be numeric.")
  }

  if (!is.null(pupil)) {
    if (!is.numeric(pupil)) {
      stop("pupil must be numeric.")
    }
  }

  # Determine cut-off threshold
  MAD <- median(abs(x - median(x, na.rm = TRUE)), na.rm = TRUE)
  threshold <- median(x, na.rm = TRUE) + constant * MAD

  # Remove outliers
  if (!is.null(pupil)) {
    output <- dplyr::if_else(x > threshold, NA_real_, pupil)
  } else {
    output <- dplyr::if_else(x > threshold, NA_real_, x)
  }

  return(output)
}
