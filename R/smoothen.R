#' Smoothen pupil measurements
#'
#' \code{smoothen} returns a vector of smoothened pupil measurements according
#' to the specified method.
#'
#' @param pupil A numeric vector of pupil size measurements.
#' @param time An optional vector containing the timestamps associated with the
#' pupil size measurements.
#' @param method The type of interpolation to be used. Possible values are
#' 'low pass' (default) or 'loess'.
#'
#' @examples
#'
#' @importFrom signal butter
#' @importFrom signal filtfilt
#'
#' @export
smoothen <- function(pupil, time = NULL, method = "low pass",
  sampling_frequency = NULL, cutoff_frequency = 10, padding = 50, span = 0.2) {

  # Check which method of smoothing is requested, and apply the respective
  # technique:
  if (method == "low pass") {

    # Check if there are missing data points:
    if (sum(is.na(pupil)) > 1) {
      warning("Missing data found; returning unsmoothened data.")
    }

    # Pad the pupil data with the first and last pupil observations to prevent
    # artifacts at the start and end caused by applying the low pass filter
    pupil <- c(rep(first(pupil), padding), pupil, rep(last(pupil), padding))

    # Determine critical frequencies as required by signal::filtfilt():
    W <- cutoff_frequency / (sampling_frequency / 2)

    # Generate a Butterworth filter:
    filt <- signal::butter(1, W, type = "low")

    # Apply low pass filter:
    pupil_smooth <- signal::filtfilt(filt, pupil)

    # Remove padding:
    pupil <- pupil_smooth[(padding + 1):(length(pupil_smooth) - padding)]
  } else if (method == "loess") {
    tryCatch(
      {
        # Perform a loess regression:
        model <- loess(pupil ~ time, span = span)

        # We can't simply return the fitted values because missing data is
        # automatically removed, so we return predicted values instead:
        pupil <- predict(model, tibble(x = x))
      },
      error = function(cond) {
        warning(cond)
        pupil <- NA
      },
      warning = function(cond) {
        warning(cond)
        pupil <- NA
      }
    )
  }

  return(pupil)
}



