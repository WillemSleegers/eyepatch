#' Smoothen the pupil dilate
#'
#' \code{smoothen} returns a vector of smoothened pupil data according to the
#' specified method.
#'
#' @param pupil A numeric vector containing pupil data.
#' @param method The type of interpolation to be used. Possible values are
#' 'loess' (default) or 'lowpass'.
#' @param grouping A vector containing grouping information
#' @param log A logical value. Should the action and results be logged?
#' @param log_file A character string specifying the path to the log file.
#'
#' @examples
#'
#' @importFrom signal butter
#' @importFrom signal filtfilt
#'
#' @export
smoothen <- function(pupil, time = NULL, method = "low pass",
  sampling_frequency = NULL, cutoff_frequency = 10, padding = 50, span = 0.2,
  log = FALSE, log_file = NULL) {

  # Check which method of smoothing is requested, and apply the respective
  # technique
  if (method == "low pass") {
    output <- low_pass_filter(pupil = pupil, cutoff_frequency = cutoff_frequency,
      sampling_frequency = sampling_frequency, padding = padding)
  } else if (method == "loess") {
    output <- fit_loess(x = time, y = pupil, span = span)
  }

  # Log
  if (log) {
    # Find the log file if it is not specified
    if (is.null(log_file)) {
      log_file <- find_log()
    }

    # Log the smooth step
    if (method == "low pass") {
      text <- paste0("Applied a low pass filter with a cutoff frequency of ",
        cutoff_frequency, "Hz, at a sampling rate of ", sampling_frequency,
        "Hz, to '", deparse(substitute(pupil)), "'")
    } else if (method == "loess") {
      text <- paste0("Applied a loess regression filter with a span of ", span)
    } else {
      text <- "Performed a smoothing filter"
    }

    write(text, file = log_file, append = TRUE)
  }

  return(output)
}



