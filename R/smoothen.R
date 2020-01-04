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
smoothen <- function(pupil, time = NULL, grouping = NULL, method = "low pass",
  sampling_rate, frequency = 10, padding = 50, span = 0.2, log = FALSE,
  log_file = NULL) {

  # Combine the timestamps and the pupil measurements into a data frame
  if (is.null(time)) {
    df <- tibble::tibble(pupil = pupil)
  } else {
    df <- tibble::tibble(
      time = time,
      pupil = pupil
    )
  }

  # Check whether grouping information has been provided
  if (!is.null(grouping)) {
    df <- apply_grouping(df, grouping)
  }

  # Check which method of smoothing is requested, and apply the respective
  # technique
  if (method == "loess") {
    df <- dplyr::mutate(df, pupil = fit_loess(x = time, y = pupil, span = span))
  } else if (method == "low pass") {
    df <- dplyr::mutate(df, pupil = low_pass_filter(pupil = pupil,
      frequency = frequency, sampling_rate = sampling_rate, padding = padding))
  }

  output <- dplyr::pull(df, pupil)

  # Log
  if (log) {
    # Find the log file if it is not specified
    if (is.null(log_file)) {
      log_file <- find_log()
    }

    # Log the smooth step
    if (method == "low pass") {
      text <- paste0("Applied a low pass filter with a cut-off frequency of ",
        frequency, "Hz, at a sampling rate of ", sampling_rate, "Hz, to '",
        deparse(substitute(pupil)), "'")
    } else if (method == "loess") {
      text <- paste0("Applied a loess regression filter with a span of ", span)
    } else {
      text <- "Performed a smoothing filter"
    }

    write(text, file = log_file, append = TRUE)
  }

  return(output)
}



