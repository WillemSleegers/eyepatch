#' Centers time around a specified moment
#'
#' \code{center_time} centers time around a specified moment, such as the start
#' of a trial or when a particular event occurs.
#'
#' @param time A vector containing the timestamps.
#' @param condition A condition specifying the onset of the to-be-centered around moment.
#'
#' @details To center time in multiple trials or in multiple participants, use
#' dplyr's \code{group_by}. See the example below.
#'
#' @examples
#'
#' # Example 1: Simple data set
#' data <- dplyr::tibble(
#'   timestamp = 1:30,
#'   trial = rep(1:3, each = 10),
#'   event = rep(c(rep("baseline", 3), rep("event", 7)), times = 3)
#' )
#'
#' # Create a new time column with time centered around the first timestamp
#' data <- dplyr::mutate(data, time = center_time(timestamp))
#'
#' # More interestingly, center time at the first timestamp of each trial
#' data <- data %>%
#'   dplyr::group_by(trial) %>%
#'   dplyr::mutate(time = center_time(timestamp))
#'
#' # More more interestingly, center time at the start of the event within each
#' trial
#' data <- data %>%
#'   dplyr::group_by(trial) %>%
#'   dplyr::mutate(time = center_time(timestamp, event == "event"))
#'
#' # Example 2: Realistic data
#'
#'
#' @export

center_time <- function(time, condition = NULL) {

  # Check whether time is numeric
  if (!is.numeric(time)) {
    stop("'time' should be numeric")
  }

  # Check whether the condition consists of only logical values
  if (!is.null(condition)) {
    if (!is.logical(condition)) {
      stop("'condition' should be a vector of logical values")
    }
  }

  # Center the time vector
  if (length(condition) > 0) {
    time_valid <- time[condition]
    time_centered <- time - min(time_valid, na.rm = TRUE)
  } else {
    time_centered <- time - min(time)
  }

  return(time_centered)
}
