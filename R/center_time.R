#' Centers time around a specified moment
#'
#' \code{center_time} centers time around a specified moment, such as the start
#' of a trial or when a particular event occurs.
#'
#' @param time A vector containing the timestamps associated with the
#' pupil size measurements.
#' @param condition A condition specifying when the onset of the trial begins.
#'
#' @examples
#'
#' # Create example data
#' data <- tibble(
#'   timestamp = 1:30,
#'   trial = rep(1:3, each = 10),
#'   event = rep(c(rep("baseline", 3), rep("event", 7)), times = 3)
#'  )
#'
#'  # Create a new time column with the time centered at 0
#'  data <- mutate(data, time = center_time(timestamp))
#'
#'  # More interestingly, center time at 0, for each trial
#'  data <- data %>%
#'    group_by(trial) %>%
#'    mutate(time = center_time(timestamp))
#'
#'  # More more interestingly, center time at 0, for each trial, starting after
#'  # the baseline period of the trial
#'  data <- data %>%
#'    group_by(trial) %>%
#'    mutate(time = center_time(timestamp, event == "event"))
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
