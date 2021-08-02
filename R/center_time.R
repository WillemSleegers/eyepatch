#' Center time around a specified moment
#'
#' \code{center_time} centers time around a specified moment, such as the start
#' of a trial or when a particular event occurs.
#'
#' @param time A vector containing timestamps.
#' @param condition An (optional) condition specifying the onset of the moment
#' that the timestamps should be centered around.
#'
#' @details To center time in multiple trials and/or for multiple participants,
#' use dplyr's \code{group_by}. See below for some examples.
#'
#' @examples
#' # Load the "dplyr" package for access to its functions and the "%>%" pipe:
#' library(dplyr)
#'
#' # Example 1: Artificial data
#' # Create some data for this example:
#' data <- tibble(
#'   timestamp = 1:14,
#'   trial = rep(1:2, each = 7),
#'   event = rep(c(rep("baseline", 2), rep("event", 5)), times = 2)
#' )
#' data
#'
#' # Create a new "time" column with the timestamps centered around the first
#' # timestamp:
#' mutate(data, time = center_time(timestamp))
#'
#' # More interestingly, center time at the first timestamp of each trial:
#' data %>%
#'   group_by(trial) %>%
#'   mutate(time = center_time(timestamp))
#'
#' # More more interestingly, center time around the moment when an event begins
#' # within each trial:
#' data %>%
#'   group_by(trial) %>%
#'   mutate(time = center_time(timestamp, event == "event"))
#'
#' # Example 2: Realistic data
#' # Inspect the "trial1" data set:
#' trial1
#' count(trial1, event)
#' slice(trial1, 58:63)
#'
#' # Center time around the moment when the 'feedback' event takes place:
#' trial1 <- mutate(trial1, time = center_time(timestamp, event == "feedback"))
#'
#' # Inspect the rows where the event goes from 'baseline' to 'event' to
#' # verify that the function worked:
#' slice(trial1, 58:63)
#'
#' @export
center_time <- function(time, condition = NULL) {

  # Check whether the "time" argument is numeric:
  if (!is.numeric(time)) {
    stop("'time' should be a vector of numeric values.")
  }

  if (!is.null(condition)) {
    # Check whether the "condition" argument consists of only logical values:
    if (!is.logical(condition)) {
      stop("'condition' should be a vector of logical values.")
    }

    # Check whether "time" and "condition" have the same number of values:
    if (length(time) != length(condition)) {
      stop("'time' and 'condition' must have the same number of values.")
    }
  }

  # Center the "time" vector:
  if (length(condition) > 0) {
    time_valid <- time[condition]
    time_centered <- time - min(time_valid, na.rm = TRUE)
  } else {
    time_centered <- time - min(time)
  }

  return(time_centered)
}
