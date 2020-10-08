#' Complete time
#'
#' \code{complete_time} adds missing timestamps to a data frame.
#'
#' @param data The data frame containing the timestamps.
#' @param timestamp The timestamp column in the data frame.
#' @param difference The supposed difference between two timestamps.
#'
#' @details Completes a timestamp column by turning implicit missing values into
#' explicit missing values, followed by a linear interpolation between missing
#' values.
#'
#' The data frame can be grouped using dplyr's \code{group_by}.
#'
#' @examples
#' library(dplyr)
#'
#' # Example 1: Simple data
#' df <- tibble(
#'   timestamp = 1:30,
#'   trial = rep(1:3, each = 10),
#'   event = rep(c(rep("baseline", 3), rep("event", 7)), times = 3)
#' )
#'
#' # Remove some random observations, creating implicit missing values
#' set.seed(2020)
#' df <- slice_sample(df, n = 20) %>%
#'   arrange(trial, timestamp)
#'
#' # Add missing rows
#' df <- complete_time(df, timestamp, difference = 1)
#'
#' # Example 2: Realistic data
#' missing_complete <- complete_time(missing, timestamp, difference = 1000/60)
#'
#' @export
complete_time <- function(data, time, difference) {

  # Check whether data is indeed a data frame
  if (!"data.frame" %in% class(data)) {
    stop("data is not a data frame.")
  }

  # Check whether time is a column in data
  if (!rlang::as_name(enquo(time)) %in% names(data)) {
    stop(paste0("'", rlang::as_name(enquo(time)), "' is not a column in '",
      rlang::as_name(enquo(data)), "'"))
  }

  # Check whether time is numeric
  if (!is.numeric(dplyr::pull(data, {{ time }}))) {
    stop("'time' should be numeric")
  }

  # Check whether sampling_rate is numeric
  if (!is.numeric(difference)) {
    stop("'difference' should be numeric")
  }

  output <- data %>%
    dplyr::mutate(.i = round(({{ time }} - dplyr::lag({{ time }})) /
        difference)) %>%
    tidyr::replace_na(list(.i = 1)) %>%
    dplyr::mutate(.i = cumsum(.i)) %>%
    tidyr::complete(.i = 1:max(.i)) %>%
    dplyr::mutate({{ time }} := stats::approx({{ time }}, n = n())$y) %>%
    dplyr::select(-.i)

  return(output)
}
