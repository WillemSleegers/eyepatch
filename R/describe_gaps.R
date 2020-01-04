#' Calculate summary statistics of gaps in the data
#'
#' \code{describe_gaps} returns a data frame with summary statistics describing
#' the gaps in the data.
#'
#' @param data A data frame.
#' @param timestamp The timestamp column in the data frame.
#' @param ... One or more numeric columns in the data frame that you want to
#' calculate the gap descriptives of.
#'
#' @details Returns a data frame containing the start and end point, as well as
#' the duration of each gap.
#'
#' The data set can be grouped using \strong{dplyr}'s \code{group_by}
#' so that the gap descriptives will be calculated for each group level.
#'
#' @examples
#' library(dplyr)
#'
#' describe_gaps(gaps, pupil_left)
#' describe_gaps(gaps, pupil_left, pupil_right)
#' gaps %>%
#'   group_by(trial) %>%
#'   describe_gaps(pupil_left, pupil_right)
#'
#' @importFrom magrittr %>%
#'
#' @export
describe_gaps <- function(data, time, ...) {

  # Quote the time and provided columns names
  time <- dplyr::enquo(time)
  vars <- dplyr::enquos(...)

  output <- tibble::tibble()

  # Calculate gap descriptives
  for (var in vars) {
    gaps <- data %>%
      dplyr::mutate(
        gap := dplyr::if_else(is.na(!!var) & !is.na(lag(!!var)), 1, 0),
        gap := dplyr::if_else(is.na(!!var), cumsum(gap), NA_real_)
      ) %>%
      dplyr::group_by(gap, add = TRUE) %>%
      dplyr::summarize(
        start = min(!!time),
        end = max(!!time),
        duration = end - start
      ) %>%
      dplyr::filter(!is.na(gap)) %>%
      dplyr::mutate(var = quo_name(var)) %>%
      dplyr::select(var, everything())

    output <- dplyr::bind_rows(output, gaps)
  }

  # Ungroup the output data frame
  output <- dplyr::ungroup(output)

  return(output)
}
