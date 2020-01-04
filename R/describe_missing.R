#' Describe missing data statistics
#'
#' \code{describe_missing} returns a data frame with summary statistics
#' describing the missing data of both or either pupil
#'
#' @param data A data frame.
#' @param ... A variable number of numeric columns to calculate the missing
#' descriptives of.
#'
#' @details The data set can be grouped using \strong{dplyr}'s \code{group_by}
#' so that the missing descriptives will be calculated for each group level.
#'
#' @importFrom magrittr %>%
#'
#' @export
describe_missing <- function(data, ...) {

  vars <- enquos(...)

  missing <- data %>%
    gather("var", "value", !!!vars) %>%
    group_by(var, add = TRUE) %>%
    summarize(
      n = n(),
      missing = sum(is.na(value)),
      pct = missing / n * 100
    ) %>%
    select(var, everything()) %>%
    arrange(var)

  return(missing)
}
