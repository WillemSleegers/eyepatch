#' Sparsity filter
#'
#' \code{sparse_to_na} removes sparse samples in the data.
#'
#' @param pupil A numeric vector of pupil size measurements.
#' @param time A vector containing the timestamps associated with the
#' pupil size measurements.
#' @param gap_criterion A numeric value specifying the gap duration preceding
#' potential sparse observations.
#' @param cluster_criterion A numeric value specifying the section duration of
#' sparse value that should be removed.
#' @param grouping A vector containing grouping information
#' @param log A logical value. Should the action and results be logged?
#' @param log_file A character string specifying the path to the log file.
#'
#' @examples
#' library(dplyr)
#'
#' gaps <- mutate(gaps,
#'   pupil_left_clean = sparse_to_na(pupil_left, timestamp,
#'     first_criterion = 9, second_criterion = 11),
#'   pupil_right_clean = sparse_to_na(pupil_right, timestamp,
#'     first_criterion = 9, second_criterion = 11),
#'   )
#'
#' ggplot(gaps, aes(x = timestamp)) +
#'   geom_point(aes(y = pupil_left), color = "red") +
#'   geom_point(aes(y = pupil_right), color = "red") +
#'   geom_point(aes(y = pupil_left_clean)) +
#'   geom_point(aes(y = pupil_right_clean))
#'
#' @importFrom magrittr %>%
#'
#' @export
sparse_to_na <- function(pupil, time, gap_criterion = 40,
  cluster_criterion = 50, grouping = NULL, log = FALSE, log_file = NULL) {

  # Combine the timestamps and the pupil measurements into a data frame
  df <- tibble(
    time = time,
    pupil = pupil
  )

  # Check whether grouping information has been provided
  if (!is.null(grouping)) {

    if (is.list(grouping)) {

      df <- bind_cols(df, bind_cols(grouping))
      df <- group_by_at(df, vars(-pupil, -time))

    } else {
      if (length(grouping) != nrow(df)) {
        stop("Grouping length is not equal to the length of pupil observations")
      } else {
        df <- bind_cols(df, tibble(group = grouping))
        df <- group_by(df, group)
      }
    }
  }

  # Determine sections
  df <- df %>%
    mutate(
      section = ifelse(
        (is.na(pupil) & !is.na(lag(pupil))) |
          (!is.na(pupil) & is.na(lag(pupil))), 1, 0),
      section = cumsum(section)
    ) %>%
    group_by(section, add = TRUE) %>%
    mutate(
      section_start = ifelse(!is.na(section), min(time), NA),
      section_end = ifelse(!is.na(section), max(time), NA),
      section_duration = section_end - section_start
    ) %>%
    group_by_at(vars(-time, -pupil, -section, -section_start, -section_end,
      -section_duration)) %>%
    mutate(
      previous_section_duration = ifelse(section != lag(section),
        lag(section_duration), NA),
      missing_section = ifelse(is.na(pupil), 1, 0)
    ) %>%
    group_by(section, add = TRUE) %>%
    mutate(previous_section_duration = max(previous_section_duration,
      na.rm = TRUE))

  # Filter out sparse sections
  df <- df %>%
    mutate(
      pupil = ifelse(missing_section == 0 &
          previous_section_duration > gap_criterion &
          section_duration < cluster_criterion, NA_real_, pupil)
    )

  output <- pull(df, pupil)

  # Log
  if (log) {
    # Find the log file if it is not specified
    if (is.null(log_file)) {
      log_file <- find_log()
    }

    # Determine values to report
    n <- length(pupil[!is.na(pupil)])
    missing <- sum(is.na(output)) - sum(is.na(pupil))
    missing_pct <- round(missing / n * 100, 2)

    text <- paste0("Removed ", missing, " sparse observations (", missing_pct,
      "%) from '", deparse(substitute(pupil)), "' using a gap criterion of ",
      gap_criterion, " and a cluster criterion of ", cluster_criterion)
    write(text, file = log_file, append = TRUE)
  }

  return(output)
}

