#' Pad gaps
#'
#' \code{pad_gaps} removes samples that border certain gaps in the data.
#'
#' @param pupil A numeric vector of pupil size measurements.
#' @param time A vector containing the timestamps associated with the
#' pupil size measurements.
#' @param gap_minimum A numeric value describing the minimum gap duration
#' @param padding_before A numeric value describing by how much gaps should be
#' padded before a gap
#' @param padding_after
#' @param grouping A vector containing grouping information
#' @param log A logical value. Should the action and results be logged?
#' @param log_file A character string specifying the path to the log file.
#'
#' @examples
#' library(dplyr)
#'
#' gaps <- mutate(gaps,
#'   pupil_left_padded = pad_gaps(pupil_left, timestamp, gap_minimum = 10,
#'     padding_before = 4, padding_after = 4),
#'   pupil_right_padded = pad_gaps(pupil_right, timestamp, gap_minimum = 10,
#'     padding_before = 4, padding_after = 4)
#'   )
#'
#' ggplot(gaps, aes(x = timestamp)) +
#'   geom_point(aes(y = pupil_left), color = "red") +
#'   geom_point(aes(y = pupil_left_padded))
#'
#' @importFrom magrittr %>%
#'
#' @export
pad_gaps <- function(pupil, time, gap_minimum = 75, padding_before = 50,
  padding_after = 50, log = FALSE, log_file = NULL) {
  #TODO: Add a single padding argument, rather than both a before and after

  # Check arguments
  if (length(time) == 0) {
    stop("No time data provided.")
  }

  if (length(pupil) == 0) {
    stop("No pupil data provided.")
  }

  if (is.null(gap_minimum)) {
    stop("No minimum gap provided.")
  } else {
    if (gap_minimum <= 0) {
      stop("Minimum gap is too small or negative.")
    }
  }

  if (padding_before <= 0 | padding_after <= 0) {
    stop("Padding has to be greater than zero.")
  }

  # Combine the timestamps and the pupil measurements into a data frame
  #TODO: Maybe don't do this with data frames, because it's pretty slow
  df <- tibble(
    time = time,
    pupil = pupil
  )

  # Determine gap information
  df <- df %>%
    mutate(
      gap = ifelse(is.na(pupil), 1, 0),
      gap = ifelse(gap == 1 & lag(gap, default = 0) == 0, 1, 0),
      gap = ifelse(is.na(pupil), cumsum(gap), NA)
    ) %>%
    group_by(gap) %>%
    mutate(
      gap_start = ifelse(!is.na(gap), min(time), NA),
      gap_end = ifelse(!is.na(gap), max(time), NA),
      gap_duration = gap_end - gap_start
    ) %>%
    ungroup()

  # Filter out gaps that are too small
  df <- df %>%
    mutate(
      gap = ifelse(gap_duration < gap_minimum, NA, gap),
      gap_start = ifelse(gap_duration < gap_minimum, NA, gap_start),
      gap_end = ifelse(gap_duration < gap_minimum, NA, gap_end)
    )

  # Check if there are any gaps remaining
  if (length(unique(pull(df, gap))) == 1) {
    return(pull(df, pupil))
    #TODO: Log that no gaps were found
  }

  # Pad gaps before the gap
  if (padding_before) {
    suppressWarnings(
      df <- df %>%
        mutate(
          gap_before = ifelse(is.na(gap) & !is.na(dplyr::lag(gap)), 1, 0),
          gap_before = cumsum(gap_before)
        ) %>%
        group_by(gap_before) %>%
        mutate(
          time_before = min(gap_start, na.rm = TRUE),
          pupil = ifelse(time >= time_before - padding_before,
            NA, pupil)
        ) %>%
        ungroup()
    )
  }

  # Pad gaps after the gap
  if (padding_after) {
    suppressWarnings(
      df <- df %>%
        mutate(
          gap_after = ifelse(!is.na(gap) & is.na(lag(gap)), 1, 0),
          gap_after = cumsum(gap_after)
        ) %>%
        group_by(gap_after) %>%
        mutate(
          time_after = max(gap_end, na.rm = TRUE),
          pupil = ifelse(time <= time_after + padding_before,
            NA, pupil)
        ) %>%
        ungroup()
    )
  }

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

    text <- paste0("Removed ", missing, " observations (", missing_pct,
      "%) from '", deparse(substitute(pupil)), "' by padding gaps of ",
      gap_minimum, " or longer by ", padding_before, " before gaps and ",
      padding_after, " after gaps")
    write(text, file = log_file, append = TRUE)
  }

  return(output)
}
