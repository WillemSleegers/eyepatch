#' Pad gaps
#'
#' \code{pad_gaps} removes samples that border certain gaps in the data.
#'
#' @param pupil A numeric vector of pupil size measurements.
#' @param time A vector containing the timestamps associated with the
#' pupil size measurements.
#'
#' @details The exact method of calculating the dilation speed is taken from
#' Kret & Sjak-Shie (2018).
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
pad_gaps <- function(pupil, time, gap_minimum = 75000, padding_before = 50000,
  padding_after = 50000) {

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
  df <- tibble(
    time = time,
    pupil = pupil
  )

  # Determine gap information
  df <- df %>%
    mutate(
      gap = if_else(is.na(pupil), 1, 0),
      gap = if_else(gap == 1 & lag(gap, default = 0) == 0, 1, 0),
      gap = if_else(is.na(pupil), cumsum(gap), NA_real_)
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
  if (length(unique(df$gap)) == 1) {
    return(pull(df, pupil))
  }


  # Pad gaps before the gap
  if (padding_before) {
    df <- df %>%
      mutate(
        gap_before = if_else(is.na(gap) & !is.na(lag(gap)), 1, 0),
        gap_before = cumsum(gap_before)
      ) %>%
      group_by(gap_before) %>%
      mutate(
        time_before = min(gap_start, na.rm = TRUE),
        pupil = if_else(time >= time_before - padding_before,
          NA_real_, pupil)
      ) %>%
      ungroup() %>%
      select(-gap_before, -time_before)
  }

  # Pad gaps after the gap
  if (padding_after) {
    df <- df %>%
      mutate(
        gap_after = if_else(!is.na(gap) & is.na(lag(gap)), 1, 0),
        gap_after = cumsum(gap_after)
      ) %>%
      group_by(gap_after) %>%
      mutate(
        time_after = max(gap_end, na.rm = TRUE),
        pupil = if_else(time <= time_after + padding_before,
          NA_real_, pupil)
      ) %>%
      ungroup() %>%
      select(-gap_after, -time_after)
  }

  return(pull(df, pupil))
}

