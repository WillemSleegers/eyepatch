#' Control for baseline differences
#'
#' \code{baseline_control} returns a vector of pupil data
#'
#' @param pupil A numeric vector containing pupil data.
#' @param baseline_period A numeric vector that specifies the two time periods
#' @param grouping A vector containing grouping information
#'
#' @examples
#'
#' @export
baseline_control <- function(pupil, time, baseline_period, grouping = NULL,
  log = FALSE, log_file = NULL) {

  # Create a dataframe containing the pupil observations and condition values
  df <- tibble::tibble(
      pupil = pupil,
      time = time
    )

  # Check whether grouping information has been provided
  if (!is.null(grouping)) {
    df <- apply_grouping(df, grouping)
  }

  # Control for baseline differences before each trial
  df <- mutate(df,
      baseline = ifelse(between(time, baseline_period[1], baseline_period[2]),
        pupil, NA),
      baseline_mean = mean(baseline, na.rm = TRUE),
      pupil = pupil - baseline_mean
    )

  output <- pull(df, pupil)

  # Log
  if (log) {
    # Find the log file if it is not specified
    if (is.null(log_file)) {
      log_file <- find_log()
    }

    # Log the smoothing step
    text <- paste0("Controlled for baseline differences in '",
      deparse(substitute(pupil)),"' by subtracting the average pupil size ",
      "between ", baseline_period[1], " and ", baseline_period[2])

    write(text, file = log_file, append = TRUE)
  }

  return(output)
}
