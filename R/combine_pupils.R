#' Combine the left and right pupil observations
#'
#' \code{combine_pupils} returns the average between the left and right pupil.
#'
#' @param pupil_left A numeric vector containing pupil data.
#' @param pupil_right A numeric vector containing pupil data.
#' @param grouping A vector containing grouping information
#'
#' @examples
#'
#' @export
combine_pupils <- function(pupil_left, pupil_right, grouping = NULL,
  log = FALSE, log_file = NULL) {

  # Create a dataframe containing the left and right pupil
  df <- tibble::tibble(
      pupil_left = pupil_left,
      pupil_right = pupil_right
    )

  # Check whether grouping information has been provided
  if (!is.null(grouping)) {
    df <- apply_grouping(df, grouping)
  }

  # Take the average of the two pupil observations
  # If one pupil size is missing, calculate

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
