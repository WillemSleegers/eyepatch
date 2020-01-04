#' Set invalid measures to missing
#'
#' \code{invalid_to_na} sets invalid measures to missing (NA).
#'
#' @param pupil A numeric vector containing pupil data.
#' @param condition A condition to determine which values should be set to NA.
#' @param log A logical value. Should the action and results be logged?
#' @param log_file A character string specifying the path to the log file.
#'
#' @examples
#'
#' # Set 4 to missing in a vector from 1 to 10
#' invalid_to_na(1:10, 1:10 == 4)
#'
#' # Set all pupil sizes with a value of -1 to NA
#' invalid %>%
#'   mutate(
#'     pupil_left = invalid_to_na(pupil_left, pupil_left == -1),
#'     pupil_right = invalid_to_na(pupil_right, pupil_right == -1)
#'   )
#'
#' # Or, set pupil sizes to NA based on a validity score
#'
#' invalid %>%
#'   mutate(
#'     pupil_left = invalid_to_na(pupil_left, validity_left == 4),
#'     pupil_right = invalid_to_na(pupil_right, validity_right == 4)
#'   )
#'
#'
#' @export
invalid_to_na <- function(pupil, condition, log = FALSE, log_file = NULL) {

  # Check whether pupil is numeric
  if (!is.numeric(pupil)) {
    stop("pupil must be numeric.")
  }

  # Check if the result of condition is a logical value
  if (!is.logical(condition)) {
    stop("condition must be a vector of logical values (TRUE/FALSE)")
  }

  # Set values to NA
  output <- pupil
  output[condition] <- NA

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
      "%) from '", deparse(substitute(pupil)), "' based on the condition: ",
      deparse(substitute(condition)))
    write(text, file = log_file, append = TRUE)
  }

  return(output)
}
