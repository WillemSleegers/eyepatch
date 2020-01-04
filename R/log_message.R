#' Log message
#'
#' \code{log_message} adds a message to the pupil preprocessing log.
#'
#' @param message A character string specifying the message.
#' @param log_file A character string specifying the path to the log file.
#'
#' @examples
#'
#' @export
log_message <- function(message, log_file = NULL) {

  # Find the log file
  if (is.null(log_file)) {
    log_file <- find_log()
  } else {
    if (!file.exists(log_file)) {
      stop("Log not found.")
    }
  }

  # Add the section to the log
  write(message, file = log_file, append = TRUE)
}

