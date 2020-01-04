#' Log section
#'
#' \code{log_section} adds a section to the pupil preprocessing log.
#'
#' @param section A character string specifying the section name.
#' @param log_file A character string specifying the path to the log file.
#'
#' @examples
#'
#' @export
log_section <- function(section, log_file = NULL) {

  # Find the log file
  if (is.null(log_file)) {
    log_file <- find_log()
  } else {
    if (!file.exists(log_file)) {
      stop("Log not found.")
    }
  }

  # Add an empty line before the section
  write("", file = log_file, append = TRUE)

  # Add the section to the log
  text <- paste0("## ", section)
  write(text, file = log_file, append = TRUE)
}

