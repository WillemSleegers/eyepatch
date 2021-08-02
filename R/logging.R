#' Create a log of pupil preprocessing steps
#'
#' \code{create_log} creates a variable can be updated to log the pupil
#' preprocessing steps and how these steps affected the pupil data.
#'
#' @details Use the \code{cat} function to print the log to the console in a
#' human-friendly format.
#'
#' @examples
#' # Create the log
#' log <- create_log()
#'
#'
#'
#' # Print the log
#' cat(log)
#'
#' @export
create_log <- function() {

  # Create log variable with a title line
  log <- "# Pupil preprocessing log"

  # Add creation date information
  date <- paste("Log created on", Sys.time())
  log <- paste0(log, "\n\n", date)

  return(log)
}

#' Add a section in the pupil preprocessing log
#'
#' \code{log_section} adds a section to the pupil preprocessing log.
#'
#' @param log A variable containing the pupil preprocessing log, created with
#' \code{create_log}.
#' @param section A character string specifying the section name.
#'
#' @details \code{log_section} automatically turns the section name into a
#' level-2 Markdown header and separates it from preceding messages with two
#' line breaks.
#'
#' @examples See \code{\link[eyepatch]{create_log}} for an example.
#'
#' @export
log_section <- function(log, section) {

  # Add the header to the existing log, after adding two line breaks
  log <- paste0(log, "\n\n", "##", section)

  return(log)
}

#' Add a message in the pupil preprocessing log
#'
#' \code{log_message} adds a message to the pupil preprocessing log.
#'
#' @param log A variable containing the pupil preprocessing log, created with
#' \code{create_log}.
#' @param message A character string specifying the message.
#'
#' @examples See \code{\link[eyepatch]{create_log}} for an example.
#'
#' @export
log_message <- function(log, message) {

  # Add the message
  log <- paste0(log, "\n", message)

  return(log)
}

#' View the pupil preprocessing log
#'
#' \code{view_log} prints the pupil preprocessing log to the console.
#'
#' @param log A variable containing the pupil preprocessing log, created with
#' \code{create_log}.
#'
#' @examples See \code{\link[eyepatch]{create_log}} for an example.
#'
#' @export
view_log <- function(log) {

  # Print the log to the console
  cat(log)
}

#' Write the pupil preprocessing log to a file
#'
#' \code{write_log} saves the log to a file on your computer.
#'
#' @param log A variable containing the pupil preprocessing log, created with
#' \code{create_log}.
#' @param file A character string specifying the path and file name.
#'
#' @examples See \code{\link[eyepatch]{create_log}} for an example.
#'
#' @export
write_log <- function(log, file) {
  writeLines(log, file)
}

