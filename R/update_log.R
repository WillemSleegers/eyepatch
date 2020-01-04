#' Update the pupil preprocessing log
#'
#' \code{update_log} updates the pupil preprocessing log.
#'
#' @param location A character string specifying the location where the log
#' should be created.
#' @param file_name A character string specifying the name of the file. By default
#' this is set to 'pupil-preprocessing'
#' @param overwrite A boolean specifying whether the log file should be
#' overwritten.
#'
#'
#' @examples
#'
#' # Validity
#' # Outliers
#' # Gaps
#' # Sparsity
#' # Smoothing
#'
#' @export
update_log <- function(location = NULL, file_name = NULL,
  overwrite = FALSE) {

  # Check whether the arguments are of the right type
  if (!is.null(location)) {
    if (!is.character(location)) {
      stop("location is not a character string.")
    }
  }

  if (!is.null(file_name)) {
    if (!is.character(file_name)) {
      stop("file_name is not a character string.")
    }
  }

  if (!is.logical(overwrite)) {
    stop("overwrite is not a boolean value (TRUE or FALSE).")
  }

  # Append the character arguments
  file_path <- ""

  if (!is.null(location)) {

    # Add a forward slash if it is missing
    if (str_detect(location, "/$")) {
      file_path <- paste0(location)
    } else {
      file_path <- paste0(location, "/")
    }
  }

  if (!is.null(file_name)) {
    file_path <- paste0(file_path, file_name)
  } else {
    file_path <- paste0(file_path, "pupil-preprocessing")
  }

  # Append '.log' to the location path
  file_path <- paste0(file_path, ".log")

  # Create the file
  if (file.exists(file_path) & !overwrite) {
    stop("File already exists.")
  } else {

    if (file.exists(file_path)) {
      file.remove(file_path)
    }

    file.create(file_path)
    print(paste("Created", file_path))

    # Add setup information
    write("# Pupil preprocessing log", file = file_path, append = TRUE)
    date <- paste("Log created on", Sys.time())
    write(date, file = file_path, append = TRUE)
    write("", file = file_path, append = TRUE)
  }
}

