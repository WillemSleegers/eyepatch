#' Create a log of pupil data transformations
#'
#' \code{create_pupil_log} creates a file to store the pupil preprocessing steps
#' into.
#'
#' @param file A character string specifying the path where the log should be
#' created.
#' @param overwrite A boolean specifying whether the file should be overwritten.
#'
#' @export
create_pupil_log <- function(file, overwrite = FALSE) {

  # Append '.log' to the file path
  file <- paste0(file, ".log")

  # Create the file
  if (file.exists(file) & !overwrite) {
    stop("File already exists.")
  } else {
    file.create(file, overwrite = overwrite)
    print(paste("Created", file))

    # Add setup information
    write("Pupil preprocessing log", file = file, append = TRUE)
    date <- paste("Log created on", Sys.time())
    write(date, file = file, append = TRUE)
  }
}



