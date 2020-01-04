  find_log <- function() {

    # Find all files ending with .log
    possible_logs <- list.files(recursive = TRUE, pattern = ".log$")

    # If only 1 result is found, assume that is the log file
    if (length(possible_logs) == 1) {
      log <- possible_logs[1]
    } else {
      if (length(possible_logs) > 1) {
        stop("Multiple log files found; please specify the correct log file.")
      } else {
        stop("No log file found.")
      }
    }

    return(log)
  }

  create_log_section <- function(section, file) {
    # Determine previous section to see whether we either have to create a new
    # section
    lines <- readLines(file)
    sections <- lines[str_detect(lines, "^##")]

    if (length(sections) > 0) {
      last_section <- str_remove(sections[length(sections)], "## ")
    } else {
      last_section <- ""
    }

    if (section != last_section) {
      write("", file = file, append = TRUE)
      write(paste("##", section), file = file, append = TRUE)
      write("", file = file, append = TRUE)
    }
  }

  determine_log_section <- function() {
    log_file <- find_pupil_log()

  }
