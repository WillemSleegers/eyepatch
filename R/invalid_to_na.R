#' Set invalid pupil measurements to missing
#'
#' \code{invalid_to_na} sets invalid pupil measurements to missing (NA).
#'
#' @param pupil A numeric vector containing pupil data.
#' @param condition A condition to determine which values should be set to NA.
#'
#' @examples
#' # Example 1: Artificial vector
#' # Set the value '4' to missing in a vector from '1' to '10':
#' invalid_to_na(1:10, 1:10 == 4)
#'
#' # Example 2: Artificial data
#' # Load the "dplyr" package:
#' library(dplyr)
#'
#' # Create some artificial data:
#' data <- tibble(
#'   pupil_left = c(3.15, 3.14, 3.13, -1, -1, 3.11),
#'   pupil_right = c(2.92, 2.89, 2.93, -1, -1, 2.97),
#'   validity_left = c(0, 0, 0, 4, 4, 0),
#'   validity_right = c(0, 0, 0, 4, 4, 0)
#' )
#' data
#'
#' # Set all pupil sizes with a value of -1 to NA:
#' mutate(data,
#'   pupil_left = invalid_to_na(pupil_left, pupil_left == -1),
#'   pupil_right = invalid_to_na(pupil_right, pupil_right == -1)
#' )
#'
#' # Or, set pupil sizes to NA based on values in the validity columns:
#' mutate(data,
#'   pupil_left = invalid_to_na(pupil_left, validity_left == 4),
#'   pupil_right = invalid_to_na(pupil_right, validity_right == 4)
#' )
#'
#' # Example 3: Realistic data
#' # Inspect data:
#' trial1
#' count(trial1, validity_left, validity_right)
#' slice(trial1, 100:115)
#'
#' # Set invalid pupil observations of the left eye to NA:
#' trial1 <- mutate(trial1, pupil_left = invalid_to_na(pupil_left,
#'   validity_left == 4))
#'
#' # Inspect the data frame again to see whether it worked:
#' slice(trial1, 100:115)
#'
#' @export
invalid_to_na <- function(pupil, condition) {

  # Check whether the "pupil" argument is numeric:
  if (!is.numeric(pupil)) {
    stop("'pupil' must be a vector of numeric values")
  }

  # Check whether the "condition" argument consists of only logical values:
  if (!is.logical(condition)) {
    stop("'condition' must be a vector of logical values (TRUE/FALSE).")
  }

  # Check whether "pupil" and "condition have the same number of values:
  if (length(pupil) != length(condition)) {
    stop("'pupil' and 'condition' must have the same number of values.")
  }

  # Set values to NA:
  output <- pupil
  output[condition] <- NA

  return(output)
}
