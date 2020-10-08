#' Calculate summary statistics of validity indicators
#'
#' \code{describe_validity} returns a data frame with summary statistics
#' describing the eye tracker's validity indicators
#'
#' @param data A data frame.
#' @param ... One or more numeric columns in the data frame that contain data on
#' the validity of the recordings.
#' @param log A boolean indicating whether the result is added to the log.
#' @param message A string containing an optional message to add to the log.
#'
#' @details The data set can be grouped using \strong{dplyr}'s \code{group_by}
#' so that the gap descriptives will be calculated for each group level.
#'
#' @importFrom magrittr %>%
#' @importFrom knitr kable
#'
#' @export
describe_validity <- function(data, ..., log = TRUE) {

  # Quote the provided columns names
  vars <- dplyr::enquos(...)

  # Check whether data is indeed a data frame
  if (!"data.frame" %in% class(data)) {
    stop("data is not a data frame.")
  }

  # Check whether any vars have been provided
  if (length(vars) == 0) {
    stop("No variables found; please specify one more numeric variables.")
  }

  # Check whether the vars are numeric
  if (ncol(dplyr::select_if(dplyr::select(data, !!!vars), is.numeric)) !=
      length(vars)) {
    stop("One or more of the validity variables are not numeric.")
  }

  output <- tibble::tibble()

  # Calculate gap descriptives
  for (var in vars) {
    descriptives <- data %>%
      dplyr::group_by(!!var) %>%
      dplyr::summarize(n = n()) %>%
      dplyr::rename(code := !!var) %>%
      dplyr::mutate(
        pct = n / sum(n) * 100,
        var = quo_name(var)
      ) %>%
      dplyr::select(var, everything())

    output <- dplyr::bind_rows(output, descriptives)
  }

  # Ungroup the output data frame
  output <- dplyr::ungroup(output)

  return(output)
}
