#' Sparsity filter
#'
#' \code{sparse_to_na} removes sparse samples in the data.
#'
#' @param pupil A numeric vector of pupil size measurements.
#' @param time A vector containing the timestamps associated with the
#' pupil size measurements.
#' @param gap_criterion A numeric value specifying the gap duration. Clusters
#' that follow a gap of this size or larger will be removed.
#' @param cluster_criterion A numeric value specifying the cluster duration.
#' Clusters smaller than this this value will be removed.
#'
#' @examples
#' # Load the "dplyr", "tidyr", and "ggplot2" packages:
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#'
#' # Example 1: Artificial data
#' # Create some artificial data:
#' data <- tibble(
#'   time = 1:8,
#'   pupil_left = c(3.11, 3.13, NA, NA, 3.24, NA, NA, 3.12),
#'   pupil_right = c(2.92, 2.95, NA, NA, 3.06, NA, NA, 2.95)
#' )
#' data
#'
#' # Remove the cluster in the left eye:
#' mutate(data,
#'   pupil_left = sparse_to_na(pupil_left, time, gap_criterion = 2,
#'     cluster_criterion = 1)
#' )
#'
#' # Example 2: Realistic data
#' sparse
#'
#' # Remove a sparse cluster of the left eye:
#' sparse <- mutate(sparse,
#'   pupil_left_new = sparse_to_na(pupil_left, timestamp, gap_criterion = 10,
#'     cluster_criterion = 5)
#'   )
#'
#' # Restructure the data and plot the results to compare the pupil measurements
#' # before and after padding the gaps:
#' sparse %>%
#'   rename(pupil_left_old = pupil_left) %>%
#'   pivot_longer(
#'     cols = c(pupil_left_old, pupil_left_new),
#'     names_to = "status",
#'     names_pattern = "(old|new)",
#'     values_to = "pupil_size"
#'  ) %>%
#'  ggplot(aes(x = timestamp, y = pupil_size, color = status)) +
#'    geom_point()
#'
#' @importFrom magrittr %>%
#'
#' @export
sparse_to_na <- function(pupil, time, gap_criterion = 40,
    cluster_criterion = 50) {

  # Check whether the "pupil" argument is numeric:
  if (!is.numeric(pupil)) {
    stop("'pupil' must be a vector of numeric values.")
  }

  # Check whether the "time" argument is numeric:
  if (!is.numeric(time)) {
    stop("'time' must be a vector of numeric values.")
  }

  # Check whether "pupil" and "time" have the same number of values:
  if (length(pupil) != length(time)) {
    stop("'pupil' and 'time' must have the same number of values.")
  }

  # Check whether the "gap_criterion" argument is numeric:
  if (!is.numeric(gap_criterion)) {
    stop("'gap_criterion' must be a numeric value.")
  }

  # Check whether the "gap_criterion" argument is larger than 0:
  if (gap_criterion <= 0) {
    stop("'gap_criterion' must be larger than 0.")
  }

  # Check whether the "cluster_criterion" argument is numeric:
  if (!is.numeric(cluster_criterion)) {
    stop("'cluster_criterion' must be a numeric value.")
  }

  # Check whether the "cluster_criterion" argument is larger than 0:
  if (cluster_criterion <= 0) {
    stop("'cluster_criterion' must be larger than 0.")
  }

  # Loop over the pupil measurements and remove clusters when found:
  gap <- FALSE
  cluster <- FALSE
  gap_duration <- 0
  cluster_duration <- 0

  for (i in 1:length(pupil)) {
    if (is.na(pupil[i])) {

      if (cluster == TRUE) {
        cluster_end <- time[i]
        cluster_duration = cluster_end - cluster_start

        if (gap_duration >= gap_criterion &
            cluster_duration <= cluster_criterion) {
          pupil[time >= cluster_start & time <= cluster_end] <- NA
        }
      }

      if (gap == FALSE) {
        gap_start <- time[i]
      }
      gap <- TRUE
      cluster <- FALSE
    } else {
      if (cluster == FALSE) {
        cluster_start <- time[i]
      }

      if (gap == TRUE) {
        gap_end <- time[i]
        gap_duration <- gap_end - gap_start
      }
      gap <- FALSE
      cluster <- TRUE
    }
  }

  return(pupil)
}

