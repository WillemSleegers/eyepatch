#' Control for baseline differences
#'
#' \code{baseline_control} returns a vector of pupil data
#'
#' @param pupil A numeric vector containing pupil data.
#' @param baseline_period A numeric vector that specifies the two time periods
#'
#' @examples
#'
#' @export
baseline_control <- function(pupil, time, baseline_period = c(-500, 0)) {
  baseline_mean <- mean(pupil[between(time, baseline_period[1],
    baseline_period[2])], na.rm = TRUE)

  pupil <- pupil - baseline_mean

  return(pupil)
}
