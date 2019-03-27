#' Smoothen the pupil dilate
#'
#' \code{smoothen} returns a vector of smoothened pupil data according to the
#' specified method.
#'
#' @param pupil A numeric vector containing pupil data.
#' @param type The type of interpolation to be used. Possible values are
#' 'loess' (default) or 'lowpass'.
#'
#' @examples
#' trials <- trials %>%
#'   group_by(trial) %>%
#'   mutate(pupil_left_smooth = smoothen(pupil_left, time))
#'
#' ggplot(filter(trials, trial == 6 & time <= 500), aes(x = time)) +
#'   geom_point(aes(y = pupil_left)) +
#'   geom_line(aes(y = pupil_left_smooth), color = "red")
#'
#' @export
smoothen <- function(pupil, time, method = "loess", span = 0.1) {

  if (method == "loess") {
    # Run the loess regression
    model <- loess(pupil ~ time, span = span)

    # Smoothen the data by predicting new values for each time moment
    pupil_new <- predict(model, time)
  }

  return(pupil_new)
}



