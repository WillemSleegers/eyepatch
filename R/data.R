#' Missing timestamp data
#'
#' Pupil size data from a single trial that contains implicit missing timestamps.
#'
#' @format A data frame with 269 rows and 10 columns:
#' \describe{
#'   \item{ID}{participant number}
#'   \item{trial}{trial number}
#'   \item{timestamp}{timestamp (in ms)}
#'   \item{event}{onscreen event, either a fixation cross or feedback}
#'   \item{pupil_left}{pupil size of the left eye}
#'   \item{pupil_right}{pupil size of the right eye}
#'   \item{validity_left}{Tobii validity marker of the left eye}
#'   \item{validity_right}{Tobii validity marker of the right eye}
#'   \item{eyetracker}{name of the eyetracker that was used to record the data}
#'   \item{sampling_rate}{sampling rate of the eye tracker (in Hz)}
#'
"missing"
