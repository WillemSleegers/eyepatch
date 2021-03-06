% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dilation_speed_outliers_to_na.R
\name{dilation_speed_outliers_to_na}
\alias{dilation_speed_outliers_to_na}
\title{Set dilation speed outliers to missing.}
\usage{
dilation_speed_outliers_to_na(pupil, time, constant = 10, log = FALSE)
}
\arguments{
\item{pupil}{A numeric vector of pupil size measurements.}

\item{time}{A vector containing the timestamps associated with the
pupil size measurements.}

\item{constant}{A numeric value specifying the threshold for outlier removal.}

\item{log}{A logical value. Should the action and results be logged?}

\item{log_file}{A character string specifying the path to the log file.}
}
\description{
\code{dilation_speed_outliers_to_na} calculates the dilation speed based on
timestamps and pupil size measurements and then sets outliers to missing
(NA).
}
\details{
The exact method of calculating the dilation speed is taken from
Kret & Sjak-Shie (2018).
}
\examples{
library(dplyr)

blink <- mutate(blink,
    pupil_left = dilation_speed_outliers_to_na(pupil_left, timestamp),
    pupil_right = dilation_speed_outliers_to_na(pupil_right, timestamp)
  )

}
