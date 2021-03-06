% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outliers_to_missing.R
\name{remove_outliers}
\alias{remove_outliers}
\title{Remove outliers}
\usage{
remove_outliers(x, pupil = NULL, constant = 10, log_file = NULL, type = NULL)
}
\arguments{
\item{x}{A numeric vector containing the outlier data.}

\item{pupil}{An optional numeric vector of pupil size measurements. If
specified, the function will return a new vector of pupil sizes, but with
outliers removed.}

\item{constant}{An integer that raises or lowers the cutoff threshold.}
}
\description{
\code{remove_outliers} removes pupil size outliers based on the median
absolute deviation (MAD).
}
\details{
See Leys, Ley, Klein, Bernard, & Licata (2013) for details regarding
why the MAD should be used.
}
\examples{
library(dplyr)
library(ggplot2)

# Example 1: Outlier removal based on pupil dilation speed
blink <- mutate(blink,
    pupil_left_d = dilation_speed(pupil_left, timestamp),
    pupil_right_d = dilation_speed(pupil_right, timestamp),
    pupil_left_clean = remove_outliers(pupil_left_d, pupil_left),
    pupil_right_clean = remove_outliers(pupil_right_d, pupil_right)
  )

ggplot(blink, aes(x = timestamp)) +
  geom_point(aes(y = pupil_left), color = "red") +
  geom_point(aes(y = pupil_left_clean))

}
