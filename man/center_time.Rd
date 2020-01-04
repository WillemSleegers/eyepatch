% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/center_time.R
\name{center_time}
\alias{center_time}
\title{Centers time around a specified moment}
\usage{
center_time(time, condition = NULL)
}
\arguments{
\item{time}{A vector containing the timestamps associated with the
pupil size measurements.}

\item{condition}{A condition specifying when the onset of the trial begins.}
}
\description{
\code{center_time} centers time around a specified moment, such as the start
of a trial or when a particular event occurs.
}
\examples{

# Create example data
data <- tibble(
  timestamp = 1:30,
  trial = rep(1:3, each = 10),
  event = rep(c(rep("baseline", 3), rep("event", 7)), times = 3)
 )

 # Create a new time column with the time centered at 0
 data <- mutate(data, time = center_time(timestamp))

 # More interestingly, center time at 0, for each trial
 data <- data \%>\%
   group_by(trial) \%>\%
   mutate(time = center_time(timestamp))

 # More more interestingly, center time at 0, for each trial, starting after
 # the baseline period of the trial
 data <- data \%>\%
   group_by(trial) \%>\%
   mutate(time = center_time(timestamp, event == "event"))

}