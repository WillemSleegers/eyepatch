low_pass_filter <- function(pupil, cutoff_frequency, sampling_frequency,
    padding) {

  # Check if there are missing data points
  if (sum(is.na(pupil)) > 1) {
    warning("Missing data found; returning unsmoothened data.")
    return(pupil)
  }

  # Pad the pupil data with the first and last pupil observations to prevent
  # artifacts at the start and end caused by applying the low pass filter
  pupil <- c(rep(first(pupil), padding), pupil, rep(last(pupil), padding))

  # Determine critical frequencies as required by signal::filtfilt()
  W <- cutoff_frequency / (sampling_frequency / 2)

  # Generate a Butterworth filter
  filt <- signal::butter(1, W, type = "low")

  # Apply low pass filter
  pupil_smooth <- signal::filtfilt(filt, pupil)

  # Remove padding
  pupil_smooth <- pupil_smooth[(padding + 1):(length(pupil_smooth) - padding)]

  # Return output
  return(pupil_smooth)
}

fit_loess <- function(x, y, span) {
  tryCatch(
    {
      # Perform a loess regression
      model <- loess(y ~ x, span = span)

      # Return predicted values for x
      # We can't simply return the fitted values because missing data is
      # automatically removed
      return(predict(model, tibble(x = x)))
    },
    error = function(cond) {
      warning(cond)
      return(NA)
    },
    warning = function(cond) {
      warning(cond)
      return(NA)
    }
  )
}


filtfilt_with_init <- function(filt, a, x, init)  {
    y = filter(filt, a, c(x, numeric(2 * max(length(a), length(filt)))), init=init)
    y = rev(filter(filt, a, rev(y)))[seq_along(x)]
    y
}

