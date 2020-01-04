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

low_pass_filter <- function(pupil, frequency, sampling_rate, padding) {

  # Create a data frame with the pupil observations and a column identifying
  # each observation
  df <- tibble::tibble(
    pupil = pupil,
    n = 1:length(pupil)
  )

  # Create a new df that does not contain any missing observations
  df_complete <- dplyr::filter(df, !is.na(pupil))

  # Pad the data frame with the first and last pupil observations to prevent
  # artifacts at the start and end caused by applying the low pass filter
  df_complete <- dplyr::bind_rows(
    tibble::tibble(pupil = rep(first(pull(df_complete, pupil)), padding)),
    df_complete,
    tibble::tibble(pupil = rep(last(pull(df_complete, pupil)), padding)),
  )

  # Determine critical frequencies as required by signal::filtfilt()
  W <- frequency / (sampling_rate / 2)

  # Generate a Butterworth filter
  filt <- signal::butter(1, W, type = "low")

  # Apply low pass filter
  df_complete <- mutate(df_complete, pupil_filtered = filtfilt(filt, pupil))

  # Merge the result back to the original pupil observations (including missing
  # observations)
  df <- left_join(df, df_complete, by = "n")

  # Return output
  return(pull(df, pupil_filtered))
}

