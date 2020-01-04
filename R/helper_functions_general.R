apply_grouping <- function(df, grouping) {
  if (is.list(grouping)) {

    df <- bind_cols(df, bind_cols(grouping))
    # TODO: See if we can optimize this; perhaps by using quotes as column names
    if ("time" %in% names(df)) {
      df <- group_by_at(df, vars(-pupil, -time))
    } else {
      df <- group_by_at(df, vars(-pupil))
    }

  } else {
    if (length(grouping) != nrow(df)) {
      stop("Grouping length is not equal to the length of pupil observations")
    } else {
      df <- bind_cols(df, tibble(group = grouping))
      df <- group_by(df, group)
    }
  }

  return(df)
}
