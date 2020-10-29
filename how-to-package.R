
# Shortcuts ---------------------------------------------------------------

devtools::document()
devtools::install()

# TODOs -------------------------------------------------------------------

# Check if dilation speed outliers remove observations surrounding gaps
# Remove grouping after each function

# Creating data files -----------------------------------------------------

# Read in data
trial1 <- readr::read_csv("data/trial1.csv")

# Save data
usethis::use_data(trial1, overwrite = TRUE)

# Documentation -----------------------------------------------------------

usethis::use_vignette("introduction", title = "Introduction to eyepatch")

# Build website -----------------------------------------------------------

# Run once to configure package to use pkgdown
usethis::use_pkgdown()

# Run to build the website
pkgdown::build_site()

# Preview the site
pkgdown::preview_site()

# Delete website files
pkgdown::clean_site()

# Related packages --------------------------------------------------------

# - https://cran.r-project.org/web/packages/PupilPre/index.html


library(tidyverse)

ggplot(gaps, aes(x = timestamp, y = pupil_left)) +
  geom_point()

gaps <- mutate(gaps, pupil_left_clean = pad_gaps(pupil_left, timestamp,
  gap_minimum = 7, padding_before = 5, padding_after = 5))

ggplot(gaps, aes(x = timestamp)) +
  geom_point(aes(y = pupil_left), color = "red") +
  geom_point(aes(y = pupil_left_clean), color = "green")
