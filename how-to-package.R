
# Shortcuts ---------------------------------------------------------------

devtools::document()
devtools::install()

# TODOs -------------------------------------------------------------------

# Check if dilation speed outliers remove observations surrounding gaps
# Remove grouping after each function

# Creating data files -----------------------------------------------------

# Read in data
missing <- readr::read_csv("data/missing.csv")

# Save data
usethis::use_data(missing, overwrite = TRUE)

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




