
# TODOs -------------------------------------------------------------------

# - Figure out the best way to do logging
# - Add examples to create_log()
# - Check if dilation speed outliers remove observations surrounding gaps
# - Remove grouping after each function
# - Check whether missing values should be filled with the last known pupil value
#   in the dilation speed outlier function

# Update ------------------------------------------------------------------

# Update documentation and (re)install the package
devtools::document()
devtools::install()
detach("package:eyepatch", unload = TRUE)
library(eyepatch)

# Restart sessions --------------------------------------------------------

.rs.restartR()

# Testing -----------------------------------------------------------------

# Add a test
# usethis::use_test("add_stats")

# Test all tests
devtools::test()

# Test specific tests
testthat::test_file("tests/testthat/test_counts.R")

# Create a vignette -------------------------------------------------------

# usethis::use_vignette("read-and-use-a-tidystats-file")

# Add a data set ----------------------------------------------------------

usethis::use_data(sparse, overwrite = TRUE)

# Build website -----------------------------------------------------------

# Run once to configure package to use pkgdown
# usethis::use_pkgdown()

# Run to build the website
pkgdown::build_site()

# Preview the site
pkgdown::preview_site()

# Delete website files
pkgdown::clean_site()

# CRAN submission ---------------------------------------------------------

# Check examples
devtools::run_examples()

# Check tests
devtools::test()

# Check package
# devtools::load_all()
devtools::check()
devtools::check(args = c('--run-donttest')) # Without examples test
devtools::check(args = c('--as-cran'))

# run R CMD check on CRAN’s servers
devtools::check_win_devel()
devtools::check_win_release()

# Build tar
devtools::build()

# Related packages --------------------------------------------------------

# - https://cran.r-project.org/web/packages/PupilPre/index.html
