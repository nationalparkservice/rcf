data <- readr::read_csv(system.file("extdata","BAND_small.csv", package = "rcf"))

# my_directory <- here::here()

test_that("Function results in a dataframe", {
  skip_on_cran()
  pca_data <- summarize_for_pca(SiteID = "BAND",
                               data = calc_thresholds(SiteID = "BAND",
                                                      data = data,
                                                      past_years = c(1950,2000)))
  expect_s3_class(pca_data, "data.frame")
})


test_that("Past years are input correctly", {
  expect_error(
    summarize_for_pca(SiteID = "BAND",
                data = data,
                past_years = c(1950:2000),
                future_year = 2040,
                directory = my_directory),
    regexp = "You may have entered the range of years")
})

test_that("Past year reference range is valid", {
  expect_error(
    summarize_for_pca(SiteID = "YELL",
                data = data,
                past_years = c(2020, 2030),
                future_year = 2040,
                directory = my_directory),
    regexp = "The requested period for historic values is incorrect for this function.")
})

test_that("Past year range is at least 30 years",{
  expect_error(
    summarize_for_pca(SiteID = "BAND",
                data = data,
                past_years = c(1950, 1960),
                directory = my_directory),
    regexp = "Past year range must be at least 30 years."
  )
})

test_that("Past years entered in correct order", {
  expect_error(summarize_for_pca(SiteID = "BAND",
                                 data = data,
                                 past_years = c(2000, 1950),
                                 directory = my_directory),
               regexp = "Past years entered in incorrect order"

  )
})

test_that("Future year is a single year", {
  expect_error(
    summarize_for_pca(SiteID = "BAND",
                data = data,
                past_years = c(1950,2000),
                future_year = c(2040, 2041),
                directory = my_directory),
    regexp = "Future year should be")
})

test_that("Future year is between 2040 and 2084", {
  expect_error(
    summarize_for_pca(SiteID = "BAND",
                data = data,
                past_years = c(1950,2000),
                future_year = 2030,
                directory = my_directory),
    regexp = "Future year can only be")
})

#
# test_that("tempdir() gives warning", {
#   expect_warning(calc_thresholds(SiteID = "BAND",
#                                  data = data,
#                                  past_years = c(1950, 2000)),
#                  regexp = "Files have been saved to temporary directory")
# })
