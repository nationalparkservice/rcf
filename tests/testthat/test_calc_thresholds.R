
data <- readr::read_csv(system.file("extdata","BAND_small.csv", package = "rcf"))

my_directory <- here::here()

test_that("Function results in a dataframe", {
  threshold_data <- calc_thresholds(
    SiteID = "BAND",
    data = data,
    past_years = c(1950,2000),
    directory = my_directory
  )
  expect_s3_class(threshold_data, "data.frame")
})

test_that("Past years are input correctly", {
  expect_error(
    calc_thresholds(SiteID = "BAND",
                    data = data,
                    past_years = c(1950:2000),
                    directory = my_directory))
})

test_that("Past year reference range is valid", {
  expect_error(
    calc_thresholds(SiteID = "YELL",
                    data = data,
                    past_years = c(2020, 2030),
                    directory = my_directory),
    regexpr = "The requested period for historic values is incorrect for this function.")
})
