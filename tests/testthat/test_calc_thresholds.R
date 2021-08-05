
data <- readr::read_csv(system.file("extdata","BAND_small.csv", package = "rcf"))

my_directory <- here::here()


test_that("Function results in a dataframe", {
  skip_on_cran()
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
    regexp = "The requested period for historic values is incorrect for this function.")
})

test_that("Past year range is at least 30 years",{
  expect_error(
    calc_thresholds(SiteID = "BAND",
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
               regexp = "Past years entered in incorrect order")
})

test_that("tempdir() gives warning", {
  expect_warning(calc_thresholds(SiteID = "BAND",
                                 data = data,
                                 past_years = c(1950, 2000)),
                 regexp = "Files have been saved to temporary directory")
})

test_that("csv size gives warning", {
  expect_warning(calc_thresholds(SiteID = "BAND",
                                 data = data,
                                 past_years = c(1950, 2000),
                                 directory = my_directory),
                 regexp = "thresholds.csv generated successfully")
})
