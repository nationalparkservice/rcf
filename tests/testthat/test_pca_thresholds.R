pca_data <- readr::read_csv(system.file("extdata", "BAND_pca_summary.csv", package = "rcf"))
# pca_data <- pca_data %>%
#     dplyr::filter(gcm %in% c("bcc-csm1-1.rcp45", "bcc-csm1-1.rcp85", "BNU-ESM.rcp45",
#                              "BNU-ESM.rcp85", "CanESM2.rcp45", "CanESM2.rcp85", "CCSM4.rcp45",
#                              "CCSM4.rcp85","GFDL-ESM2G.rcp45", "GFDL-ESM2G.rcp85"))
raw_data <- readr::read_csv("https://irmadev.nps.gov/DataStore/DownloadFile/660685")

all_data <- calc_thresholds(
    SiteID = "BAND",
    data = raw_data,
    past_years = c(1950,2000))
  #readr::read_csv(system.file("extdata","BAND_thresholds.csv", package = "rcf"))


my_directory <- here::here()

test_that("Function results in a dataframe", {
  skip_on_cran()
  threshold_summary <- pca_thresholds(SiteID = "BAND",
                                   pca_data = cf_pca(SiteID = "BAND",
                                                     data = pca_data,
                                                     directory = my_directory),
                                   all_data = all_data,
                                   summarize_by = "season",
                                   directory = my_directory)
  expect_s3_class(threshold_summary, "data.frame")
})

test_that("Past years are input correctly", {
  expect_error(
    pca_thresholds(SiteID = "BAND",
                   past_years = c(1950:2000),
                   pca_data = cf_pca(SiteID = "BAND",
                                     data = pca_data,
                                     directory = my_directory),
                   all_data = all_data,
                   future_year = 2040,
                   directory = my_directory),
    regexp = "You may have entered the range of years")
})

test_that("Past year reference range is valid", {
  expect_error(
    pca_thresholds(SiteID = "BAND",
                past_years = c(2020, 2030),
                pca_data = cf_pca(SiteID = "BAND",
                                  data = pca_data,
                                  directory = my_directory),
                all_data = all_data,
                future_year = 2040,
                directory = my_directory),
    regexp = "The requested period for historic values is incorrect for this function.")
})

test_that("Past year range is at least 30 years",{
  expect_error(
    pca_thresholds(SiteID = "BAND",
                past_years = c(1950, 1960),
                pca_data = cf_pca(SiteID = "BAND",
                                  data = pca_data,
                                  directory = my_directory),
                all_data = all_data,
                directory = my_directory),
    regexp = "Past year range must be at least 30 years."
  )
})

test_that("Past years entered in correct order", {
  expect_error(pca_thresholds(SiteID = "BAND",
                                 past_years = c(2000, 1950),
                                 all_data = all_data,
                                 directory = my_directory,
                                 pca_data = cf_pca(SiteID = "BAND",
                                                   data = pca_data,
                                                   directory = my_directory)),
               regexp = "Past years entered in incorrect order")
})

test_that("Future year is a single year", {
  expect_error(
    pca_thresholds(SiteID = "BAND",
                past_years = c(1950,2000),
                future_year = c(2040, 2041),
                pca_data = cf_pca(SiteID = "BAND",
                                 data = pca_data,
                                 directory = my_directory),
                all_data = all_data,
                directory = my_directory),
    regexp = "Future year should be")
})

test_that("Future year is between 2040 and 2084", {
  expect_error(
    pca_thresholds(SiteID = "BAND",
                   past_years = c(1950,2000),
                   future_year = 2030,
                   pca_data = cf_pca(SiteID = "BAND",
                                     data = pca_data,
                                     directory = my_directory),
                   all_data = all_data,
                   directory = my_directory),
    regexp = "Future year can only be")
})

test_that("Summarize by is only month, season or year", {
  expect_error(
    pca_thresholds(SiteID = "BAND",
                   summarize_by = "canyon",
                   past_years = c(1950,2000),
                   future_year = 2040,
                   all_data = all_data,
                   directory = my_directory,
                   pca_data = cf_pca(SiteID = "BAND",
                                     data = pca_data,
                                     directory = my_directory)),
    regexp = "summarize_by can only be month")
})


test_that("tempdir() gives warning", {
  skip_on_cran()
  expect_warning(pca_thresholds(SiteID = "BAND",
                                pca_data = cf_pca(SiteID = "BAND",
                                                 data = pca_data,
                                                 directory = my_directory),
                                all_data = all_data))
})
