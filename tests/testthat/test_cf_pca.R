data_for_pca <- readr::read_csv(system.file("extdata", "BAND_test_pca_summary.csv", package = "rcf"))

data <- readr::read_csv(system.file("extdata","BAND_small.csv", package = "rcf"))

my_directory <- here::here()

test_that("Function results in a dataframe", {
  skip_on_cran()
  pca_means <- cf_pca(SiteID = "BAND",
                               data = data_for_pca,
                               variables = "all_threshold",
                               num_cf = 4,
                               directory = my_directory)
  expect_s3_class(pca_means, "data.frame")
})

test_that("num_cf is 2 or 4", {
  expect_error(cf_pca(SiteID = "BAND",
                      data = data_for_pca,
                      variables = "all_threshold",
                      num_cf = 3,
                      directory = my_directory))
})

testthat::test_that("Variable names match column names", {
  testthat::expect_error(
    cf_pca(SiteID = "BAND",
           data = data,
           variables = c("cat", "dog"),
           directory = my_directory))
})


test_that("tempdir() gives warning", {
  expect_warning(cf_pca(SiteID = "BAND",
                        data = data_for_pca,
                        variables = "all_threshold"))
})
