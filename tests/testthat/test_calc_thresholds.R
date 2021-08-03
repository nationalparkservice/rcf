test_that()

test_that("Past years are input correctly", {
  expect_error(
    calc_thresholds(SiteID = "YELL",
                    data = data,
                    past_years = c(1950:2000),
                    directory = tempdir()),
    regexpr = "The years entered are separated by a colon. Please write years formated as c(past_start, past_end).")
})

test_that("Past year reference range is valid", {
  expect_error(
    calc_thresholds(SiteID = "YELL",
                    data = data,
                    past_years = c(2020, 2030),
                    directory = tempdir()),
    regexpr = "Years are not representative of a historic timeframe.")
})
