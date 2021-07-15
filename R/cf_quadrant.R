
#' Calculate climate futures based on their quadrant
#'
#' @param SiteID chosen name to use in file names, attributes, and
#'  directories. (character)
#' @param data Default data set to use for .csv creation. Must be created
#' prior to running function. Follow vignette for example data set creation (data frame)
#' @param year year to center changes from historical data around (numeric)
#' @param method method for calculating resulting .csv. Options "quadrant" and "corners".
#' "quadrant" will return a data frame in which all models are labeled
#' in their respective quadrants "hot wet" "hot dry" "warm wet" "warm dry" and "central
#' "corners" will do the same, but additionally return the model that represents
#' the most extreme scenario in each quadrant
#'
#' @return three .csv files:
#' 1. SiteID_daily_past.csv - past data by model and day, gcm is historical
#' 2. SiteID_future_means_x.csv - future means of all models calculated on central year
#' 3' SiteID_daily_future_x.csv - future data by model and day, future quadrants labeled
#' _x: c = corner, q = quadrant
#' @export
#'
#' @examples
#' # Generate sample data
#'
#' df <- data.frame(
#' date = sample(seq(as.Date('1950/01/01'), as.Date('2099/12/31'), by="day"), 100),
#' yr = sample(seq(as.Date('1950/01/01'), as.Date('2099/12/31'), by="year"), 100),
#' gcm = paste0(rep(letters[1:5], each = 20), rep(letters[1:20], each = 5), rep(letters[20:26], each = 1)),
#' precip = rnorm(100),
#' tmin = rnorm(100),
#' tmax = rnorm(100),
#' rhmax = rnorm(100),
#' rhmin = rnorm(100),
#' tavg = rnorm(100)
#' )
#'
#' cf_quadrant("SCBL", data = df, year = 2040, method = "corners", summarize_by = "year")
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data

cf_quadrant <- function(SiteID, data = NULL, year, method = NULL, summarize_by = NULL, units = "imperial"){


  # ---------
  #subset data for future to be 30 years around focus year
  # ---------

  start_year <- year - 15
  end_year <- year + 15

  future_all <- data %>%
    dplyr::filter(.data$yr %in% c(start_year:end_year)) %>%
    dplyr::select(.data$date, .data$yr, .data$gcm, .data$precip, .data$tmax, .data$tmin, .data$rhmax, .data$rhmin, .data$tavg)

  baseline_all <- data %>%
    dplyr::filter(.data$yr < 2006) %>%
    dplyr::select(.data$date, .data$yr, .data$gcm, .data$precip, .data$tmax, .data$tmin, .data$rhmax, .data$rhmin, .data$tavg) %>%
    dplyr::mutate(cf = "Historical")

  readr::write_csv(baseline_all, here::here(SiteID,
                                            paste0(SiteID,"_daily_past.csv")))

  # ---------
  # create means for baseline and future
  # ---------

baseline_mean <- baseline_all %>%
    dplyr::group_by(.data$gcm) %>%
    dplyr::summarize(precip_mean_b = mean(.data$precip),
                   # data source needs to be specified using rlang::.data
                   # error `no visible binding for global variable x` will be thrown
                   tmax_mean_b = mean(.data$tmax),
                   tmin_mean_b = mean(.data$tmin),
                   tavg_mean_b = (.data$tmax_mean_b + .data$tmin_mean_b) / 2)

future_mean <- future_all %>%
  dplyr::group_by(.data$gcm) %>%
  dplyr::summarize(precip_mean_f = mean(.data$precip),
                   tmax_mean_f = mean(.data$tmax),
                   tmin_mean_f = mean(.data$tmin),
                   tavg_mean_f = (.data$tmax_mean_f + .data$tmin_mean_f) / 2)
# ---------
# make change variables
# ---------

change <- future_mean %>%
  dplyr::summarize(precip_change = .data$precip_mean_f - baseline_mean$precip_mean_b,
                   tmax_change = .data$tmax_mean_f - baseline_mean$tmax_mean_b,
                   tmin_change = .data$tmin_mean_f - baseline_mean$tmin_mean_b,
                   tavg_change = .data$tavg_mean_f - baseline_mean$tavg_mean_b,
                   gcm = unique(.data$gcm))

# ---------
# set quantile variables
# ---------

Pr0 = as.numeric(stats::quantile(change$precip_change, 0))
Pr25 = as.numeric(stats::quantile(change$precip_change, 0.25))
PrAvg = as.numeric(mean(change$precip_change))
Pr75 = as.numeric(stats::quantile(change$precip_change, 0.75))
Pr100 = as.numeric(stats::quantile(change$precip_change, 1))
Tavg0 = as.numeric(stats::quantile(change$tavg_change, 0))
Tavg25 = as.numeric(stats::quantile(change$tavg_change, 0.25))
Tavg = as.numeric(mean(change$tavg_change))
Tavg75 = as.numeric(stats::quantile(change$tavg_change, 0.75))
Tavg100 = as.numeric(stats::quantile(change$tavg_change, 1))

# ---------
# calculate climate futures based on model
# ---------

quantiles <- change %>%
  dplyr::mutate(cf = dplyr::case_when(
    change$tavg_change<Tavg & change$precip_change>Pr75 ~ "Warm Wet",
    change$tavg_change<Tavg25 & change$precip_change>PrAvg ~ "Warm Wet",
    change$tavg_change>Tavg & change$precip_change>Pr75 ~ "Hot Wet",
    change$tavg_change>Tavg75 & change$precip_change>PrAvg ~ "Hot Wet",
    (change$tavg_change>Tavg25 & change$tavg_change<Tavg75) & (change$precip_change>Pr25 & change$precip_change<Pr75) ~"Central",
    change$tavg_change<Tavg & change$precip_change<Pr25 ~ "Warm Dry",
    change$tavg_change<Tavg25 & change$precip_change<PrAvg ~ "Warm Dry",
    TRUE ~ "Hot Dry"
  ))

# -----------
# quadrant method
# this will output the scatterplot data and the daily_future data
# -----------

if(method == "quadrant"){

  readr::write_csv(quantiles, here::here(SiteID,
                                         paste0(SiteID,"_future_means_q.csv")))

  quadrant_cf_gcm <- quantiles %>%
    dplyr::select(.data$gcm, .data$cf) %>%
    dplyr::full_join(future_all, by = "gcm")

  readr::write_csv(quadrant_cf_gcm, here::here(SiteID,
                                               paste0(SiteID, "_daily_future_q.csv")))

}#close if for method quadrant

if(method == "corners"){

  euclidean <- function(a, b, c, d) sqrt(((a - b)^2 + (c - d)^2))

  corners_df <- quantiles %>%
    dplyr::mutate(temp_min = min(scales::rescale(.data$tavg_change, to = c(-1,1))),
                  temp_max = max(scales::rescale(.data$tavg_change, to = c(-1,1))),
                  precip_min = min(scales::rescale(.data$precip_change, to = c(-1,1))),
                  precip_max = max(scales::rescale(.data$precip_change, to = c(-1,1)))) %>%
    dplyr::mutate(tavg_scale = scales::rescale(.data$tavg_change, to = c(-1,1)),
                  precip_scale = scales::rescale(.data$precip_change)) %>%
    dplyr::mutate(euc_warm_wet = euclidean(.data$tavg_scale,
                                           .data$temp_min,
                                           .data$precip_scale,
                                           .data$precip_max),
                  # upper left hand corner, max precip, min temp
                  euc_warm_dry = euclidean(.data$tavg_scale,
                                           .data$temp_min,
                                           .data$precip_scale,
                                           .data$precip_min),
                  # lower left hand corner, min precip, min temp
                  euc_hot_wet = euclidean(.data$tavg_scale,
                                          .data$temp_max,
                                          .data$precip_scale,
                                          .data$precip_max),
                  # upper right hand corner, max precip, max temp
                  euc_hot_dry = euclidean(.data$tavg_scale,
                                          .data$temp_max,
                                          .data$precip_scale,
                                          .data$precip_min)
                  # lower right hand corner, min precip, max temp
                  ) %>%
    dplyr::mutate(corner = dplyr::case_when(
      euc_warm_wet == min(euc_warm_wet) ~ "Warm Wet",
      euc_warm_dry == min(euc_warm_dry) ~ "Warm Dry",
      euc_hot_wet == min(euc_hot_wet) ~ "Hot Wet",
      euc_hot_dry == min(euc_hot_dry) ~ "Hot Dry",
      TRUE ~ "NA"
    )) %>%
    dplyr::select(.data$precip_change, .data$tmax_change, .data$tmin_change, .data$tavg_change, .data$gcm, .data$cf, .data$corner)

readr::write_csv(corners_df, here::here(SiteID,
                                        paste0(SiteID, "_future_means_c.csv")))

cf_gcm <- corners_df %>%
  dplyr::select(.data$gcm, .data$cf)

corner_cf_gcm <- corners_df %>%
  dplyr::select(.data$gcm, .data$corner) %>%
  dplyr::full_join(cf_gcm, by = "gcm") %>%
  dplyr::full_join(future_all, by = "gcm") %>%
  dplyr::select(.data$precip_change, .data$tmax_change, .data$tmin_change, .data$tavg_change, .data$gcm, .data$cf, .data$corner)

readr::write_csv(corner_cf_gcm, here::here(SiteID,
                                           paste0(SiteID, "_daily_future_c.csv")))

}#close if for method corners

}# close function
