
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
#' @param summarize_by how to summarize the data, options are "month", "season", "year"
#'
#' @return three .csv files:
#' 1.
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

data <- readr::read_csv("BAND_thresholds.csv")
year = 2040

method = "corners"
SiteID = "BAND"
summarize_by = "month"

cf_quadrant <- function(SiteID, data = NULL, year, method = NULL, summarize_by = NULL){

  rh_exists <-  exists("rhmin", where = data)

  # ---------
  #subset data for future to be 30 years around focus year
  # ---------

  start_year <- year - 15
  end_year <- year + 15

  future_all <- data %>%
    dplyr::filter(.data$yr %in% c(start_year:end_year))

  past_all <- data %>%
    dplyr::filter(.data$yr < 2006) %>%
    dplyr::mutate(cf = "Historical")

  # ---------
  # create means for past and future
  # ---------

past_mean <- past_all %>%
    dplyr::group_by(.data$gcm) %>%
    dplyr::summarize(precip_mean_p = mean(.data$precip),
                   # data source needs to be specified using rlang::.data
                   # error `no visible binding for global variable x` will be thrown
                   tmax_mean_p = mean(.data$tmax),
                   tmin_mean_p = mean(.data$tmin),
                   tavg_mean_p = (.data$tmax_mean_p + .data$tmin_mean_p) / 2)

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
  dplyr::summarize(gcm = unique(.data$gcm),
                   precip_change = .data$precip_mean_f - past_mean$precip_mean_p,
                   tmax_change = .data$tmax_mean_f - past_mean$tmax_mean_p,
                   tmin_change = .data$tmin_mean_f - past_mean$tmin_mean_p,
                   tavg_change = .data$tavg_mean_f - past_mean$tavg_mean_p)


# -------------------------------
# # # # CALCULATE FUTRE MEANS # # # #
# -------------------------------


# ---------
# set quadrant variables
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

quadrant_df <- change %>%
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
# # # # # QUADRANT METHOD # # # #
# -----------

if(method == "quadrant"){

  # this will output the scatterplot data

  readr::write_csv(quadrant_df, paste0(SiteID,"_future_means_q.csv"))

  # future means = total mean centered on year selected, one for each model and rcp


}#close if for method quadrant


# ------------
# # # # CORNERS METHOD # # # #
# ------------


if(method == "corners"){
  # this will output the scatterplot data

  # -----
  # create function for euclidean distance
  # -----

  euclidean <- function(a, b, c, d) sqrt(((a - b)^2 + (c - d)^2))

  # -----
  # calculate euc distance from each corner
  # -----

  corners_df <- quadrant_df %>%
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
      TRUE ~ NA_character_
    )) %>%
    dplyr::select(gcm, precip_change, tmax_change, tmin_change, tavg_change, cf, corner)

readr::write_csv(corners_df, paste0(SiteID, "_future_means_c.csv"))

# future means = total mean centered on year selected, one for each model and rcp

}#close if for method corners



# -----------------
# # # # SUMMARIZE THRESHOLD VALUES # # # #
# -----------------


#--------------
# QUADRANT
# -------------

# get the number of gcms in each quadrant

num_gcm <- quadrant_df %>%
  dplyr::summarise(`Warm Wet` = sum(stringr::str_count(.data$cf, "Warm Wet")),
                   `Warm Dry` = sum(stringr::str_count(.data$cf, "Warm Dry")),
                   `Hot Wet` = sum(stringr::str_count(.data$cf, "Hot Wet")),
                   `Hot Dry` = sum(stringr::str_count(.data$cf, "Hot Dry")),
                   Central = sum(stringr::str_count(.data$cf, "Central"))) %>%
  tidyr::pivot_longer(`Warm Wet`:Central,
                      names_to = "cf",
                      values_to = "num_of_gcms")

# break into if statements based on method and summarize_by

# --------
# GROUP FOR MONTH and QUADRANT
# --------

if(summarize_by == "month" & method == "quadrant"){

  quadrant_cf_gcm <- quadrant_df %>%
    dplyr::select(.data$gcm, .data$cf) %>%
    dplyr::full_join(future_all, by = "gcm") %>%
    dplyr::full_join(num_gcm, by = "cf") %>%
    dplyr::group_by(month, cf)
}

# --------
# GROUP FOR SEASON and QUADRANT
# --------

if(summarize_by == "season" & method == "quadrant"){

  quadrant_cf_gcm <- quadrant_df %>%
    dplyr::select(.data$gcm, .data$cf) %>%
    dplyr::full_join(future_all, by = "gcm") %>%
    dplyr::group_by(cf, quarter)

}

# --------
# CROUP FOR YEAR and QUADRANT
# --------

if(summarize_by == "year" & method == "quadrant"){
  quadrant_cf_gcm <- quadrant_df %>%
    dplyr::select(.data$gcm, .data$cf) %>%
    dplyr::full_join(future_all, by = "gcm") %>%
    dplyr::group_by(cf, yr)
}


# -----------
# SUMMARIZE THRESHOLD VALUES FOR QUADRANT
# -----------

if(method == "quadrant"){

  method_cf_gcm <- quadrant_cf_gcm %>%
    # grouped by month/season/year and climate future
    # sums will be per month/season/year per climate future
    dplyr::summarize(heat_index = if(rh_exists == TRUE)
      mean(.data$heat_index, na.rm = TRUE),
      heat_index_ec = if(rh_exists == TRUE)
        sum(.data$heat_index_ec,
            na.rm = TRUE) / (30 * unique(.data$num_of_gcms)),
      heat_index_dan = if(rh_exists == TRUE)
        sum(.data$heat_index_dan,
            na.rm = TRUE) / (30 * unique(.data$num_of_gcms)),
      temp_over_95_pctl = sum(.data$temp_over_95_pctl,
                              na.rm = TRUE) / (30 * unique(.data$num_of_gcms)),
      temp_over_99_pctl = sum(.data$temp_over_99_pctl,
                              na.rm = TRUE) / (30 * unique(.data$num_of_gcms)),
      temp_over_95_pctl_length = max(.data$temp_over_95_pctl_length,
                                     na.rm = TRUE),
      temp_under_freeze = sum(.data$temp_under_freeze,
                              na.rm = TRUE) / (30 * unique(.data$num_of_gcms)),
      temp_under_freeze_length = max(.data$temp_under_freeze_length,
                                     na.rm = TRUE),
      temp_under_5_pctl = sum(.data$temp_under_5_pctl,
                              na.rm = TRUE) / (30 * unique(.data$num_of_gcms)),
      no_precip = sum(.data$no_precip,
                      na.rm = TRUE) / (30 * unique(.data$num_of_gcms)),
      no_precip_length = max(.data$no_precip_length, na.rm = TRUE),
      precip_95_pctl = sum(.data$precip_95_pctl,
                           na.rm = TRUE) / (30 * unique(.data$num_of_gcms)),
      precip_99_pctl = sum(.data$precip_99_pctl,
                           na.rm = TRUE) / (30 * unique(.data$num_of_gcms)),
      precip_moderate = sum(.data$precip_moderate,
                            na.rm = TRUE) / (30 * unique(.data$num_of_gcms)),
      precip_heavy = sum(.data$precip_heavy,
                         na.rm = TRUE) / (30 * unique(.data$num_of_gcms)),
      freeze_thaw = sum(.data$freeze_thaw,
                        na.rm = TRUE) / (30 * unique(.data$num_of_gcms)),
      gdd = sum(.data$gdd, na.rm = TRUE) / (30 * unique(.data$num_of_gcms)),
      gdd_count = max(.data$gdd_count, na.rm = TRUE),
      not_gdd_count = max(.data$not_gdd_count, na.rm = TRUE),
      frost = sum(.data$frost, na.rm = TRUE) / (30 * unique(.data$num_of_gcms)),
      grow_len = mean(.data$grow_len, na.rm = TRUE),
      .groups = "keep")

  # (sum of threshold value, per month/season/year, per cf)
  # divided by (30 years * number of gcms in a quadrant) =
  # average value per month/season/yr, per cf, across all years observed

  assign("method_cf_gcm", method_cf_gcm, envir = .GlobalEnv)
  # move the df to global env for later use

}


# ------------------
# CORNERS
# ------------------

# --------
# GROUP FOR MONTH and CORNERS
# --------

if(summarize_by == "month" & method == "corners"){
  corners_cf_gcm <- corners_df %>%
    dplyr::select(.data$gcm, .data$cf, .data$corner) %>%
    dplyr::full_join(future_all, by = "gcm") %>%
    dplyr::group_by(corner, month)
}

# --------
# GROUP FOR SEASON and CORNERS
# --------

if(summarize_by == "season" & method == "corners"){
  corners_cf_gcm <- corners_df %>%
    dplyr::select(.data$gcm, .data$cf, .data$corner) %>%
    dplyr::full_join(future_all, by = "gcm") %>%
    dplyr::group_by(corner, quarter)
}

# --------
# GROUP FOR YEAR and CORNERS
# --------

if(summarize_by == "year" & method == "corners"){
  corners_cf_gcm <- corners_df %>%
    dplyr::select(.data$gcm, .data$cf, .data$corner) %>%
    dplyr::full_join(future_all, by = "gcm") %>%
    dplyr::group_by(corner, yr)
}

# --------
# SUMMARIZE THRESHOLD VALUES FOR CORNERS
# --------


if(method == "corners"){

  method_cf_gcm <- corners_cf_gcm %>%
    # grouped by month/season/year and climate future
    # sums will be per month/season/year per climate future
    dplyr::summarize(heat_index = if(rh_exists == TRUE)
      mean(.data$heat_index, na.rm = TRUE),
      heat_index_ec = if(rh_exists == TRUE) sum(.data$heat_index_ec, na.rm = TRUE) / 30,
      heat_index_dan = if(rh_exists == TRUE) sum(.data$heat_index_dan, na.rm = TRUE) / 30,
      temp_over_95_pctl = sum(.data$temp_over_95_pctl, na.rm = TRUE) / 30,
      temp_over_99_pctl = sum(.data$temp_over_99_pctl, na.rm = TRUE) / 30,
      temp_over_95_pctl_length = max(.data$temp_over_95_pctl_length, na.rm = TRUE),
      temp_under_freeze = sum(.data$temp_under_freeze, na.rm = TRUE) / 30,
      temp_under_freeze_length = max(.data$temp_under_freeze_length, na.rm = TRUE),
      temp_under_5_pctl = sum(.data$temp_under_5_pctl, na.rm = TRUE) / 30,
      no_precip = sum(.data$no_precip, na.rm = TRUE) / 30,
      no_precip_length = max(.data$no_precip_length, na.rm = TRUE),
      precip_95_pctl = sum(.data$precip_95_pctl, na.rm = TRUE) / 30,
      precip_99_pctl = sum(.data$precip_99_pctl,na.rm = TRUE) / 30,
      precip_moderate = sum(.data$precip_moderate, na.rm = TRUE) / 30,
      precip_heavy = sum(.data$precip_heavy, na.rm = TRUE) / 30,
      freeze_thaw = sum(.data$freeze_thaw, na.rm = TRUE) / 30,
      gdd = sum(.data$gdd, na.rm = TRUE) / 30,
      gdd_count = max(.data$gdd_count, na.rm = TRUE),
      not_gdd_count = max(.data$not_gdd_count, na.rm = TRUE),
      frost = sum(.data$frost, na.rm = TRUE) / 30,
      grow_len = mean(.data$grow_len, na.rm = TRUE),
      .groups = "keep") %>%
    tidyr::drop_na(corner)

  # (sum of threshold value, per month/season/year, per cf)
  # divided by (30 years * number of gcms in a quadrant) =
  # average value per month/season/yr, per cf, across all years observed

  assign("method_cf_gcm", method_cf_gcm, envir = .GlobalEnv)
  # move the df to global env for later use

}


# -------------------------
# # # #  CSV CREATION # # # #
# -------------------------

if(summarize_by == "month" & method == "quadrant"){
  readr::write_csv(method_cf_gcm, paste0(SiteID, "_future_month_q.csv"))
}

if(summarize_by == "season" & method == "quadrant"){
  readr::write_csv(method_cf_gcm, paste0(SiteID, "_future_season_q.csv"))
}

if(summarize_by == "year" & method == "quadrant"){
  readr::write_csv(method_cf_gcm, paste0(SiteID, "_future_year_q.csv"))
}

if(summarize_by == "month" & method == "corners"){
  readr::write_csv(method_cf_gcm, paste0(SiteID, "_future_month_c.csv"))
}

if(summarize_by == "season" & method == "corners"){
  readr::write_csv(method_cf_gcm, paste0(SiteID, "_future_season_c.csv"))
}

if(summarize_by == "year" & method == "corners"){
  readr::write_csv(method_cf_gcm, paste0(SiteID, "_future_year_c.csv"))
}

}# close function

cf_quadrant("BAND", data = thresholds, 2040, method = "corners", summarize_by = "month")
