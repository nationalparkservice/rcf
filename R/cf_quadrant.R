#' Calculate climate futures based on their quadrant
#'
#' Designates climate futures of "Warm Wet", "Warm Dry", "Hot Wet", "Hot Dry" and "Central"
#' and calculates mean values for each GCM. Will additionally calculate the most extreme
#' of each quadrant if calculation method is "corner". Calculates summary of threshold
#' values based off of selected summarization parameter and calculation method.
#'
#' @param SiteID chosen name to use in file names, attributes, and
#'  directories. (character)
#' @param data Default data set to use for .csv creation. Must be created
#' prior to running function. Follow vignette for example data set creation (data frame)
#' @param future_year year to center changes from historical data around. Defaults
#' to 2040 (numeric)
#' @param past_years years to base past data off of. Cannot be any earlier than 1950 or later
#' 2005, due to the definition of past in the MACA v2 data. Must be written as
#' c(past_start, past_end). Defaults to 1950 to 2000 (numeric)
#' @param method method for calculating resulting .csv. Options "quadrant" and "corner".
#' "quadrant" will return a data frame in which all models are labeled
#' in their respective quadrants "hot wet" "hot dry" "warm wet" "warm dry" and "central
#' "corner" will do the same, but additionally return the model that represents
#' the most extreme scenario in each quadrant. Defaults to quadrant.
#' @param summarize_by how to summarize the data, options are "month", "season", "year".
#' Defaults to year.
#' @param directory where to save files to. Per CRAN guidelines, this
#' defaults to a temporary directory and files created will be lost after
#' R session ends. Specify a path to retain files.
#'
#' @return Two (2) .csv files:
#' 1.SiteID_future_means_x.csv - future means of all models calculated on central year
#' 2. SiteID_y_summary_x.csv - Summary of threshold values calculated based on method (x) and
#' summary metric (y). Means are taken based on month/season/year and either all models in
#' a quadrant for quadrant method, or most extreme model in each quadrant for corner method
#' x: c = corner, q = quadrant
#' y: month, season or year
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate sample data
#'
#' data <- data.frame(
#' date = sample(seq(as.Date('1950/01/01'), as.Date('2099/12/31'), by="day"), 1000),
#' yr = rep(c(1980, 2040, 1980, 2040, 1980, 2040, 1980, 2040, 1980, 2040, 1980, 2040, 1980,
#'            2040, 1980, 2040, 1980, 2040, 1980, 2040), each = 50),
#' gcm = rep(c("bcc-csm1-1.rcp45", "BNU-ESM.rcp45", "CanESM2.rcp85", "CCSM4.rcp45",
#'             "CSIRO-Mk3-6-0.rcp45"), each = 200),
#' precip = rnorm(1000),
#' tmin = rnorm(1000),
#' tmax = rnorm(1000),
#' rhmax = rnorm(1000),
#' rhmin = rnorm(1000),
#' tavg = rnorm(1000),
#' heat_index = rnorm(1000),
#' heat_index_ec = rnorm(1000),
#' heat_index_dan = rnorm(1000),
#' temp_over_95_pctl =  as.logical(sample(x = c("TRUE","FALSE"), size = 1000, replace = TRUE)),
#' temp_over_99_pctl =  as.logical(sample(x = c("TRUE","FALSE"), size = 1000, replace = TRUE)),
#' temp_over_95_pctl_length =  as.logical(sample(x = c("TRUE","FALSE"), size = 1000,
#'                                               replace = TRUE)),
#' temp_under_freeze =  as.logical(sample(x = c("TRUE","FALSE"), size = 1000, replace = TRUE)),
#' temp_under_freeze_length =  as.logical(sample(x = c("TRUE","FALSE"), size = 1000,
#'                                               replace = TRUE)),
#' temp_under_5_pctl =  as.logical(sample(x = c("TRUE","FALSE"), size = 1000, replace = TRUE)),
#' no_precip  =  as.logical(sample(x = c("TRUE","FALSE"), size = 1000, replace = TRUE)),
#' no_precip_length =  as.logical(sample(x = c("TRUE","FALSE"), size = 1000, replace = TRUE)),
#' precip_95_pctl =  as.logical(sample(x = c("TRUE","FALSE"), size = 1000, replace = TRUE)),
#' precip_99_pctl =  as.logical(sample(x = c("TRUE","FALSE"), size = 1000, replace = TRUE)),
#' precip_moderate =  as.logical(sample(x = c("TRUE","FALSE"), size = 1000, replace = TRUE)),
#' precip_heavy =  as.logical(sample(x = c("TRUE","FALSE"), size = 1000, replace = TRUE)),
#' freeze_thaw =  as.logical(sample(x = c("TRUE","FALSE"), size = 1000, replace = TRUE)),
#' gdd =  as.logical(sample(x = c("TRUE","FALSE"), size = 1000, replace = TRUE)),
#' gdd_count = rnorm(1000),
#' not_gdd_count = rnorm(1000),
#' frost = as.logical(sample(x = c("TRUE","FALSE"), size = 1000, replace = TRUE)),
#' grow_length = rnorm(1000),
#' units = rep("imperial", each = 1000)
#' )
#'
#' cf_quadrant("SCBL", data = df, future_year = 2040, past_years = c(1950, 2000),
#' method = "corner", summarize_by = "year")
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data

cf_quadrant <- function(SiteID = "unnamed_site",
                        data = NULL,
                        future_year = 2040,
                        past_years = c(1950, 2000),
                        method = "quadrant",
                        summarize_by = "year",
                        directory = tempdir()){


  rh_exists <-  any(names(data) == "rhmin")
  suppressMessages(if(!file.exists(".here")) here::set_here(directory))

  #stop create errors if people enter incorrect years
  if (any(past_years < 1950 | past_years > 2005)) {
    stop("The requested period for historic values is incorrect for this function. Years must be between 1950 and 2005")
  }

  if(length(past_years) > 2){
    stop("You may have entered the range of years as (start_year:end_year). Did you mean to write (start_year, end_year)? Vector cannot be of length greater than 2.")
  }

  if(past_years[2] - past_years[1] < 30 & past_years[1] < past_years[2]){
    stop("Past year range must be at least 30 years.")
  }

  if(past_years[1] > past_years[2]){
    stop("Past years entered in incorrect order, should be c(start_year, end_year).")
  }

  if(length(future_year) > 1){
    stop("Future year should be a single year.")
  }

  if(any(future_year < 2040 | future_year > 2084)){
    stop("Future year can only be between 2040 and 2084.")
  }

  if(method %in% c("quadrant", "corner") == FALSE){
    stop("Method can only be quadrant or corner, did you misspell?")
  }

  if(summarize_by %in% c("month", "season", "year") == FALSE){
    stop("summarize_by can only be month, season or year, did you misspell?")
  }

  # ---------
  #subset data for future to be 30 years around focus year
  # ---------


  future_start <- future_year - 15
  future_end <- future_year + 15

  past_start = past_years[1]
  past_end = past_years[2]

  future_all <- data %>%
    dplyr::filter(.data$yr %in% c(future_start:future_end)) %>%
    dplyr::mutate(time = "Future")

  past_all <- data %>%
    dplyr::filter(.data$yr %in% c(past_start:past_end)) %>%
    dplyr::mutate(time = "Historical")

  # ---------
  # create means for past and future
  # ---------

past_mean <- past_all %>%
    dplyr::group_by(.data$gcm) %>%
    dplyr::summarize(precip_mean_p = mean(.data$precip) * 365,
                   # data source needs to be specified using rlang::.data
                   # error `no visible binding for global variable x` will be thrown
                   tmax_mean_p = mean(.data$tmax),
                   tmin_mean_p = mean(.data$tmin),
                   tavg_mean_p = (.data$tmax_mean_p + .data$tmin_mean_p) / 2)

future_mean <- future_all %>%
  dplyr::group_by(.data$gcm) %>%
  dplyr::summarize(precip_mean_f = mean(.data$precip) * 365,
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



# --------------------------------------------------------
# # # # # # # # # # QUADRANT METHOD # # # # # # # # # # #
# --------------------------------------------------------



if(method == "quadrant"){

  # no mor manipulation needed for future means for quadrant
  # this will output the scatterplot data

  quadrant_df <- quadrant_df %>%
    dplyr::mutate(corner = NA_character_)

  if(!file.exists(here::here(directory,
                             paste0(SiteID,"_future_means.csv"))))readr::write_csv(quadrant_df,
                                                                                   here::here(directory,
                                           paste0(SiteID,"_future_means.csv")))

  # future means = total mean centered on year selected, one for each model and rcp

  # ----------------
  # Attach climate futures to baseline data
  # ----------------

  cf_gcm_only <- quadrant_df %>%
    dplyr::select(.data$gcm, .data$cf)

  past_all <- past_all %>%
    dplyr::full_join(cf_gcm_only, by = "gcm")

  # ------------
  # get the number of gcms in each quadrant
  # ------------


num_gcm <- quadrant_df %>%
  dplyr::summarise(`Warm Wet` = sum(stringr::str_count(.data$cf, "Warm Wet")),
                   `Warm Dry` = sum(stringr::str_count(.data$cf, "Warm Dry")),
                   `Hot Wet` = sum(stringr::str_count(.data$cf, "Hot Wet")),
                   `Hot Dry` = sum(stringr::str_count(.data$cf, "Hot Dry")),
                   Central = sum(stringr::str_count(.data$cf, "Central"))) %>%
  tidyr::pivot_longer(.data$`Warm Wet`:.data$Central,
                      names_to = "cf",
                      values_to = "num_of_gcms")


suppressMessages(quadrant_cf_gcm <- quadrant_df %>%
  dplyr::select(.data$gcm, .data$cf) %>%
  dplyr::full_join(future_all, by = "gcm") %>%
  dplyr::full_join(past_all) %>%
  dplyr::full_join(num_gcm, by = "cf"))

# break into if statements based on summarize_by

# -----------
# SUMMARIZE THRESHOLD VALUES FOR QUADRANT
# -----------

# --------
# GROUP FOR YEAR
# --------


# it is different than the others b/c it doesn't need to be divided by num yr
if(summarize_by == "year"){
  method_cf_gcm <-  quadrant_cf_gcm %>%
    dplyr::group_by(.data$cf, .data$yr, .data$time) %>%
    # grouped by month/season/year and climate future
    # sums will be per month/season/year per climate future
    dplyr::summarize(precip_yearly = mean(.data$precip) * 365,
      #mean is mean of all cfs over month/season/year
      tmin = mean(.data$tmin, na.rm = TRUE),
      tmax = mean(.data$tmax, na.rm = TRUE),
      tavg = mean(.data$tavg, na.rm = TRUE),
      rhmin = ifelse(rh_exists == TRUE, mean(.data$rhmin, na.rm = TRUE), NA_integer_),
      rhmax = ifelse(rh_exists == TRUE, mean(.data$rhmax, na.rm = TRUE), NA_integer_),
      heat_index = ifelse(rh_exists == TRUE, mean(.data$heat_index, na.rm = TRUE),
                          NA_integer_),
      heat_index_ec = ifelse(rh_exists == TRUE, sum(.data$heat_index_ec,
            na.rm = TRUE) / unique(.data$num_of_gcms), NA_integer_),
      heat_index_dan = ifelse(rh_exists == TRUE,
        sum(.data$heat_index_dan,
            na.rm = TRUE) / unique(.data$num_of_gcms), NA_integer_),
      temp_over_95_pctl = sum(.data$temp_over_95_pctl,
                              na.rm = TRUE) / unique(.data$num_of_gcms),
      temp_over_99_pctl = sum(.data$temp_over_99_pctl,
                              na.rm = TRUE) / unique(.data$num_of_gcms),
      temp_over_95_pctl_length = max(.data$temp_over_95_pctl_length,
                                     na.rm = TRUE),
      temp_under_freeze = sum(.data$temp_under_freeze,
                              na.rm = TRUE) / unique(.data$num_of_gcms),
      temp_under_freeze_length = max(.data$temp_under_freeze_length,
                                     na.rm = TRUE),
      temp_under_5_pctl = sum(.data$temp_under_5_pctl,
                              na.rm = TRUE) / unique(.data$num_of_gcms),
      no_precip = sum(.data$no_precip,
                      na.rm = TRUE) / unique(.data$num_of_gcms),
      no_precip_length = max(.data$no_precip_length, na.rm = TRUE),
      precip_95_pctl = sum(.data$precip_95_pctl,
                           na.rm = TRUE) / unique(.data$num_of_gcms),
      precip_99_pctl = sum(.data$precip_99_pctl,
                           na.rm = TRUE) / unique(.data$num_of_gcms),
      precip_moderate = sum(.data$precip_moderate,
                            na.rm = TRUE) / unique(.data$num_of_gcms),
      precip_heavy = sum(.data$precip_heavy,
                         na.rm = TRUE) / unique(.data$num_of_gcms),
      freeze_thaw = sum(.data$freeze_thaw,
                        na.rm = TRUE) / unique(.data$num_of_gcms),
      gdd = sum(.data$gdd, na.rm = TRUE) / unique(.data$num_of_gcms),
      gdd_count = max(.data$gdd_count, na.rm = TRUE),
      not_gdd_count = max(.data$not_gdd_count, na.rm = TRUE),
      frost = sum(.data$frost, na.rm = TRUE) / unique(.data$num_of_gcms),
      grow_length = mean(.data$grow_length, na.rm = TRUE),
      units = unique(units),
      .groups = "keep")
} # close summarize by year

# -------------
# SUMMARIZE THRESHOLD VALUES FOR MONTH AND SEASON
# -------------

# --------
# GROUP FOR MONTH
# --------

if(summarize_by == "month"){
  method_cf_gcm_1 <- quadrant_cf_gcm %>%
    dplyr::group_by(.data$month, .data$cf, .data$time)
  # month, climate future, past or future
}

# --------
# GROUP FOR SEASON
# --------

if(summarize_by == "season"){

  method_cf_gcm_1 <- quadrant_cf_gcm %>%
    dplyr::group_by(.data$cf, .data$quarter, .data$time)
  # climate future, season, past or future

}

if(summarize_by %in% c("month", "season")){

  num_years <- ifelse(.data$time == "Future", 30, (past_end - past_start))

  method_cf_gcm <- method_cf_gcm_1 %>%
    # grouped by month/season/year and climate future
    # sums will be per month/season/year per climate future
    dplyr::mutate(num_years = ifelse(.data$time == "Future", 30, past_end - past_start)) %>%
    dplyr::summarize(precip_monthly = mean(.data$precip, na.rm = TRUE) * 30,
      tmin = mean(.data$tmin, na.rm = TRUE),
      tmax = mean(.data$tmax, na.rm = TRUE),
      tavg = mean(.data$tavg, na.rm = TRUE),
      rhmin = ifelse(rh_exists == TRUE, mean(.data$rhmin, na.rm = TRUE), NA_integer_),
      rhmax = ifelse(rh_exists == TRUE, mean(.data$rhmax, na.rm = TRUE), NA_integer_),
      heat_index = ifelse(rh_exists == TRUE, mean(.data$heat_index, na.rm = TRUE),
                          NA_integer_),
      heat_index_ec = ifelse(rh_exists == TRUE, sum(.data$heat_index_ec,
                                                    na.rm = TRUE) /(unique(
                                                      .data$num_years) * unique(
                                                        .data$num_of_gcms)),
                             NA_integer_),
      heat_index_dan = ifelse(rh_exists == TRUE,
                              sum(.data$heat_index_dan,
                                  na.rm = TRUE) / (unique(.data$num_years) * unique(
                                    .data$num_of_gcms)), NA_integer_),
      temp_over_95_pctl = sum(.data$temp_over_95_pctl,
                              na.rm = TRUE) / (unique(.data$num_years) * unique(
                                .data$num_of_gcms)),
      temp_over_99_pctl = sum(.data$temp_over_99_pctl,
                              na.rm = TRUE) / (unique(.data$num_years) * unique(
                                .data$num_of_gcms)),
      temp_over_95_pctl_length = NA_integer_,
      temp_under_freeze = sum(.data$temp_under_freeze,
                              na.rm = TRUE) / (unique(.data$num_years) * unique(
                                .data$num_of_gcms)),
      temp_under_freeze_length = NA_integer_,
      temp_under_5_pctl = sum(.data$temp_under_5_pctl,
                              na.rm = TRUE) / (unique(
                                .data$num_years) * unique(.data$num_of_gcms)),
      no_precip = sum(.data$no_precip,
                      na.rm = TRUE) / (unique(.data$num_years) * unique(.data$num_of_gcms)),
      no_precip_length = NA_integer_,
      precip_95_pctl = sum(.data$precip_95_pctl,
                           na.rm = TRUE) / (unique(.data$num_years) * unique(
                             .data$num_of_gcms)),
      precip_99_pctl = sum(.data$precip_99_pctl,
                           na.rm = TRUE) / (unique(.data$num_years) * unique(
                             .data$num_of_gcms)),
      precip_moderate = sum(.data$precip_moderate,
                            na.rm = TRUE) / (unique(.data$num_years) * unique(
                              .data$num_of_gcms)),
      precip_heavy = sum(.data$precip_heavy,
                         na.rm = TRUE) / (unique(.data$num_years) * unique(.data$num_of_gcms)),
      freeze_thaw = sum(.data$freeze_thaw,
                        na.rm = TRUE) / (unique(.data$num_years) * unique(.data$num_of_gcms)),
      gdd = sum(.data$gdd, na.rm = TRUE) / (unique(.data$num_years) * unique(
        .data$num_of_gcms)),
      gdd_count = max(.data$gdd_count, na.rm = TRUE),
      not_gdd_count = max(.data$not_gdd_count, na.rm = TRUE),
      frost = sum(.data$frost, na.rm = TRUE) / (unique(.data$num_years) * unique(
        .data$num_of_gcms)),
      grow_length = NA_integer_,
      units = unique(units),
      .groups = "keep")

  # (sum of threshold value, per month/season/year, per cf)
  # divided by (30 years * number of gcms in a quadrant) =
  # average value per month/season/yr, per cf, across all years observed

  # assign("method_cf_gcm", method_cf_gcm, envir = .GlobalEnv)
  # move the df to global env for later use

} # close summarize by season or month
} #close method == quadrant


# --------------------------------------------------------------
# # # # # # # # # # # # # # CORNER # # # # # # # # # # # # # #
# --------------------------------------------------------------


# ------------
# # # # corner METHOD # # # #
# ------------


if(method == "corner"){
  # this will output the scatterplot data

  # -----
  # create function for euclidean distance
  # -----

  euclidean <- function(a, b, c, d) sqrt(((a - b)^2 + (c - d)^2))

  # -----
  # calculate euc distance from each corner
  # -----

  corner_df <- quadrant_df %>%
    dplyr::mutate(temp_min = min(scales::rescale(.data$tavg_change, to = c(-1,1))),
                  temp_max = max(scales::rescale(.data$tavg_change, to = c(-1,1))),
                  precip_min = min(scales::rescale(.data$precip_change, to = c(-1,1))),
                  precip_max = max(scales::rescale(.data$precip_change, to = c(-1,1)))) %>%
    dplyr::mutate(tavg_scale = scales::rescale(.data$tavg_change, to = c(-1,1)),
                  precip_scale = scales::rescale(.data$precip_change, to = c(-1,1))) %>%
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
    dplyr::select(.data$gcm, .data$precip_change, .data$tmax_change, .data$tmin_change, .data$tavg_change, .data$cf, .data$corner)



  if(!file.exists(here::here(directory,
                             paste0(SiteID,"_future_means.csv"))))readr::write_csv(corner_df,
                                                                                   here::here(directory,
                                             paste0(SiteID,"_future_means.csv")))

  # -----------------------
  # Attach climate futures to baseline data
  # ----------------

  cf_gcm_only <- corner_df %>%
    dplyr::select(.data$gcm, .data$cf, .data$corner)

  past_all <- past_all %>%
    dplyr::full_join(cf_gcm_only, by = "gcm")

  # future means = total mean centered on year selected, one for each model and rcp


  suppressMessages(corner_cf_gcm <- corner_df %>%
    dplyr::select(.data$gcm, .data$cf, .data$corner) %>%
    dplyr::full_join(future_all, by = "gcm") %>%
    dplyr::full_join(past_all))

# --------
# SUMMARIZE THRESHOLD VALUES FOR corner
# --------

# --------
# GROUP FOR YEAR and corner
# --------

if(summarize_by == "year"){
  method_cf_gcm <- corner_cf_gcm %>%
    dplyr::group_by(.data$corner, .data$yr, .data$time) %>%
    dplyr::filter(.data$corner %in% c("Hot Wet", "Hot Dry", "Warm Wet", "Warm Dry")) %>%
    dplyr::summarize(gcm = unique(.data$gcm),
      cf = unique(.data$cf),
      precip_yearly = mean(.data$precip, na.rm = TRUE) * 365,
      tmin = mean(.data$tmin, na.rm = TRUE),
      tmax = mean(.data$tmax, na.rm = TRUE),
      tavg = mean(.data$tavg, na.rm = TRUE),
      rhmin = ifelse(rh_exists == TRUE, mean(.data$rhmin, na.rm = TRUE), NA_integer_),
      rhmax = ifelse(rh_exists == TRUE, mean(.data$rhmax, na.rm = TRUE), NA_integer_),
      heat_index = ifelse(rh_exists == TRUE, mean(.data$heat_index, na.rm = TRUE),
                          NA_integer_),
      heat_index_ec = ifelse(rh_exists == TRUE, sum(.data$heat_index_ec, na.rm = TRUE),
                             NA_integer_),
      heat_index_dan = ifelse(rh_exists == TRUE,
                              sum(.data$heat_index_dan, na.rm = TRUE), NA_integer_),
      temp_over_95_pctl = sum(.data$temp_over_95_pctl, na.rm = TRUE),
      temp_over_99_pctl = sum(.data$temp_over_99_pctl, na.rm = TRUE),
      temp_over_95_pctl_length = max(.data$temp_over_95_pctl_length, na.rm = TRUE),
      temp_under_freeze = sum(.data$temp_under_freeze, na.rm = TRUE),
      temp_under_freeze_length = max(.data$temp_under_freeze_length, na.rm = TRUE),
      temp_under_5_pctl = sum(.data$temp_under_5_pctl, na.rm = TRUE),
      no_precip = sum(.data$no_precip, na.rm = TRUE),
      no_precip_length = max(.data$no_precip_length, na.rm = TRUE),
      precip_95_pctl = sum(.data$precip_95_pctl, na.rm = TRUE),
      precip_99_pctl = sum(.data$precip_99_pctl,na.rm = TRUE),
      precip_moderate = sum(.data$precip_moderate, na.rm = TRUE),
      precip_heavy = sum(.data$precip_heavy, na.rm = TRUE),
      freeze_thaw = sum(.data$freeze_thaw, na.rm = TRUE),
      gdd = sum(.data$gdd, na.rm = TRUE),
      gdd_count = max(.data$gdd_count, na.rm = TRUE),
      not_gdd_count = max(.data$not_gdd_count, na.rm = TRUE),
      frost = sum(.data$frost, na.rm = TRUE),
      grow_length = mean(.data$grow_length, na.rm = TRUE),
      units = unique(units),
      .groups = "keep")

}


# ---------------
# SUMMARIZE FOR corner AND MONTH OR SEASON
# ---------------

  # --------
  # GROUP FOR MONTH
  # --------

  if(summarize_by == "month"){
    method_cf_gcm_1 <- corner_cf_gcm %>%
      dplyr::group_by(.data$corner, .data$month, .data$time)
  }

  # --------
  # GROUP FOR SEASON and corner
  # --------

  if(summarize_by == "season"){
    method_cf_gcm_1 <- corner_cf_gcm %>%
      dplyr::group_by(.data$corner, .data$quarter, .data$time)
  }

if(summarize_by %in% c("month", "season")){

  method_cf_gcm <- method_cf_gcm_1 %>%
    # grouped by month/season and climate future
    # sums will be per month/season per climate future
    dplyr::mutate(num_years = ifelse(.data$time == "Future", 30, past_end - past_start)) %>%
    dplyr::filter(.data$corner %in% c("Hot Wet", "Hot Dry", "Warm Wet", "Warm Dry")) %>%
    dplyr::summarize(gcm = unique(.data$gcm),
      cf = unique(.data$cf),
      precip_monthly = mean(.data$precip, na.rm = TRUE)  * 30,
      tmin = mean(.data$tmin, na.rm = TRUE),
      tmax = mean(.data$tmax, na.rm = TRUE),
      tavg = mean(.data$tavg, na.rm = TRUE),
      rhmin = ifelse(rh_exists == TRUE, mean(.data$rhmin, na.rm = TRUE), NA_integer_),
      rhmax = ifelse(rh_exists == TRUE, mean(.data$rhmax, na.rm = TRUE), NA_integer_),
      heat_index = ifelse(rh_exists == TRUE, mean(.data$heat_index, na.rm = TRUE),
                          NA_integer_),
      heat_index_ec = ifelse(rh_exists == TRUE, sum(.data$heat_index_ec, na.rm = TRUE) / unique(.data$num_years),
                             NA_integer_),
      heat_index_dan = ifelse(rh_exists == TRUE,
                              sum(.data$heat_index_dan, na.rm = TRUE) / unique(.data$num_years), NA_integer_),
      temp_over_95_pctl = sum(.data$temp_over_95_pctl, na.rm = TRUE) / unique(.data$num_years),
      temp_over_99_pctl = sum(.data$temp_over_99_pctl, na.rm = TRUE) / unique(.data$num_years),
      temp_over_95_pctl_length = NA_integer_,
      temp_under_freeze = sum(.data$temp_under_freeze, na.rm = TRUE) / unique(.data$num_years),
      temp_under_freeze_length = NA_integer_,
      temp_under_5_pctl = sum(.data$temp_under_5_pctl, na.rm = TRUE) / unique(.data$num_years),
      no_precip = sum(.data$no_precip, na.rm = TRUE) / unique(.data$num_years),
      no_precip_length = NA_integer_,
      precip_95_pctl = sum(.data$precip_95_pctl, na.rm = TRUE) / unique(.data$num_years),
      precip_99_pctl = sum(.data$precip_99_pctl,na.rm = TRUE) / unique(.data$num_years),
      precip_moderate = sum(.data$precip_moderate, na.rm = TRUE) / unique(.data$num_years),
      precip_heavy = sum(.data$precip_heavy, na.rm = TRUE) / unique(.data$num_years),
      freeze_thaw = sum(.data$freeze_thaw, na.rm = TRUE) / unique(.data$num_years),
      gdd = sum(.data$gdd, na.rm = TRUE) / unique(.data$num_years),
      gdd_count = max(.data$gdd_count, na.rm = TRUE),
      not_gdd_count = max(.data$not_gdd_count, na.rm = TRUE),
      frost = sum(.data$frost, na.rm = TRUE) / unique(.data$num_years),
      grow_length = NA_integer_,
      units = unique(units),
      .groups = "keep")

  # (sum of threshold value, per month/season/year, per cf)
  # divided by (number of years) =
  # average value per month/season/yr, per cf, across all years observed

}

} # close for method == corner


# Damp/Dry differentiation

change_precip_dry <- change %>%
  dplyr::full_join(cf_gcm_only, by = "gcm") %>%
  dplyr::filter(.data$cf %in% c("Hot Dry", "Warm Dry")) #select for only "dry" observations

mean_change_precip_dry <- mean(change_precip_dry$precip_change) > 0 #is dry > 0?

if(mean_change_precip_dry == "TRUE"){

  method_cf_gcm <- method_cf_gcm %>%
  dplyr::mutate(cf = stringr::str_replace(.data$cf, "Dry", "Damp"))#replace dry w/ damp

}

# -------------------------
# # # #  CSV CREATION # # # #
# -------------------------

#if directory isn't temp, save to local file
# if it is temp, give warning and save to temp directory

if(directory == tempdir()){warning("Files have been saved to temporary directory and will be deleted when this R session is closed. To save locally, input a local directory in which to save files into the `directory` argument.")}

method_cf_gcm$time <- factor(method_cf_gcm$time, levels = c("Historical", "Future"))

if(method == "quadrant"){

  readr::write_csv(method_cf_gcm, here::here(directory,
                                             paste0(SiteID, ifelse(summarize_by == "month",
                                                                   "_month_summary_q.csv",
                                                                   ifelse(summarize_by == "season",
                                                                          "_season__summary_q.csv",
                                                                          "_year_summary_q.csv")))))
  }

if(method == "corner"){
  readr::write_csv(method_cf_gcm, here::here(directory,
                                             paste0(SiteID, ifelse(summarize_by == "month",
                                                                   "_month_summary_c.csv",
                                                                   ifelse(summarize_by == "season",
                                                                          "_season_summary_c.csv",
                                                                          "_year_summary_c.csv")))))
}

return(method_cf_gcm)

}# close function
