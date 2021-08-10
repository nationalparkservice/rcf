#' Summarize threshold values to allocate climate futures by PCA*
#'
#' Takes data from `calc_thresholds` and summarizes the thresholds to be used to find
#' most extreme climate futures from principal components analysis (PCA)
#'
#' *For advanced users only.
#'
#' @param SiteID chosen name to use in file names, attributes, and
#'  directories. (character)
#' @param data Default dataset to use for the .csv files this function will create.
#' Follow vignette for example dataset creation. This should be the output of
#' the `calc_thresholds` function (data frame)
#' @param past_years years to base past data off of. Cannot be any earlier than 1950.
#' Must be written as c(past_start, past_end). Defaults to 1950:2000 (numeric)
#' @param future_year year to center changes from historical data around. Defaults to
#' 2040 (numeric)
#' @param directory where to save files to. Per CRAN guidelines, this
#' defaults to a temporary directory and files created will be lost after
#' R session ends. Specify a path to retain files.
#'
#' @return
#' A csv that has summarized threshold values across all years for all models.
#' This csv can be directly used to calculate model selection by pca using the `cf_pca`
#' function.
#' @export
#'
#' @examples
#'
#' @examples
#'
#' \dontrun{
#'
#' # Generate sample data
#'
#' data <- data.frame(
#' date = sample(seq(as.Date('1950/01/01'), as.Date('2099/12/31'), by="day"), 100),
#' yr = rep(c(1960, 1970, 1980, 1990, 2000, 2010, 2020, 2030, 2040, 2050), each = 10),
#' month = rep(c(1:10), each = 10),
#' quarter = rep(rep(c("DJF", "MAM", "JJA", "SON"), each = 25)),
#' gcm = rep(c("bcc-csm1-1.rcp45", "BNU-ESM.rcp45", "CanESM2.rcp85", "CCSM4.rcp45",
#' "CSIRO-Mk3-6-0.rcp45"), each = 20),
#' precip = rnorm(100),
#' tmin = rnorm(100),
#' tmax = rnorm(100),
#' rhmax = rnorm(100),
#' rhmin = rnorm(100),
#' tavg = rnorm(100),
#' heat_index = rnorm(100),
#' heat_index_ec = rnorm(100),
#' heat_index_dan = rnorm(100),
#' temp_over_95_pctl =  sample(x = c("TRUE","FALSE"), size = 100, replace = TRUE),
#' temp_over_99_pctl =  sample(x = c("TRUE","FALSE"), size = 100, replace = TRUE),
#' temp_over_95_pctl_length =  sample(x = c("TRUE","FALSE"), size = 100, replace = TRUE),
#' temp_under_freeze =  sample(x = c("TRUE","FALSE"), size = 100, replace = TRUE),
#' temp_under_freeze_length =  sample(x = c("TRUE","FALSE"), size = 100, replace = TRUE),
#' temp_under_5_pctl =  sample(x = c("TRUE","FALSE"), size = 100, replace = TRUE),
#' no_precip  =  sample(x = c("TRUE","FALSE"), size = 100, replace = TRUE),
#' no_precip_length =  sample(x = c("TRUE","FALSE"), size = 100, replace = TRUE),
#' precip_95_pctl =  sample(x = c("TRUE","FALSE"), size = 100, replace = TRUE),
#' precip_99_pctl =  sample(x = c("TRUE","FALSE"), size = 100, replace = TRUE),
#' precip_moderate =  sample(x = c("TRUE","FALSE"), size = 100, replace = TRUE),
#' precip_heavy =  sample(x = c("TRUE","FALSE"), size = 100, replace = TRUE),
#' freeze_thaw =  sample(x = c("TRUE","FALSE"), size = 100, replace = TRUE),
#' gdd =  sample(x = c("TRUE","FALSE"), size = 100, replace = TRUE),
#' gdd_count = rnorm(100),
#' not_gdd_count = rnorm(100),
#' frost = sample(x = c("TRUE","FALSE"), size = 100, replace = TRUE),
#' grow_length = rnorm(100),
#' units = rep("imperial", each = 100)
#' )
#'
#' summarize_for_pca("SCBL", data = data, 2040)
#'}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data

summarize_for_pca <- function(SiteID,
                        data = NULL,
                        past_years = c(1950, 2000),
                        future_year = 2040,
                        directory = tempdir()){

  #create stops if data entered incorrectly

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


  # ---------
  #subset data for future to be 30 years around focus year
  # ---------

  rh_exists <-  any(names(data) == "rhmin")
  suppressMessages(if(!file.exists(".here")) here::set_here(directory))


  future_start <- future_year - 15
  future_end <- future_year + 15

  past_start <- past_years[1]
  past_end <- past_years[2]


  future_all <- data %>%
    dplyr::filter(.data$yr %in% c(future_start:future_end)) %>%
    dplyr::mutate(time = "Future")

  past_all <- data %>%
    dplyr::filter(.data$yr %in% c(past_start:past_end)) %>%
    dplyr::mutate(time = "Historical")


  # ---------
  # Find observations for past and future
  # ---------

  past_change <- past_all %>%
    dplyr::group_by(.data$gcm) %>%
    dplyr::mutate(num_years = past_end - past_start) %>%
    dplyr::summarize(precip_mean_p = mean(.data$precip),
                     # data source needs to be specified using rlang::.data
                     # error `no visible binding for global variable x` will be thrown
                     tmax_mean_p = mean(.data$tmax),
                     tmin_mean_p = mean(.data$tmin),
                     tavg_mean_p = (.data$tmax_mean_p + .data$tmin_mean_p) / 2,
                     rhmin_mean_p = mean(.data$rhmin),
                     rhmax_mean_p = mean(.data$rhmax),
                     heat_index_p = ifelse(rh_exists == TRUE,
                                           mean(.data$heat_index, na.rm = TRUE),
                                           NA_integer_),
                     heat_index_ec_p = ifelse(rh_exists == TRUE,
                                            sum(.data$heat_index_ec,
                                                na.rm = TRUE) / unique(.data$num_years),
                                                NA_integer_),
                     heat_index_dan_p = ifelse(rh_exists == TRUE,
                                               sum(.data$heat_index_dan,
                                                   na.rm = TRUE) / unique(.data$num_years),
                                                   NA_integer_),
                     temp_over_95_pctl_p = sum(.data$temp_over_95_pctl,
                                             na.rm = TRUE) / unique(.data$num_years),
                     temp_over_99_pctl_p = sum(.data$temp_over_99_pctl,
                                             na.rm = TRUE) / unique(.data$num_years),
                     temp_over_95_pctl_length_p = max(.data$temp_over_95_pctl_length,
                                                    na.rm = TRUE),
                     temp_under_freeze_p = sum(.data$temp_under_freeze,
                                             na.rm = TRUE) / unique(.data$num_years),
                     temp_under_freeze_length_p = max(.data$temp_under_freeze_length,
                                                    na.rm = TRUE),
                     temp_under_5_pctl_p = sum(.data$temp_under_5_pctl,
                                             na.rm = TRUE) / unique(.data$num_years),
                     no_precip_p = sum(.data$no_precip,
                                       na.rm = TRUE) / unique(.data$num_years),
                     no_precip_length_p = max(.data$no_precip_length, na.rm = TRUE),
                     precip_95_pctl_p = sum(.data$precip_95_pctl,
                                          na.rm = TRUE) / unique(.data$num_years),
                     precip_99_pctl_p = sum(.data$precip_99_pctl,
                                          na.rm = TRUE) / unique(.data$num_years),
                     precip_moderate_p = sum(.data$precip_moderate,
                                           na.rm = TRUE) / unique(.data$num_years),
                     precip_heavy_p = sum(.data$precip_heavy,
                                        na.rm = TRUE) / unique(.data$num_years),
                     freeze_thaw_p = sum(.data$freeze_thaw,
                                       na.rm = TRUE) / unique(.data$num_years),
                     gdd_p = sum(.data$gdd, na.rm = TRUE) / unique(.data$num_years),
                     frost_p = sum(.data$frost, na.rm = TRUE) / unique(.data$num_years),
                     grow_length_p = mean(.data$grow_length, na.rm = TRUE),
                     units = unique(units))

  future_change <- future_all %>%
    dplyr::group_by(.data$gcm) %>%
    dplyr::summarize(precip_mean_f = mean(.data$precip),
                     tmax_mean_f = mean(.data$tmax),
                     tmin_mean_f = mean(.data$tmin),
                     tavg_mean_f = (.data$tmax_mean_f + .data$tmin_mean_f) / 2,
                     rhmin_mean_f = mean(.data$rhmin),
                     rhmax_mean_f = mean(.data$rhmax),
                     heat_index_f = ifelse(rh_exists == TRUE,
                                           mean(.data$heat_index, na.rm = TRUE),
                                           NA_integer_),
                     heat_index_ec_f = ifelse(rh_exists == TRUE,
                                              sum(.data$heat_index_ec,
                                                  na.rm = TRUE) / 30,
                                              NA_integer_),
                     heat_index_dan_f = ifelse(rh_exists == TRUE,
                                               sum(.data$heat_index_dan,
                                                   na.rm = TRUE) / 30,
                                               NA_integer_),
                     temp_over_95_pctl_f = sum(.data$temp_over_95_pctl,
                                               na.rm = TRUE) / 30,
                     temp_over_99_pctl_f = sum(.data$temp_over_99_pctl,
                                               na.rm = TRUE) / 30,
                     temp_over_95_pctl_length_f = max(.data$temp_over_95_pctl_length,
                                                      na.rm = TRUE),
                     temp_under_freeze_f = sum(.data$temp_under_freeze,
                                               na.rm = TRUE) / 30,
                     temp_under_freeze_length_f = max(.data$temp_under_freeze_length,
                                                      na.rm = TRUE),
                     temp_under_5_pctl_f = sum(.data$temp_under_5_pctl,
                                               na.rm = TRUE) / 30,
                     no_precip_f = sum(.data$no_precip,
                                       na.rm = TRUE) / 30,
                     no_precip_length_f = max(.data$no_precip_length, na.rm = TRUE),
                     precip_95_pctl_f = sum(.data$precip_95_pctl,
                                            na.rm = TRUE) / 30,
                     precip_99_pctl_f = sum(.data$precip_99_pctl,
                                            na.rm = TRUE) / 30,
                     precip_moderate_f = sum(.data$precip_moderate,
                                             na.rm = TRUE) / 30,
                     precip_heavy_f = sum(.data$precip_heavy,
                                          na.rm = TRUE) / 30,
                     freeze_thaw_f = sum(.data$freeze_thaw,
                                         na.rm = TRUE) / 30,
                     gdd_f = sum(.data$gdd, na.rm = TRUE) / 30,
                     frost_f = sum(.data$frost, na.rm = TRUE) / 30,
                     grow_length_f = mean(.data$grow_length, na.rm = TRUE),
                     units = unique(units))
  # ------------
  # CALCULATE THREHOLDS
  # ------------

  change <- future_change %>%
    dplyr::summarize(gcm = unique(.data$gcm),
                     units = unique(units),
                     precip_change = .data$precip_mean_f - past_change$precip_mean_p,
                     tmax_change = .data$tmax_mean_f - past_change$tmax_mean_p,
                     tmin_change = .data$tmin_mean_f - past_change$tmin_mean_p,
                     tavg_change = .data$tavg_mean_f - past_change$tavg_mean_p,
                     rhmin_change = .data$rhmin_mean_f  - past_change$rhmin_mean_p,
                     rhmax_change = .data$rhmax_mean_f  - past_change$rhmax_mean_p,
                     heat_index_change = .data$heat_index_f - past_change$heat_index_p,
                     heat_index_ec_change = .data$heat_index_ec_f -
                       past_change$heat_index_ec_p,
                     heat_index_dan_change = .data$heat_index_dan_f -
                       past_change$heat_index_dan_p,
                     temp_over_95_pctl_change = .data$temp_over_95_pctl_f -
                       past_change$ temp_over_95_pctl_p,
                     temp_over_99_pctl_change = .data$temp_over_99_pctl_f -
                       past_change$ temp_over_99_pctl_p,
                     temp_over_95_pctl_length_change = .data$temp_over_95_pctl_length_f -
                       past_change$ temp_over_95_pctl_length_p,
                     temp_under_freeze_change = .data$temp_under_freeze_f -
                       past_change$temp_under_freeze_p,
                     temp_under_freeze_length_change = .data$temp_under_freeze_length_f -
                       past_change$temp_under_freeze_length_p,
                     temp_under_5_pctl_change = .data$temp_under_5_pctl_f -
                       past_change$temp_under_5_pctl_p,
                     no_precip_change = .data$no_precip_f - past_change$no_precip_p,
                     no_precip_length_change = .data$no_precip_length_f -
                       past_change$no_precip_length_p,
                     precip_95_pctl_change = .data$precip_95_pctl_f -
                       past_change$precip_95_pctl_p,
                     precip_99_pctl_change = .data$precip_99_pctl_f -
                       past_change$precip_99_pctl_p,
                     precip_moderate_change = .data$precip_moderate_f -
                       past_change$precip_moderate_p,
                     precip_heavy_change = .data$precip_heavy_f - past_change$precip_heavy_p,
                     freeze_thaw_change = .data$freeze_thaw_f - past_change$freeze_thaw_p,
                     gdd_change = .data$gdd_f - past_change$gdd_p,
                     frost_change = .data$frost_f - past_change$frost_p,
                     grow_length_change = .data$grow_length_f - past_change$grow_length_p)

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

  # --------------
  # filter out gcm and cf only
  # --------------

  cf_gcm_only <- quadrant_df %>%
    dplyr::select(.data$gcm, .data$cf)

  threshold_summary <- change %>%
    dplyr::full_join(cf_gcm_only, by = "gcm")


  if(directory == "tempdir()"){warning("Files have been saved to temporary directory and will be deleted when this R session is closed. To save locally, input a local directory in which to save files into the `directory` argument.")}


readr::write_csv(threshold_summary, here::here(directory,
                                           paste0(SiteID, "_pca_summary.csv")))

return(threshold_summary)

}
