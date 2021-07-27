
#' Title
#'
#' @param SiteID chosen name to use in file names, attributes, and
#'  directories. (character)
#' @param data Default dataset to use for the .csv files this function will create.
#' Follow vignette for example dataset creation. (data frame)
#' @param year year to center changes from historical data around (numeric)
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
#'#' @examples
#' \dontrun{
#' # Generate sample data
#'
#' data <- data.frame(
#' date = sample(seq(as.Date('1950/01/01'), as.Date('2099/12/31'), by="day"), 100),
#' yr = sample(seq(as.Date('1950/01/01'), as.Date('2099/12/31'), by="year"), 100),
#' gcm = paste0(rep(letters[1:5], each = 20),
#' rep(letters[1:20], each = 5),
#' rep(letters[20:26], each = 1)),
#' precip = rnorm(100),
#' tmin = rnorm(100),
#' tmax = rnorm(100),
#' rhmax = rnorm(100),
#' rhmin = rnorm(100),
#' tavg = rnorm(100)
#' heat_index = rnorm(100)
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
#' grow_len = rnorm(100)
#' )
#'
#' summarize_for_pca("SCBL", data = data, 2040, directory = my_project_directory)
#' }


summarize_for_pca <- function(SiteID,
                        data = NULL,
                        year,
                        directory = tempdir()){

  # ---------
  #subset data for future to be 30 years around focus year
  # ---------

  rh_exists <-  any(names(data) == "rhmin")
  suppressMessages(if(!file.exists(".here")) here::set_here(directory))

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

  # ------------
  # CALCULATE THREHOLDS
  # ------------

  threshold_summary <- future_all %>%
    dplyr::group_by(.data$gcm) %>%
    dplyr::summarize(heat_index = if(rh_exists == TRUE) mean(.data$heat_index, na.rm = TRUE),
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
    dplyr::full_join(quadrant_df, by = "gcm") %>%
    dplyr::ungroup()

  if(directory == "tempdir()"){print("Files have been saved to temporary directory and will be deleted when this R session is closed. To save locally, input where to save them into the `directory` argument.")}


readr::write_csv(threshold_summary, here::here(directory,
                                           paste0(SiteID, "_pca_summary.csv")))

}
