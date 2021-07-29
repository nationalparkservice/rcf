#' Summarize threshold values by PCA and month, season, or year
#'
#' @param SiteID chosen name to use in file names, attributes, and
#'  directories. (character)
#' @param pca_data The data that results from the `cf_pca` function (dataframe)
#' @param all_data The data that results from the `calc_thresholds` function (dataframe)
#' @param year year to center changes from historical data around (numeric)
#' @param summarize_by how to summarize the data, options are "month", "season", "year"
#' @param directory where to save files to. Per CRAN guidelines, this
#' defaults to a temporary directory and files created will be lost after
#' R session ends. Specify a path to retain files.
#'
#' @return
#' One (1) csv file that summarizes threshold values based on PCA and month/season/year
#'
#' @export
#'
#' @examples
#' #' @examples
#' \dontrun{
#' # Generate sample data
#'
#' data <- data.frame(
#' date = sample(seq(as.Date('1950/01/01'), as.Date('2099/12/31'), by="day"), 100),
#' yr = rep(c(1960, 1970, 1980, 1990, 2000, 2010, 2020, 2030, 2040, 2050), each = 10),
#' month = rep(c(1:10), each = 10),
#' quarter = rep(rep(c("DJF", "MAM", "JJA", "SON"), each = 25)),
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
#' pca_df <- data.frame(
#'
#' )
#'
#' pca_thresholds("SCBL", pca_data = pca_df, all_data = data, year = 2040,
#' summarize_by = "month")
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data

pca_thresholds <- function(SiteID,
                           pca_data = NULL,
                           all_data = NULL,
                           year,
                           summarize_by = NULL,
                           directory = tempdir()){

  # set initials

  rh_exists <-  any(names(all_data) == "rhmin")
  suppressMessages(if(!file.exists(".here")) here::set_here(directory))

  start_year <- year - 15
  end_year <- year + 15

  #subset future data to be only for 30 yr period

  future_all <- all_data %>%
    dplyr::filter(.data$yr %in% c(start_year:end_year)) %>%
    dplyr::mutate(time = "Future")

  cf_gcm_only <- pca_data %>%
    dplyr::select(.data$gcm, .data$prcomp, .data$cf)

  past_all <- all_data %>%
    dplyr::filter(.data$yr < 2000) %>%
    dplyr::mutate(time = "Historical") %>%
    dplyr::full_join(cf_gcm_only, by = "gcm")


  # --------------------
  # CALCULATE THRESHOLD VALUES BY PCA
  # --------------------

  # --------
  # GROUP FOR MONTH and PCA
  # --------

  suppressMessages(pca_cf_gcm <- pca_data %>%
    dplyr::select(.data$gcm, .data$cf, .data$prcomp) %>%
    dplyr::full_join(future_all, by = "gcm") %>%
    dplyr::full_join(past_all))

  if(summarize_by == "month"){
    pca_cf_gcm <- pca_cf_gcm %>%
      dplyr::group_by(.data$prcomp, .data$month, .data$time)
  }

  # --------
  # GROUP FOR SEASON and CORNERS
  # --------

  if(summarize_by == "season"){
    pca_cf_gcm <- pca_cf_gcm %>%
      dplyr::group_by(.data$prcomp, .data$quarter, .data$time)
  }

  # ---------------
  # SUMMARIZE THRESHOLD VALUES FOR PCA
  # ---------------

  # --------
  # GROUP FOR YEAR and CORNERS
  # --------
  # different for year b/c doesn't need to be divided by num yr

  if(summarize_by == "year"){

    suppressMessages(
      method_cf_gcm <- pca_data %>%
      dplyr::select(.data$gcm, .data$cf, .data$prcomp) %>%
      dplyr::full_join(future_all, by = "gcm")  %>%
      dplyr::full_join(past_all) %>%
      dplyr::group_by(.data$prcomp, .data$yr, .data$time) %>%
      dplyr::summarize(cf = unique(cf),
        precip_daily = mean(.data$precip, na.rm = TRUE),
        tmin = mean(.data$tmin, na.rm = TRUE),
        tmax = mean(.data$tmax, na.rm = TRUE),
        tavg = mean(.data$tavg, na.rm = TRUE),
        rhmin = ifelse(rh_exists == TRUE, mean(.data$rhmin, na.rm = TRUE),
                                      NA_integer_),
        rhmax = ifelse(rh_exists == TRUE, mean(.data$rhmax, na.rm = TRUE), NA_integer_),
        heat_index = ifelse(rh_exists == TRUE, mean(.data$heat_index, na.rm = TRUE),
                           NA_integer_),
        heat_index_ec = ifelse(rh_exists == TRUE,
                            sum(.data$heat_index_ec, na.rm = TRUE),
                            NA_integer_),
        heat_index_dan = ifelse(rh_exists == TRUE, sum(.data$heat_index_dan,
                                                   na.rm = TRUE),
                                NA_integer_),
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
        grow_len = mean(.data$grow_len, na.rm = TRUE),
        units = unique(units),
        .groups = "keep")) %>%
      tidyr::drop_na(prcomp)
  }

  # ------------
  # SUMMARIZE FOR PCA AND MONTH OR SEASON
  # ------------


  if(summarize_by %in% c("month", "season")){

    method_cf_gcm <- pca_cf_gcm %>%
      # grouped by month/season/year and climate future
      # sums will be per month/season/year per climate future
      dplyr::summarize(cf = unique(cf),
        precip_daily = mean(.data$precip, na.rm = TRUE),
        tmin = mean(.data$tmin, na.rm = TRUE),
        tmax = mean(.data$tmax, na.rm = TRUE),
        tavg = mean(.data$tavg, na.rm = TRUE),
        rhmin = ifelse(rh_exists == TRUE, mean(.data$rhmin, na.rm = TRUE),
                       NA_integer_),
        rhmax = ifelse(rh_exists == TRUE, mean(.data$rhmax, na.rm = TRUE), NA_integer_),
        heat_index = ifelse(rh_exists == TRUE, mean(.data$heat_index, na.rm = TRUE),
                            NA_integer_),
        heat_index_ec = ifelse(rh_exists == TRUE,
                        sum(.data$heat_index_ec, na.rm = TRUE) / 30,
                               NA_integer_),
        heat_index_dan = ifelse(rh_exists == TRUE, sum(.data$heat_index_dan,
                                                    na.rm = TRUE) / 30,
                                NA_integer_),
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
        units = unique(units),
        .groups = "keep") %>%
      tidyr::drop_na(.data$prcomp)

    # (sum of threshold value, per month/season/year, per cf)
    # divided by (30 years) =
    # average value per month/season/yr, per cf, across all years observed

  }

# -------------
# CREATE CSV
# -------------

if(directory == "tempdir()"){print("Files have been saved to temporary directory and will be deleted when this R session is closed. To save locally, input where to save them into the `directory` argument.")}


readr::write_csv(method_cf_gcm, here::here(directory,
                                           paste0(SiteID,
                                           ifelse(summarize_by == "month",
                                                  "_month_summary_pca.csv",
                                                  ifelse(summarize_by == "season",
                                                         "_season_summary_pca.csv",
                                                         "_year_summary_pca.csv"
                                                                )))))

} #close function
