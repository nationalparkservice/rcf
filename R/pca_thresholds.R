#' Summarize threshold values by PCA and month, season, or year*
#'
#' Summarizes threshold values calculated in the `calc_thresholds` function based on
#' PCA.
#'
#' *For advanced users only.
#'
#' @param SiteID chosen name to use in file names, attributes, and directories. Default
#' name is "unnamed_site" (character)
#' @param pca_data The data that results from the `cf_pca` function (dataframe)
#' @param all_data The data that results from the `calc_thresholds` function (dataframe)
#' @param past_years years to base past data off of. Cannot be any earlier than 1950.
#' Must be written as c(past_start, past_end). Defaults to 1950:2000 (numeric)
#' @param future_year year to center changes from historical data around. Defaults to
#' 2040 (numeric)
#' @param summarize_by how to summarize the data, options are "month", "season", "year"
#' Defaults to year.
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
#'   date = sample(seq(as.Date('1950/01/01'), as.Date('2099/12/31'), by="day"), 1000),
#'   yr = rep(c(1980, 2040, 1980, 2040, 1980, 2040, 1980, 2040, 1980, 2040, 1980, 2040, 1980,
#'              2040, 1980, 2040, 1980, 2040, 1980, 2040), each = 50),
#'   gcm = rep(c("bcc-csm1-1.rcp45", "BNU-ESM.rcp45", "CanESM2.rcp85", "CCSM4.rcp45",
#'               "CSIRO-Mk3-6-0.rcp45"), each = 200),
#'   precip = rnorm(1000),
#'   tmin = rnorm(1000),
#'   tmax = rnorm(1000),
#'   rhmax = rnorm(1000),
#'   rhmin = rnorm(1000),
#'   tavg = rnorm(1000),
#'   heat_index = rnorm(1000),
#'   heat_index_ec = rnorm(1000),
#'   heat_index_dan = rnorm(1000),
#'   temp_over_95_pctl =  as.logical(sample(x = c("TRUE","FALSE"), size = 1000, replace = TRUE)),
#'   temp_over_99_pctl =  as.logical(sample(x = c("TRUE","FALSE"), size = 1000, replace = TRUE)),
#'   temp_over_95_pctl_length =  as.logical(sample(x = c("TRUE","FALSE"), size = 1000,
#'                                                 replace = TRUE)),
#'   temp_under_freeze =  as.logical(sample(x = c("TRUE","FALSE"), size = 1000, replace = TRUE)),
#'   temp_under_freeze_length =  as.logical(sample(x = c("TRUE","FALSE"), size = 1000,
#'                                                 replace = TRUE)),
#'   temp_under_5_pctl =  as.logical(sample(x = c("TRUE","FALSE"), size = 1000, replace = TRUE)),
#'   no_precip  =  as.logical(sample(x = c("TRUE","FALSE"), size = 1000, replace = TRUE)),
#'   no_precip_length =  as.logical(sample(x = c("TRUE","FALSE"), size = 1000, replace = TRUE)),
#'   precip_95_pctl =  as.logical(sample(x = c("TRUE","FALSE"), size = 1000, replace = TRUE)),
#'   precip_99_pctl =  as.logical(sample(x = c("TRUE","FALSE"), size = 1000, replace = TRUE)),
#'   precip_moderate =  as.logical(sample(x = c("TRUE","FALSE"), size = 1000, replace = TRUE)),
#'   precip_heavy =  as.logical(sample(x = c("TRUE","FALSE"), size = 1000, replace = TRUE)),
#'   freeze_thaw =  as.logical(sample(x = c("TRUE","FALSE"), size = 1000, replace = TRUE)),
#'   gdd =  as.logical(sample(x = c("TRUE","FALSE"), size = 1000, replace = TRUE)),
#'   gdd_count = rnorm(1000),
#'   not_gdd_count = rnorm(1000),
#'   frost = as.logical(sample(x = c("TRUE","FALSE"), size = 1000, replace = TRUE)),
#'   grow_length = rnorm(1000),
#'   units = rep("imperial", each = 1000)
#' )
#'
#' pca_data <- data.frame(
#'   gcm = c("bcc-csm1-1.rcp45", "BNU-ESM.rcp45", "CanESM2.rcp85", "CCSM4.rcp45",
#'           "CSIRO-Mk3-6-0.rcp45"),
#'   precip_change = rnorm(5),
#'   tmin_change = rnorm(5),
#'   tmax_change = rnorm(5),
#'   rhmax_change = rnorm(5),
#'   rhmin_change = rnorm(5),
#'   tavg_change = rnorm(5),
#'   heat_index_change = rnorm(5),
#'   heat_index_ec_change = rnorm(5),
#'   heat_index_dan_change = rnorm(5),
#'   temp_over_95_pctl_change =  rnorm(5),
#'   temp_over_99_pctl_change =  rnorm(5),
#'   temp_over_95_pctl_length_change =  rnorm(5),
#'   temp_under_freeze_change =  rnorm(5),
#'   temp_under_freeze_length_change =  rnorm(5),
#'   temp_under_5_pctl_change =  rnorm(5),
#'   no_precip_change  =  rnorm(5),
#'   no_precip_length_change =  rnorm(5),
#'   precip_95_pctl_change =  rnorm(5),
#'   precip_99_pctl_change =  rnorm(5),
#'   precip_moderate_change =  rnorm(5),
#'   precip_heavy_change =  rnorm(5),
#'   freeze_thaw_change =  rnorm(5),
#'   gdd_change =  rnorm(5),
#'   gdd_count_change = rnorm(5),
#'   not_gdd_count_change = rnorm(5),
#'   frost_change = rnorm(5),
#'   grow_length_change = rnorm(5),
#'   units = rep("imperial", each = 5),
#'   pca_type = c("PC1 Max", "PC1 Min", "PC2 Max", "PC2 Min", "NA"),
#'   cf = c("Hot Wet", "Hot Dry", "Warm Wet", "Warm Dry", "Central")
#' )
#'
#' pca_thresholds("SCBL", pca_data = pca_data, all_data = data, future_year = 2040,
#'                summarize_by = "year", past_years = c(1950, 2000))
#'                }
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data

pca_thresholds <- function(SiteID,
                           pca_data = NULL,
                           all_data = NULL,
                           past_years = c(1950, 2000),
                           future_year = 2040,
                           summarize_by = "year",
                           directory = tempdir()){

  # set code to stop or create warnings for mistakes

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

  if(summarize_by %in% c("month", "season", "year") == FALSE){
    stop("summarize_by can only be month, season or year, did you misspell?")
  }

  # set initials

  rh_exists <-  any(names(all_data) == "rhmin")
  suppressMessages(if(!file.exists(".here")) here::set_here(directory))

  future_start <- future_year - 15
  future_end <- future_year + 15

  past_start <- past_years[1]
  past_end <- past_years[2]

  #subset future data to be only for 30 yr period

  future_all <- all_data %>%
    dplyr::filter(.data$yr %in% c(future_start:future_end)) %>%
    dplyr::mutate(time = "Future")

  cf_gcm_only <- pca_data %>%
    dplyr::select(.data$gcm, .data$pca_type, .data$cf)

  past_all <- all_data %>%
    dplyr::filter(.data$yr %in% c(past_start:past_end)) %>%
    dplyr::mutate(time = "Historical") %>%
    dplyr::full_join(cf_gcm_only, by = "gcm")


  # --------------------
  # CALCULATE THRESHOLD VALUES BY PCA
  # --------------------


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
      dplyr::select(.data$gcm, .data$cf, .data$pca_type) %>%
      dplyr::full_join(future_all, by = "gcm")  %>%
      dplyr::full_join(past_all) %>%
      dplyr::group_by(.data$pca_type, .data$yr, .data$time) %>%
      tidyr::drop_na(.data$pca_type) %>%
      dplyr::summarize(gcm = unique(.data$gcm),
        cf = unique(.data$cf),
        precip_yearly = mean(.data$precip, na.rm = TRUE) * 365,
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
        grow_length = mean(.data$grow_length, na.rm = TRUE),
        units = unique(units),
        .groups = "keep"))
  }

  # ------------
  # SUMMARIZE FOR PCA AND MONTH OR SEASON
  # ------------

  # --------
  # GROUP FOR MONTH and PCA
  # --------

  suppressMessages(pca_cf_gcm <- pca_data %>%
                     dplyr::select(.data$gcm, .data$cf, .data$pca_type) %>%
                     dplyr::full_join(future_all, by = "gcm") %>%
                     dplyr::full_join(past_all))

  if(summarize_by == "month"){
    pca_cf_gcm <- pca_cf_gcm %>%
      dplyr::group_by(.data$pca_type, .data$month, .data$time)
  }

  # --------
  # GROUP FOR SEASON and CORNERS
  # --------

  if(summarize_by == "season"){
    pca_cf_gcm <- pca_cf_gcm %>%
      dplyr::group_by(.data$pca_type, .data$quarter, .data$time)
  }


  if(summarize_by %in% c("month", "season")){


    method_cf_gcm <- pca_cf_gcm %>%
      # grouped by month/season/year and climate future
      # sums will be per month/season/year per climate future
      dplyr::mutate(num_years = ifelse(.data$time == "Future", 30,
                                       (past_end - past_start)))  %>%
      tidyr::drop_na(.data$pca_type) %>%
      dplyr::summarize(gcm = unique(.data$gcm),
        cf = unique(.data$cf),
        precip_monthly = mean(.data$precip, na.rm = TRUE) * 30,
        tmin = mean(.data$tmin, na.rm = TRUE),
        tmax = mean(.data$tmax, na.rm = TRUE),
        tavg = mean(.data$tavg, na.rm = TRUE),
        rhmin = ifelse(rh_exists == TRUE, mean(.data$rhmin, na.rm = TRUE),
                       NA_integer_),
        rhmax = ifelse(rh_exists == TRUE, mean(.data$rhmax, na.rm = TRUE), NA_integer_),
        heat_index = ifelse(rh_exists == TRUE, mean(.data$heat_index, na.rm = TRUE),
                            NA_integer_),
        heat_index_ec = ifelse(rh_exists == TRUE,
                        sum(.data$heat_index_ec, na.rm = TRUE) / unique(.data$num_years),
                               NA_integer_),
        heat_index_dan = ifelse(rh_exists == TRUE, sum(.data$heat_index_dan,
                                                    na.rm = TRUE) / unique(.data$num_years),
                                NA_integer_),
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
    # divided by (30 years) =
    # average value per month/season/yr, per cf, across all years observed

  }

# -------------
# CREATE CSV
# -------------

  if(directory == tempdir()){warning("Files have been saved to temporary directory and will be deleted when this R session is closed. To save locally, input a local directory in which to save files into the `directory` argument.")}

  method_cf_gcm$time <- factor(method_cf_gcm$time, levels = c("Historical", "Future"))

readr::write_csv(method_cf_gcm, here::here(directory,
                                           paste0(SiteID,
                                           ifelse(summarize_by == "month",
                                                  "_month_summary_pca.csv",
                                                  ifelse(summarize_by == "season",
                                                         "_season_summary_pca.csv",
                                                         "_year_summary_pca.csv"
                                                                )))))

  return(method_cf_gcm)

} #close function
