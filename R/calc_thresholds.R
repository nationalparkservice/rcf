#' Calculate threshold values for climate futures
#'
#' Calculate values that exceed or go below a certain number as either "TRUE" of "FALSE."
#' The summary of these threshold values is calculated in either `cf_quadrant` or
#' `summarize_for_pca`functions. This is a very large dataframe and SHOULD NOT be edited
#' outside of R. Editing this dataframe outside of R will result in a loss of data, and
#' lead to inaccurate results.
#'
#' @param SiteID chosen name to use in file names, attributes, and
#'  directories. (character)
#' @param data Default data set to use for .csv creation. Must be created prior to
#' running function. Follow vignette for example data set creation, and names must match
#' naming convention mentioned in the vignette, will be the data that results from the
#' rcf_data function if using that data. (data frame)
#' @param units the unit type that will be used, defaults to "imperial"
#' ("imperial" or "metric")
#' @param past_years years to base past data off of. Cannot be any earlier than 1950 or later
#' 2005, due to the definition of past in the MACA v2 data (AMBER TO FIX). Must be written as
#' c(past_start, past_end). Defaults to 1950 to 2000 (numeric)
#' @param directory where to save files to. Per CRAN guidelines, this
#' defaults to a temporary directory and files created will be lost after
#' R session ends. Specify a path to retain files.
#'
#' @return
#' one csv file:
#' SiteID_thresholds.csv
#' @export
#'
#' @examples
#'
#'\dontrun{
#' # Generate sample data
#'
#' data <- data.frame(
#' date = sample(seq(as.Date('1950/01/01'), as.Date('2099/12/31'), by="day"), 1000),
#' yr = rep(c(1960, 1970, 1980, 1990, 2000, 2010, 2020, 2030, 2040, 2050), each = 100),
#' gcm = rep(c("bcc-csm1-1.rcp45", "BNU-ESM.rcp45", "CanESM2.rcp85", "CCSM4.rcp45",
#' "CSIRO-Mk3-6-0.rcp45"), each = 200),
#' precip = rnorm(1000),
#' tmin = rnorm(1000),
#' tmax = rnorm(1000),
#' rhmax = rnorm(1000),
#' rhmin = rnorm(1000),
#' tavg = rnorm(1000)
#' )
#'
#' calc_thresholds(SiteID = "SCBL", data = data, past_years = c(1950, 2002), units = "imperial")
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data

calc_thresholds <- function(SiteID = "unnamed_site",
                            data = NULL,
                            units = "imperial",
                            past_years = c(1950,2000),
                            directory = tempdir()){

  #stop create errors if people enter incorrect years
  #past years can only be between 1950 and 2005
  if (any(past_years < 1950 | past_years > 2005)) {
    stop("The requested period for historic values is incorrect for this function. Years must be between 1950 and 2005")
  }

  #past years must be only 2 distinct years
  if(length(past_years) > 2){
    stop("You may have entered the range of years as (start_year:end_year). Did you mean to write (start_year, end_year)? Vector cannot be of length greater than 2.")
  }

  if(past_years[2] - past_years[1] < 30 & past_years[1] < past_years[2]){
    stop("Past year range must be at least 30 years.")
  }

  if(past_years[1] > past_years[2]){
    stop("Past years entered in incorrect order, should be c(start_year, end_year).")
  }

  if(units %in% c("imperial", "metric") == FALSE){
    stop("Units can only be imperial or metric, did you misspell?")
  }

  rh_exists <-  any(names(data) == "rhmin")
  suppressMessages(if(!file.exists(".here")) here::set_here(directory))

  # set up past years
  past_start = past_years[1]
  past_end = past_years[2]

  # set variables based on metric or imperial

  no_precip_num <- ifelse(units == "imperial", 0.04, 1) #0.04 in, 1 mm
  freeze_num <- ifelse(units == "imperial", 32, 0) #32f, 0c
  precip_moderate_num <- ifelse(units == "imperial", 1, 25) #1 in, 25 mm
  precip_heavy_num <- ifelse(units == "imperial", 2, 50) #2 in, 50 mm
  freeze_thaw_low <- ifelse(units == "imperial", 28, -2.2) #28f, -2.2c
  freeze_thaw_high <- ifelse(units == "imperial", 34, 1.1) #34f, 1.1c
  gdd_num <- ifelse(units == "imperial", 41, 5) #41f, 5c

  # get baseline data to compare future scenario pctls to

  past_pctl <- data %>%
    dplyr::filter(.data$yr %in% c(past_start:past_end)) %>%
    dplyr::summarize(temp_95_pctl_p = stats::quantile(.data$tmax, 0.95, na.rm = TRUE),
                  temp_99_pctl_p = stats::quantile(.data$tmax, 0.99, na.rm = TRUE),
                  temp_5_pctl_p = stats::quantile(.data$tmin, 0.05, na.rm = TRUE),
                  precip_95_pctl_p = stats::quantile(.data$precip[which(.data$precip > no_precip_num)], 0.95, na.rm = TRUE),
                  precip_99_pctl_p = stats::quantile(.data$precip[which(.data$precip > no_precip_num)], 0.99, na.rm = TRUE),
                  gcm = unique(.data$gcm))


  # ---------
  # functions
  # ---------


  # # # # HEAT INDEX FUNCTION # # # #

      heat_index <- function(temp, RH){

        if(units == "imperial"){tempf = temp}

        if(units == "metric"){tempf = (temp - 32) * (5 / 9)}

        Sted <- 0.5 * (tempf + 61 + ((tempf - 68) * 1.2) + (RH * 0.094))
        Roth <- -42.379 + (2.04901523 * tempf) + (10.14333127 * RH) + (-.22475541 * tempf * RH) +
          (-.00683783 * tempf^2) + (-.05481717 * RH^2) + (.00122874 * tempf^2 * RH) +
          (.00085282 * tempf * RH^2) + (-.00000199 * tempf^2 * RH^2)
        adj1 <- ifelse(RH < 13 & tempf > 80 & tempf < 112, (((13 - RH) / 4) * sqrt((17 - abs(tempf - 95)) / 17)), NA_real_)
        adj2 <- ((RH - 85) / 10) * ((87 - tempf) / 5)
        heat_index<-ifelse(tempf < 80, Sted,
                           ifelse(RH < 13 & tempf > 80 & tempf < 112, Roth-adj1,
                                  ifelse(RH > 85 & tempf > 80 & tempf < 87, Roth+adj2, Roth)))
        heat_index

      }


  if(units == "metric")warning("Heat index calculated successfully, but must be calculated using Fahrenheit. All values related to heat index are in imperial units, not metric.")
  # # # # END HEAT INDEX FUNCTION # # # #


  # ---------------------------
  # CALCULATE THRESHOLD VALUES
  # ---------------------------

  suppressMessages(
    thresholds1 <- data %>%
      dplyr::mutate(month = lubridate::month(.data$date)) %>%
      dplyr::mutate(doy = lubridate::yday(.data$date)) %>%
      dplyr::mutate(halfyr = dplyr::case_when(.data$doy <= 182 ~ 1,
                                              TRUE ~ 2)) %>%
      dplyr::mutate(quarter = dplyr::case_when(
        .data$month%in% c(12, 1, 2) ~ "DJF",
        .data$month%in% c(3:5) ~ "MAM",
        .data$month%in% c(6:8) ~ "JJA",
        .data$month%in% c(9:11) ~ "SON")) %>%
      # returns quarter name
      dplyr::full_join(past_pctl, by = "gcm") %>%
      dplyr::group_by(.data$gcm, .data$yr)

  )# close suppressMessages

    suppressWarnings(
      thresholds <- thresholds1 %>%
        dplyr::mutate(heat_index = if(rh_exists == TRUE) heat_index(.data$tmax,
                                                                    .data$rhmin) else
                                                                      (NA_integer_),
                      # returns a number
                      heat_index_ec = if(rh_exists == TRUE)
                        .data$heat_index > 89 & .data$heat_index < 103 else
                          (NA_integer_),
                      # returns TRUE or FALSE
                      heat_index_dan = if(rh_exists == TRUE)
                        .data$heat_index > 102 & .data$heat_index < 124
                      else(NA_integer_),
                      # returns TRUE or FALSE
                      temp_over_95_pctl = .data$tmax > .data$temp_95_pctl_p,
                      # returns TRUE or FALSE - based off of historic value
                      temp_over_99_pctl = .data$tmax > .data$temp_99_pctl_p,
                      # returns TRUE or FALSE - based off of historic value
                      temp_over_95_pctl_length = (.data$temp_over_95_pctl)*unlist(lapply(
                        rle(.data$temp_over_95_pctl)$lengths, seq_len)),
                      temp_under_freeze = .data$tmin < freeze_num,
                      # returns TRUE or FALSE
                      temp_under_freeze_length = (.data$temp_under_freeze)*unlist(lapply(
                        rle(.data$temp_under_freeze)$lengths, seq_len)),
                      temp_under_5_pctl = .data$tmin < .data$temp_5_pctl_p,
                      # returns temperature of the 5th quantile
                      no_precip = .data$precip < no_precip_num,
                      # returns TRUE or FALSE, precip greater than 0.04 inches
                      no_precip_length = (.data$no_precip)*unlist(lapply(rle(
                        .data$no_precip)$lengths, seq_len)),
                      precip_95_pctl = .data$precip > .data$precip_95_pctl_p,
                      # returns TRUE or FALSE
                      precip_99_pctl = .data$precip > .data$precip_99_pctl_p,
                      # returns TRUE or FALSE
                      precip_moderate = .data$precip > precip_moderate_num,
                      # returns TRUE or FALSE
                      precip_heavy = .data$precip > precip_heavy_num,
                      # returns TRUE or FALSE
                      freeze_thaw = .data$tmin < freeze_thaw_low & .data$tmax > freeze_thaw_high,
                      # returns TRUE or FALSE
                      gdd = .data$tavg > gdd_num,
                      # returns TRUE or FALSE
                      gdd_count = .data$gdd * unlist(lapply(rle(.data$gdd)$lengths, seq_len)),
                      # returns consecutive gdd
                      not_gdd_count = (.data$gdd == FALSE) * unlist(lapply(rle(
                        .data$gdd)$lengths, seq_len)),
                      # returns consecutive days that are not gdd
                      frost = .data$gdd == TRUE & .data$tmin < freeze_num) %>%
        dplyr::select(!colnames(past_pctl)))

  # print statement for lack of rh

  if(rh_exists == FALSE) warning("Cannot calculate heat index. Dataframe does not include relative humidity.")

  # -------------------
  # GROWING SEASON LENGTH CALCULATION
  # -------------------

  beg_grow <- thresholds %>%
    dplyr::select(.data$date, .data$yr, .data$gcm, .data$doy, .data$gdd_count) %>%
    dplyr::group_by(.data$yr, .data$gcm) %>%
    dplyr::filter(.data$gdd_count == 7) %>%
    dplyr::slice(1) %>% #take first observation
    dplyr::rename(beg_grow = .data$doy) %>%
    dplyr::select(.data$yr, .data$gcm, .data$beg_grow)

  end_grow <- thresholds %>%
    dplyr::select(.data$date, .data$yr, .data$gcm, .data$doy, .data$not_gdd_count, .data$halfyr) %>%
    dplyr::group_by(.data$yr, .data$gcm) %>%
    dplyr::filter(.data$not_gdd_count == 6 & .data$halfyr == 2) %>%
    dplyr::slice(1) %>%
    dplyr::rename(end_grow = .data$doy) %>%
    dplyr::select(.data$yr, .data$gcm, .data$end_grow)

  suppressMessages(
    grow_length <- end_grow %>%
      dplyr::full_join(beg_grow) %>%
      dplyr::group_by(.data$gcm, .data$yr) %>%
      dplyr::summarise(grow_length = .data$end_grow - .data$beg_grow)
  )#close supppressMessages

  suppressMessages(
    thresholds <- thresholds %>%
      dplyr::full_join(grow_length)
  )#suppressMessages

  #all warnings produced are related to heat index function

    # --------------
    # CSV CREATION
    # --------------

    #if directory isn't temporary, save to local file
    # if it is temporary, give warning and save to temp directory

    if(directory == tempdir()){warning("Files have been saved to temporary directory and will be deleted when this R session is closed. To save locally, input a local directory in which to save files into the `directory` argument.")}


    readr::write_csv(thresholds, here::here(directory,
                                            paste(SiteID,
                                                  "thresholds.csv",
                                                  sep = "_")))
    warning("thresholds.csv generated successfully. DO NOT edit this csv in excel. File is too large and data will be lost, causing errors in future calculations.")

    return(thresholds)
}
