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
#' @param data Default data set to use for .csv creation. Must be created
#' prior to running function. Follow vignette for example data set creation (data frame)
#' @param units the unit type that will be used, defaults to "imperial"
#' ("imperial" or "metric")
#' @param past_years years to base past data off of. Cannot be any earlier than 1950.
#' Must be written as c(past_start, past_end). Defaults to 1950:2000 (numeric)
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
#' \dontrun{
#'
#' # Generate sample data
#'
#' data <- data.frame(
#' date = sample(seq(as.Date('1950/01/01'), as.Date('2099/12/31'), by="day"), 100),
#' yr = rep(c(1960, 1970, 1980, 1990, 2000, 2010, 2020, 2030, 2040, 2050), each = 10),
#' gcm = rep(c("bcc-csm1-1.rcp45", "BNU-ESM.rcp45", "CanESM2.rcp85", "CCSM4.rcp45",
#' "CSIRO-Mk3-6-0.rcp45"), each = 20),
#' precip = rnorm(100),
#' tmin = rnorm(100),
#' tmax = rnorm(100),
#' rhmax = rnorm(100),
#' rhmin = rnorm(100),
#' tavg = rnorm(100)
#' )
#'
#' calc_thresholds("SCBL", data = df, units = "imperial")
#'}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data

calc_thresholds <- function(SiteID = "unnamed_site",
                            data = NULL,
                            units = "imperial",
                            past_years = c(1950,2000),
                            directory = tempdir()){



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
    dplyr::filter(.data$yr %in% c(past_end:past_start)) %>%
    dplyr::group_by(.data$gcm) %>%
    dplyr::summarize(temp_95_pctl_p = stats::quantile(.data$tmax, 0.95, na.rm = TRUE),
                  temp_99_pctl_p = stats::quantile(.data$tmax, 0.99, na.rm = TRUE),
                  temp_5_pctl_p = stats::quantile(.data$tmin, 0.05, na.rm = TRUE),
                  precip_95_pctl_p = stats::quantile(.data$precip[which(.data$precip > no_precip_num)], 0.95, na.rm = TRUE),
                  precip_99_pctl_p = stats::quantile(.data$precip[which(.data$precip > no_precip_num)], 0.99, na.rm = TRUE))


  # ---------
  # functions
  # ---------


  # # # # HEAT INDEX FUNCTION # # # #

  if(units == "imperial"){
    heat_index <- function(temp, RH){

      Sted <- 0.5 * (temp + 61 + ((temp - 68) * 1.2) + (RH * 0.094))
      Roth <- -42.379 + (2.04901523 * temp) + (10.14333127 * RH) + (-.22475541 * temp * RH) +
        (-.00683783 * temp^2) + (-.05481717 * RH^2) + (.00122874 * temp^2 * RH) +
        (.00085282 * temp * RH^2) + (-.00000199 * temp^2 * RH^2)
      adj1 <- ((13 - RH) / 4) * sqrt((17 - abs(temp - 95)) / 17)
      adj2 <- ((RH - 85) / 10) * ((87 - temp) / 5)
      heat_index<-ifelse(temp < 80, Sted,
                         ifelse(RH < 13 & temp > 80 & temp < 112, Roth-adj1,
                                ifelse(RH > 85 & temp > 80 & temp < 87, Roth+adj2, Roth)))
    heat_index
  } #creates errors but doesn't matter becuase not used when not applicable
  }

    if(units == "metric"){


      heat_index <- function(temp, RH){
        tempf = (temp - 32) * (5 / 9)
        Sted <- 0.5 * (tempf + 61 + ((tempf - 68) * 1.2) + (RH * 0.094))
        Roth <- -42.379 + (2.04901523 * tempf) + (10.14333127 * RH) + (-.22475541 * tempf * RH) +
          (-.00683783 * tempf^2) + (-.05481717 * RH^2) + (.00122874 * tempf^2 * RH) +
          (.00085282 * tempf * RH^2) + (-.00000199 * tempf^2 * RH^2)
        adj1 <- ((13 - RH) / 4) * sqrt((17 - abs(tempf - 95)) / 17)
        adj2 <- ((RH - 85) / 10) * ((87 - tempf) / 5)
        heat_index<-ifelse(tempf < 80, Sted,
                           ifelse(RH < 13 & tempf > 80 & tempf < 112, Roth-adj1,
                                  ifelse(RH > 85 & tempf > 80 & tempf < 87, Roth+adj2, Roth)))
        heat_index

      }

      }


  if(units == "metric")print("Heat index must be calculated using Fahrenheit. All values related to heat index are calculated in fahrenheit.")
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

  # --------------------------
  # # # # IMPERIAL # # # #
  # --------------------------

  if(units == "imperial"){

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
                    temp_under_freeze = .data$tmin < 32,
                    # returns TRUE or FALSE
                    temp_under_freeze_length = (.data$temp_under_freeze)*unlist(lapply(
                      rle(.data$temp_under_freeze)$lengths, seq_len)),
                    temp_under_5_pctl = .data$tmin < .data$temp_5_pctl_p,
                    # returns temperature of the 5th quantile
                    no_precip = .data$precip < 0.04,
                    # returns TRUE or FALSE, precip greater than 0.04 inches
                    no_precip_length = (.data$no_precip)*unlist(lapply(rle(
                      .data$no_precip)$lengths, seq_len)),
                    precip_95_pctl = .data$precip > .data$precip_95_pctl_p,
                    # returns TRUE or FALSE
                    precip_99_pctl = .data$precip > .data$precip_99_pctl_p,
                    # returns TRUE or FALSE
                    precip_moderate = .data$precip > 1,
                    # returns TRUE or FALSE
                    precip_heavy = .data$precip > 2,
                    # returns TRUE or FALSE
                    freeze_thaw = .data$tmin < 28 & .data$tmax > 34,
                    # returns TRUE or FALSE
                    gdd = .data$tavg > 41,
                    # returns TRUE or FALSE
                    gdd_count = .data$gdd * unlist(lapply(rle(.data$gdd)$lengths, seq_len)),
                    # returns consecutive gdd
                    not_gdd_count = (.data$gdd == FALSE) * unlist(lapply(rle(
                      .data$gdd)$lengths, seq_len)),
                    #%>%
                    # returns consecutive days that are not gdd
                    frost = .data$gdd == TRUE & .data$tmin < 32) %>%
      dplyr::select(!colnames(past_pctl))


  } # close if statement for "imperial"



  # ------------------------
  # # # # METRIC # # # #
  # ------------------------


  if(units == "metric"){

    thresholds <- thresholds1 %>%
      dplyr::mutate(heat_index = ifelse(rh_exists == TRUE,
                                        heat_index(.data$tmax, .data$rhmin), NA_integer_),
                    # returns a number
                    heat_index_ec = ifelse(rh_exists == TRUE,
                                           .data$heat_index > 89 & .data$heat_index < 103,
                                           NA_integer_),
                    # returns TRUE or FALSE
                    heat_index_dan = ifelse(rh_exists == TRUE,
                                            .data$heat_index > 102 & .data$heat_index < 124,
                                            NA_integer_),
                    # returns TRUE or FALSE
                    temp_over_95_pctl = .data$tmax > .data$temp_95_pctl_p,
                    # returns TRUE or FALSE
                    temp_over_99_pctl = .data$tmax > .data$temp_99_pctl_p,
                    # returns TRUE or FALSE
                    temp_over_95_pctl_length = (.data$temp_over_95_pctl)*unlist(lapply(
                      rle(.data$temp_over_95_pctl)$lengths, seq_len)),
                    temp_under_freeze = .data$tmin < 0,
                    # returns TRUE or FALSE
                    temp_under_freeze_length = (.data$temp_under_freeze)*unlist(lapply(
                      rle(.data$temp_under_freeze)$lengths, seq_len)),
                    temp_under_5_pctl = .data$tmin < .data$temp_5_pctl_p,
                    # returns temperature of the 5th quantile
                    no_precip = .data$precip < 1, # 0.04 in = 1 mm
                    # returns TRUE or FALSE, precip greater than 1 mm
                    no_precip_length = (.data$no_precip)*unlist(lapply(rle(
                      .data$no_precip)$lengths, seq_len)),
                    precip_95_pctl = .data$precip > .data$precip_95_pctl_p,
                    # returns TRUE or FALSE
                    precip_99_pctl = .data$precip > .data$precip_99_pctl_p,
                    # returns TRUE or FALSE
                    precip_moderate = .data$precip > 25, # 1 inch rain
                    # returns TRUE or FALSE
                    precip_heavy = .data$precip > 50, # 2 inch rani
                    # returns TRUE or FALSE
                    freeze_thaw = .data$tmin < -2.2 & .data$tmax > 1.1, #28f to 34f
                    # returns TRUE or FALSE
                    gdd = .data$tavg > 5, #41f
                    # returns TRUE or FALSE
                    gdd_count = .data$gdd * unlist(lapply(rle(.data$gdd)$lengths, seq_len)),
                    # returns consecutive gdd
                    not_gdd_count = (.data$gdd == FALSE) * unlist(lapply(rle(
                      .data$gdd)$lengths, seq_len)),
                    # returns consecutive days that are not gdd
                    frost = .data$gdd == TRUE & .data$tmin < 0) %>%
      dplyr::select(!colnames(past_pctl))


  } # close if statement for "metric"

  # print statement for lack of rh

  if(rh_exists == FALSE) print("Cannot calculate heat index. Dataframe does not include relative humitity.")

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

    if(directory == "tempdir()"){print("Files have been saved to temporary directory and will be deleted when this R session is closed. To save locally, input where to save them into the `directory` argument.")}


    readr::write_csv(thresholds, here::here(directory,
                                            paste(SiteID,
                                                  "thresholds.csv",
                                                  sep = "_")))

    print("Do not edit this csv in excel. File is too large and data will be lost.")



}
