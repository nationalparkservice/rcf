#' Calculate threshold values for climate futures
#'
#' @param SiteID chosen name to use in file names, attributes, and
#'  directories. (character)
#' @param data Default data set to use for .csv creation. Must be created
#' prior to running function. Follow vignette for example data set creation (data frame)
#' @param units the unit type that will be used, defaults to "imperial"
#' ("imperial" or "metric")
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
#' )
#'
#' calc_thresholds("SCBL", data = df)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data


calc_thresholds <- function(SiteID,
                            data = NULL,
                            units = "imperial",
                            directory = tempdir()){

  here::set_here(directory)

  # if(variables == "all"){
  # variables = c("season","over_hot_temp", "over_high_q", "tmax_99", "heat_consecutive", "under_cold_temp", "under_low_q", "cold_consecutive", "no_precip", "no_precip_length", "over_precip_95", "over_precip_99", "precip_over_1", "precip_over_2", "freeze_thaw", "gdd", "gdd_count", "n_gdd_count", "heat_index", "heat_index_ec", "heat_index_dan", "frost")
  # } # allow option for users to select all of these without needing to write them individually

### QUESTION: Are there other groups of variables that might be typical to select, i.e. "all_precip" or "all_heat_index"

  rh_exists <-  any(names(data) == "rhmin")
  if(!file.exists(".here")) here::set_here(directory)


  # --------------------------
  # # # # IMPERIAL # # # #
  # --------------------------



  if(units == "imperial"){

  # ---------
  # functions
  # ---------


  # # # # HEAT INDEX FUNCTION # # # #

  heat_index <- function(temp, RH) {
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

  # # # # END HEAT INDEX FUNCTION # # # #

  # --------
  # CALCULATE THRESHOLDS DATA FRAME
  # --------

    thresholds <- data %>%
      dplyr::mutate(month = lubridate::month(.data$date, label = TRUE)) %>%
      dplyr::mutate(doy = lubridate::yday(.data$date)) %>%
      dplyr::mutate(halfyr = dplyr::case_when(.data$doy <= 182 ~ 1,
                                              TRUE ~ 2)) %>%
      dplyr::mutate(quarter = dplyr::case_when(
        .data$month%in% c("Dec", "Jan", "Feb") ~ "DJF",
        .data$month%in% c("Mar", "Apr", "May") ~ "MAM",
        .data$month%in% c("Jun", "Jul", "Aug") ~ "JJA",
        .data$month%in% c("Sep", "Oct", "Nov") ~ "SON")) %>%
      # returns quarter name
      dplyr::group_by(.data$gcm, .data$yr) %>%
      dplyr::mutate(heat_index = if(rh_exists == TRUE) heat_index(.data$tmax, .data$rhmin),
      # returns a number
      heat_index_ec = if(rh_exists == TRUE).data$heat_index > 89 & .data$heat_index < 103,
      # returns TRUE or FALSE
      heat_index_dan = if(rh_exists == TRUE).data$heat_index > 102 & .data$heat_index < 124,
      # returns TRUE or FALSE
      temp_over_95_pctl = .data$tmax > stats::quantile(.data$tmax, 0.95, na.rm = TRUE),
      # returns TRUE or FALSE
      temp_over_99_pctl = .data$tmax > stats::quantile(.data$tmax, 0.99, na.rm = TRUE),
      # returns TRUE or FALSE
      temp_over_95_pctl_length = (.data$temp_over_95_pctl)*unlist(lapply(rle(.data$temp_over_95_pctl)$lengths, seq_len)),
      temp_under_freeze = .data$tmin < 32,
      # returns TRUE or FALSE
      temp_under_freeze_length = (.data$temp_under_freeze)*unlist(lapply(rle(.data$temp_under_freeze)$lengths, seq_len)),
      temp_under_5_pctl = .data$tmin < stats::quantile(.data$tmin, 0.05, na.rm = TRUE),
      # returns temperature of the 5th quantile
      no_precip = .data$precip < 0.05,
      # returns TRUE or FALSE, precip greater than 0.05 inches
      no_precip_length = (.data$no_precip)*unlist(lapply(rle(.data$no_precip)$lengths, seq_len)),
      precip_95_pctl = .data$precip > stats::quantile(.data$precip[which(.data$precip > 0.05)], 0.95, na.rm = TRUE),
      # returns TRUE or FALSE
      precip_99_pctl = .data$precip > stats::quantile(.data$precip[which(.data$precip > 0.05)], 0.99, na.rm = TRUE),
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
      not_gdd_count = (.data$gdd == FALSE) * unlist(lapply(rle(.data$gdd)$lengths, seq_len)),
    #%>%
      # returns consecutive days that are not gdd
      frost = .data$gdd == TRUE & .data$tmin < 32)


  } # close if statement for "imperial"



  # ------------------------
  # # # # METRIC # # # #
  # ------------------------


  if(units == "metric"){

    # ---------
    # functions
    # ---------

    # # # # HEAT INDEX FUNCTION # # # #

    heat_index <- function(temp, RH) {

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
    } #creates errors but doesn't matter becuase not used when not applicable

    # # # # END HEAT INDEX FUNCTION # # # #



    # ----------------------------
    # # # # # CALCULATE THRESHOLD DATA # # # #
    # ----------------------------

    thresholds <- data %>%
      dplyr::rename(gcm = .data$GCM) %>%
      dplyr::mutate(month = lubridate::month(.data$date, label = TRUE)) %>%
      dplyr::mutate(doy = lubridate::yday(.data$date)) %>%
      dplyr::mutate(halfyr = ifelse(.data$doy <= 182, 1, 2)) %>%
      dplyr::mutate(quarter = dplyr::case_when(
        .data$month %in% c("Dec", "Jan", "Feb") ~ "DJF",
        .data$month %in% c("Mar", "Apr", "May") ~ "MAM",
        .data$month %in% c("Jun", "Jul", "Aug") ~ "JJA",
        .data$month %in% c("Sep", "Oct", "Nov") ~ "SON")) %>%
      # returns quarter name
      dplyr::group_by(.data$gcm, .data$yr) %>%
      dplyr::mutate(heat_index = if(rh_exists == TRUE) heat_index(.data$tmax, .data$rhmin),
                    # returns a number
                    heat_index_ec = if(rh_exists == TRUE).data$heat_index > 89 & .data$heat_index < 103,
                    # returns TRUE or FALSE
                    heat_index_dan = if(rh_exists == TRUE).data$heat_index > 102 & .data$heat_index < 124,
                    # returns TRUE or FALSE
                    temp_over_95_pctl = .data$tmax > stats::quantile(.data$tmax, 0.95, na.rm = TRUE),
                    # returns TRUE or FALSE
                    temp_over_99_pctl = .data$tmax > stats::quantile(.data$tmax, 0.99, na.rm = TRUE),
                    # returns TRUE or FALSE
                    temp_over_95_pctl_length = (.data$temp_over_95_pctl)*unlist(lapply(rle(.data$temp_over_95_pctl)$lengths, seq_len)),
                    temp_under_freeze = .data$tmin < 0,
                    # returns TRUE or FALSE
                    temp_under_freeze_length = (.data$temp_under_freeze)*unlist(lapply(rle(.data$temp_under_freeze)$lengths, seq_len)),
                    temp_under_5_pctl = .data$tmin < stats::quantile(.data$tmin, 0.05, na.rm = TRUE),
                    # returns temperature of the 5th quantile
                    no_precip = .data$precip < 1.27, # 0.05 in = 1.27 mm
                    # returns TRUE or FALSE, precip greater than 1.27 mm
                    no_precip_length = (.data$no_precip)*unlist(lapply(rle(.data$no_precip)$lengths, seq_len)),
                    precip_95_pctl = .data$precip > stats::quantile(.data$precip[which(.data$precip > 0.05)], 0.95, na.rm = TRUE),
                    # returns TRUE or FALSE
                    precip_99_pctl = .data$precip > stats::quantile(.data$precip[which(.data$precip > 0.05)], 0.99, na.rm = TRUE),
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
                    not_gdd_count = (.data$gdd == FALSE) * unlist(lapply(rle(.data$gdd)$lengths, seq_len)),
                    #%>%
                    # returns consecutive days that are not gdd
                    frost = .data$gdd == TRUE & .data$tmin < 0)#freezing



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
      dplyr::slice(1) %>%
      dplyr::rename(beg_grow = .data$doy) %>%
      dplyr::select(.data$yr, .data$gcm, .data$beg_grow)

    end_grow <- thresholds %>%
      dplyr::select(.data$date, .data$yr, .data$gcm, .data$doy, .data$not_gdd_count, .data$halfyr) %>%
      dplyr::group_by(.data$yr, .data$gcm) %>%
      dplyr::filter(.data$not_gdd_count == 6 & .data$halfyr == 2) %>%
      dplyr::slice(1) %>%
      dplyr::rename(end_grow = .data$doy) %>%
      dplyr::select(.data$yr, .data$gcm, .data$end_grow)

    grow_len <- end_grow %>%
      dplyr::full_join(beg_grow) %>%
      dplyr::group_by(.data$gcm, .data$yr) %>%
      dplyr::summarise(grow_len = .data$end_grow - .data$beg_grow)

    thresholds <- thresholds %>%
      dplyr::full_join(grow_len)

    #all warnings produced are related to heat index function

    # --------------
    # CSV CREATION
    # --------------

    ifelse(directory == "tempdir()", print("Files have been saved to temporary directory and will be deleted when this R session is closed. To save locally, input where to save them into the `directory` argument."),
           readr::write_csv(thresholds, here::here(directory,
                                                   paste(SiteID, "thresholds.csv", sep = "_"))))

}
