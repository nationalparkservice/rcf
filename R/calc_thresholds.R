calc_thresholds <- function(SiteID, data = NULL, variables = NULL, units = "imperial"){

#   if(variables == "all"){
#   variables = c("season","over_hot_temp", "over_high_q", "tmax_99", "heat_consecutive", "under_cold_temp", "under_low_q", "cold_consecutive", "no_precip", "no_precip_length", "over_precip_95", "over_precip_99", "precip_over_1", "precip_over_2", "freeze_thaw", "gdd", "gdd_count", "n_gdd_count", "heat_index", "heat_index_ec", "heat_index_dan", "frost")
# } # allow option for users to select all of these without needing to write them individually

### QUESTION: Are there other groups of variables that might be typical to select, i.e. "all_precip" or "all_heat_index"

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
  # final data frame
  # --------

    thresholds <- data %>%
      dplyr::rename(gcm = GCM) %>%
      dplyr::mutate(month = lubridate::month(date, label = TRUE),
                    doy = lubridate::yday(date),
                    halfyr = ifelse(.data$doy <= 182, 1, 2)) %>%
      dplyr::mutate(quarter = dplyr::case_when(
        .data$month %in% c("Dec", "Jan", "Feb") ~ "DJF",
        .data$month %in% c("Mar", "Apr", "May") ~ "MAM",
        .data$month %in% c("Jun", "Jul", "Aug") ~ "JJA",
        .data$month %in% c("Sep", "Oct", "Nov") ~ "SON")) %>%
      # returns quarter name
      dplyr::group_by(gcm, yr) %>%
      dplyr::mutate(heat_index = heat_index(.data$tmax, .data$rhmin),
      # returns a number
      heat_index_ec = .data$heat_index > 89 & .data$heat_index < 103,
      # returns TRUE or FALSE
      heat_index_dan = .data$heat_index > 102 & .data$heat_index < 124,
      # returns TRUE or FALSE
      temp_over_95_pctl = tmax > quantile(.data$tmax, 0.95, na.rm = TRUE),
      # returns TRUE or FALSE
      temp_over_99_pctl = tmax > quantile(.data$tmax, 0.99, na.rm = TRUE),
      # returns TRUE or FALSE
      temp_over_95_pctl_length = (.data$temp_over_95_pctl)*unlist(lapply(rle(.data$temp_over_95_pctl)$lengths, seq_len)),
      temp_under_32_f = .data$tmin < 32,
      # returns TRUE or FALSE
      temp_under_32_f_length = (.data$temp_under_32_f)*unlist(lapply(rle(.data$temp_under_32_f)$lengths, seq_len)),
      temp_under_5_pctl = tmin < quantile(.data$tmin, 0.05, na.rm = TRUE),
      # returns temperature of the 5th quantile
      no_precip = .data$precip < 0.05,
      # returns TRUE or FALSE, precip greater than 0.05 inches
      no_precip_length = (.data$no_precip)*unlist(lapply(rle(.data$no_precip)$lengths, seq_len)),
      precip_95_pctl = precip > quantile(.data$precip[which(.data$precip > 0.05)], 0.95, na.rm = TRUE),
      # returns TRUE or FALSE
      precip_99_pctl = precip > quantile(.data$precip[which(.data$precip > 0.05)], 0.99, na.rm = TRUE),
      # returns TRUE or FALSE
      precip_over_1 = .data$precip > 1,
      # returns TRUE or FALSE
      precip_over_2 = .data$precip > 2,
      # returns TRUE or FALSE
      freeze_thaw = .data$tmin < 28 & .data$tmax > 34,
      # returns TRUE or FALSE
      gdd = tavg > 41,
      # returns TRUE or FALSE
      gdd_count = .data$gdd * unlist(lapply(rle(.data$gdd)$lengths, seq_len)),
      # returns consecutive gdd
      not_gdd_count = (.data$gdd == FALSE) * unlist(lapply(rle(.data$gdd)$lengths, seq_len)),
    #%>%
      # returns consecutive days that are not gdd
      frost = .data$gdd == TRUE & .data$tmin < 32)

    # ,
    #   beg_grow = match(.data$gdd_count==7))#not work 7-15-21
    # ,
    #   grow_season_length = end_grow - beg_grow)
      #dplyr::select(.data$date, .data$yr, .data$gcm, .data$precip, .data$tmax, .data$tmin, .data$rhmax, .data$rhmin, .data$tavg, paste(variables))

    #all warnings produced are related to heat index function

    readr::write_csv(thresholds, paste(SiteID, "thresholds.csv", sep = "_"))



}

calc_thresholds("SCBL", df, 2040, variables = c("season", "test"))
