

cf_pca <- function(SiteID, data = NULL, year, variables = NULL, num_cf = 4, units = "imperial"){

  # ---------
  # subset data from entered dataframe
  # ---------



  df <- data %>%
    dplyr::select(.data$date, .data$year, .data$gcm, paste(variables))

  # ---------
  # Create PCA
  # ---------




}
