

cf_pca <- function(SiteID, data = NULL, year, variables = NULL, num_cf = 4, units = "imperial"){

  # ---------
  # subset data from entered dataframe
  # ---------



  pca <- thresholds %>%
    #dplyr::select(.data$date, .data$yr, .data$gcm, paste(variables)) %>%
    tidyr::drop_na() %>%
    base::scale() %>%
    stats::prcomp()

  # ---------
  # Create PCA
  # ---------




}
