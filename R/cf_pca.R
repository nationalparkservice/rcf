data <- method_cf_gcm

cf_pca <- function(SiteID,
                   data = NULL,
                   year,
                   variables = NULL,
                   num_cf = 4,
                   directory = tempdir()){

  if(!file.exists(".here")) here::set_here(directory)

  # ---------
  # subset data from entered dataframe
  # ---------


  pca <- data %>%
    #dplyr::select(.data$date, .data$yr, .data$gcm, paste(variables)) %>%
    tidyr::drop_na() %>%
    dplyr::select()
    base::scale() %>%
    stats::prcomp()

  # ---------
  # Create PCA
  # ---------




}
