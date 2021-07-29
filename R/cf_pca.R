

#' Title
#'
#' @param SiteID chosen name to use in file names, attributes, and
#'  directories. (character)
#' @param data Default dataset to use for the .csv files this function will create.
#' Follow vignette for example dataset creation. (data frame)
#' @param variables Variables you want the PCA to be based off of. Must match column names in
#' `data` parameter exactly. If running directly from `summarize_for_pca` and would like
#' to use all threshold values to select models, write "all_threshold"
#' @param num_cf Number of climate futures to select. Option of 2 or 4. Two models will be the
#' maximum and minimum values of principal component 1 (PC1), and 4 will be the maximum and minimun
#' values of principal component 2 (PC2)
#' @param directory where to save files to. Per CRAN guidelines, this
#' defaults to a temporary directory and files created will be lost after
#' R session ends. Specify a path to retain files.
#'
#' @return
#' One (1) csv file that has selected either 2 or 4 models, depending on `num_cf` chosen
#' based upon PCA
#' One (1) png file of the PCA scatterplot, with models labeled
#' @export
#'
#' @examples
#'
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
#' cf_pca("SCBL", data = data, year = 2040, variables = c("tmin", "tmax", "rhmin"),
#' num_cf = 2)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data


cf_pca <- function(SiteID,
                   data = NULL,
                   variables = NULL,
                   num_cf = 4,
                   directory = tempdir()){


  suppressMessages(if(!file.exists(".here")) here::set_here(directory))


  if(variables == "all_threshold"){variables = c("precip_daily", "tmin", "tmax", "tavg", "rhmin", "rhmax", "heat_index_ec", "heat_index_dan", "temp_over_95_pctl", "temp_over_99_pctl", "temp_over_95_pctl_length", "temp_under_freeze", "temp_under_freeze_length", "temp_under_5_pctl", "no_precip" , "no_precip_length", "precip_95_pctl", "precip_99_pctl", "precip_moderate", "precip_heavy", "freeze_thaw", "gdd", "gdd_count", "not_gdd_count", "frost", "grow_len")}


  # ---------
  # subset data from entered dataframe
  # ---------


  future_all <- data

  named_df <- data.frame(tibble::column_to_rownames(data, var = "gcm")) %>%
  # give rownames to variables for PCA
    #dplyr::select(tidyselect::where(~length(unique(.)) > 1)) %>%
  # remove columns that have all same values (cannot scale data if not)
    dplyr::select(paste(variables))

  # ---------
  # Create PCA
  # ---------

  cf_pca <- named_df %>%
    dplyr::select_if(~ !any(is.na(.))) %>%
   # base::scale() %>%
    stats::prcomp(center = TRUE, scale. = TRUE)

  # ------------
  # PCA SCATTERPLOT
  # ------------

  # AMR::ggplot_pca(cf_pca,
  #                 labels = rownames(named_df),
  #                 labels_textsize = 5) +
   pca_plot <- ggplot2::autoplot(cf_pca,
                    data = named_df,
                    loadings = TRUE,
                    label = TRUE) +
    ggplot2::theme_classic() + #has the L shape around the graph
    ggplot2::theme(panel.grid = ggplot2::element_blank(), #removes grid lines
                               plot.title = ggplot2::element_text(size = 25), #title of plot text size
                               legend.text = ggplot2::element_text(size = 15), #legend inside text size
                               legend.title = ggplot2::element_text(size = 18), #legend title text size
                               axis.title.y = ggplot2::element_text(size = 15), #changes y axis lable text size
                               axis.title.x = ggplot2::element_text(size = 15),#x axis text size
                               axis.text.x = ggplot2::element_text(size = 15), #changes x-axis text size
                               axis.text.y = ggplot2::element_text(size = 15), #changes y-axis text size
                               legend.position = "top")  #location of legend

  if(directory == "tempdir()"){print("Files have been saved to temporary directory and will be deleted when this R session is closed. To save locally, input where to save them into the `directory` argument.")}

  ggplot2::ggsave(here::here(directory,
                             paste0(SiteID, "_pca.png")),
                             width = 15, height = 9)



  # ---------------
  # MODEL SELECTION
  # ---------------

  pca_named<-as.data.frame(cf_pca$x)

  pc1_min <- rownames(pca_named)[which.min(pca_named$PC1)]
  pc1_max <- rownames(pca_named)[which.max(pca_named$PC1)]
  pc2_min <- rownames(pca_named)[which.min(pca_named$PC2)]
  pc2_max <- rownames(pca_named)[which.max(pca_named$PC2)]


  pc_models <- data.frame(pc1_max, pc1_min, pc2_max, pc2_min) %>%
    dplyr::rename(`PC1 Max` = .data$pc1_max,
                  `PC1 Min` = .data$pc1_min,
                  `PC2 Max` = .data$pc2_max,
                  `PC2 Min` = .data$pc2_min) %>%
    tidyr::pivot_longer(.data$`PC1 Max`:.data$`PC2 Min`,
                        names_to = "prcomp",
                        values_to = "gcm") %>%
    dplyr::filter(ifelse(num_cf == 2,
                         .data$prcomp %in% c("PC1 Max", "PC1 Min"),
                         .data$prcomp %in% c("PC1 Max", "PC1 Min", "PC2 Max", "PC2 Min")))

  pca_cf_gcm <- future_all %>%
    dplyr::full_join(pc_models, by = "gcm")

  readr::write_csv(pca_cf_gcm, here::here(directory,
                                          paste(SiteID,
                                                "future_means_pca.csv",
                                                sep = "_")))

}
