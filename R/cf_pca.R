#' Calculate climate futures using PCA*
#'
#' Designates climate futures of "Warm Wet", "Warm Dry", "Hot Wet", "Hot Dry" and "Central"
#' and calculates mean values for each GCM. Will additionally calculate which models
#' represent the largest spread in data of variables entered using PCA. Calculates
#' summary of threshold values based off of selected summarization parameter and PCA.
#'
#' *For advanced users only.
#'
#' @param SiteID chosen name to use in file names, attributes, and directories. Default
#' name is "unnamed_site" (character)
#' @param data Default dataset to use for the .csv files this function will create.
#' Follow vignette for example dataset creation. (data frame)
#' @param variables Variables you want the PCA to be based off of. Must match column names
#'  from dataframe in `data` parameter exactly. If running directly from
#'  `summarize_for_pca` and would like to use all threshold values to select models,
#'  write "all_threshold" (character)
#' @param num_cf Number of climate futures to select. Option of 2 or 4. Two models will be the
#' maximum and minimum values of principal component 1 (PC1), and 4 will be the maximum
#' and minimun values of principal component 2 (PC2)
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
#' grow_length = rnorm(100)
#' )
#'
#' cf_pca("SCBL", data = data, variables = c("tmin", "tmax", "rhmin"),
#' num_cf = 2)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data


cf_pca <- function(SiteID = "unnamed_site",
                   data = NULL,
                   variables = "all_threshold",
                   num_cf = 4,
                   directory = tempdir()){


  suppressMessages(if(!file.exists(".here")) here::set_here(directory))


  if(variables == "all_threshold"){variables = c("precip_change", "tmin_change", "tmax_change", "tavg_change", "rhmin_change", "rhmax_change", "heat_index_ec_change", "heat_index_dan_change", "temp_over_95_pctl_change", "temp_over_99_pctl_change", "temp_over_95_pctl_length_change", "temp_under_freeze_change", "temp_under_freeze_length_change", "temp_under_5_pctl_change", "no_precip_change", "no_precip_length_change", "precip_95_pctl_change", "precip_99_pctl_change", "precip_moderate_change", "precip_heavy_change", "freeze_thaw_change", "gdd_change", "frost_change", "grow_length_change")}


  # ---------
  # subset data from entered dataframe
  # ---------


  future_all <- data

  named_df <- data.frame(tibble::column_to_rownames(data, var = "gcm")) %>%
  # give rownames to variables for PCA
    dplyr::select(paste(variables))

  # ---------
  # Create PCA
  # ---------

  cf_pca <- named_df %>%
    dplyr::select_if(~ !any(is.na(.))) %>% #remove columns with NA values
   # base::scale() %>%
    stats::prcomp(center = TRUE, scale. = TRUE)

  #print warning for removed columns

  kept_columns <- data %>%
    dplyr::select_if(~ !any(is.na(.)))

  removed_columns <- data %>%
    dplyr::select(!colnames(kept_columns))

  columns_empty <- length(colnames(removed_columns) == 0)

  if(columns_empty > 0L)print(paste("Removed", colnames(removed_columns), "due to NA values in column.", colnames(removed_columns), "was not used in PCA calculation."))




  # ------------
  # PCA SCATTERPLOT
  # ------------

  # AMR::ggplot_pca(cf_pca,
  #                 labels = rownames(named_df),
  #                 labels_textsize = 5) +
   pca_plot <- ggplot2::autoplot(cf_pca,
                    data = named_df,
                    loadings = TRUE,
                    loadings.label = TRUE,
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
