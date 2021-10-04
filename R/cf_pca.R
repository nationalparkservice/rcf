#' Calculate climate futures using PCA
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
#' from dataframe in `data` parameter exactly. If running directly from
#' `summarize_for_pca` and would like to use all threshold values to select models,
#' write "all_threshold" (character)
#' @param num_cf Number of climate futures to select. Option of 2 or 4. Two models will be the
#' maximum and minimum values of principal component 1 (PC1), and 4 will be the maximum
#' and minimum values of principal component 2 (PC2)
#' @param directory where to save files to. Per CRAN guidelines, this
#' defaults to a temporary directory and files created will be lost after
#' R session ends. Specify a path to retain files.
#'
#' @return
#' One (1) csv file that has selected either 2 or 4 models, depending on `num_cf` chosen
#' based upon PCA
#' One (1) png file of the PCA scatterplot, with models labeled
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Generate sample data
#'
#' data <- data.frame(
#'  gcm = c("bcc-csm1-1.rcp45", "BNU-ESM.rcp45", "CanESM2.rcp85", "CCSM4.rcp45",
#'              "CSIRO-Mk3-6-0.rcp45"),
#'  precip_change = rnorm(5),
#'  tmin_change = rnorm(5),
#'  tmax_change = rnorm(5),
#'  rhmax_change = rnorm(5),
#'  rhmin_change = rnorm(5),
#'  tavg_change = rnorm(5),
#'  heat_index_change = rnorm(5),
#'  heat_index_ec_change = rnorm(5),
#'  heat_index_dan_change = rnorm(5),
#'  temp_over_95_pctl_change =  rnorm(5),
#'  temp_over_99_pctl_change =  rnorm(5),
#'  temp_over_95_pctl_length_change =  rnorm(5),
#'  temp_under_freeze_change =  rnorm(5),
#'  temp_under_freeze_length_change =  rnorm(5),
#'  temp_under_5_pctl_change =  rnorm(5),
#'  no_precip_change  =  rnorm(5),
#'  no_precip_length_change =  rnorm(5),
#'  precip_95_pctl_change =  rnorm(5),
#'  precip_99_pctl_change =  rnorm(5),
#'  precip_moderate_change =  rnorm(5),
#'  precip_heavy_change =  rnorm(5),
#'  freeze_thaw_change =  rnorm(5),
#'  gdd_change =  rnorm(5),
#'  gdd_count_change = rnorm(5),
#'  not_gdd_count_change = rnorm(5),
#'  frost_change = rnorm(5),
#'  grow_length_change = rnorm(5),
#'  units = rep("imperial", each = 5)
#' )
#'
#' cf_pca("SCBL", data = data, variables = c("tmin", "tmax", "rhmin"),
#' num_cf = 2)
#'}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @import ggfortify
#' @importFrom utils globalVariables
#'
#' @name cf_pca

utils::globalVariables("where")

cf_pca <- function(SiteID = "unnamed_site",
                   data = NULL,
                   variables = "all_threshold",
                   num_cf = 4,
                   directory = tempdir()){

  first_variable = variables[1]

  is_all_threshold <- ifelse(first_variable == "all_threshold", TRUE, FALSE)

  all_threshold = c("precip_change", "tmax_change", "tmin_change",  "tavg_change", "rhmin_change", "rhmax_change", "heat_index_change", "heat_index_ec_change", "heat_index_dan_change", "temp_over_95_pctl_change", "temp_over_99_pctl_change", "temp_over_95_pctl_length_change", "temp_under_freeze_change", "temp_under_freeze_length_change", "temp_under_5_pctl_change", "no_precip_change", "no_precip_length_change", "precip_95_pctl_change", "precip_99_pctl_change", "precip_moderate_change", "precip_heavy_change", "freeze_thaw_change", "gdd_change", "frost_change", "grow_length_change")

    function_variables = if(is_all_threshold == TRUE){paste(all_threshold)} else{paste(variables)}

  #if() else() didn't work here - I've gone back and forth between the two functions, both
  # ultimately throw the error "the condition has length > 1 and only the first
  # element will be used" cannot say why
  #create variables to be used for the function

  # stop function with error messages

  if(num_cf %in% c(2,4) == FALSE){
    stop("num_cf can only be 2 or 4. Did you enter a different number?")
  }

variables_df <- data %>%
  dplyr::select(where(is.numeric))

#!sum(variables %in% variables_df) == length(variables)

  if(!sum(function_variables %in% colnames(variables_df)) == length(function_variables)){
    #number of matches for column names in variables argument does not match the number of
    #variables entered, then a variable has been entered incorrectly
    stop("Variable names entered are not the same as the column names of your dataframe. Please check for spelling or case sensitive errors. You also may have entered variables that contain non-numeric values.")
  }


  suppressMessages(if(!file.exists(".here")) here::set_here(directory))

  # ---------
  # subset data from entered dataframe
  # ---------

  future_all <- data

  named_df2 <- data.frame(tibble::column_to_rownames(data, var = "gcm")) %>%
    dplyr::select(base::paste(function_variables))
    # give rownames to variables for PCA
  named_df1 <- named_df2 %>%
    dplyr::select_if(!is.na(base::colSums(named_df2)) == "TRUE")
  # remove columns with NA values

  # needs to be split into two statements because it's looking for the
  # number of columns in the original dataset and throws an error

  named_df <- named_df1 %>%
    dplyr::select_if(!base::colMeans(named_df1, na.rm = TRUE) == base::unlist(named_df1[1,]))
    #remove columns where the mean is the same as the first value

  # ---------
  # Create PCA
  # ---------

  cf_pca <- named_df %>% #remove columns with NA values
    dplyr::select_if(~ !any(is.na(.data))) %>%
    # remove columns with NA values
    stats::prcomp(center = TRUE, scale. = TRUE)

  #print warning for removed columns

  kept_columns <- data %>%
    dplyr::select_if(~ !any(is.na(.data)))

  removed_columns <- data %>%
    dplyr::select(!colnames(kept_columns))

  columns_empty <- length(colnames(removed_columns) == 0)

  if(columns_empty > 0L)print(paste("Removed", colnames(removed_columns), "due to NA or all duplicate values in column.", colnames(removed_columns), "was not used in PCA calculation."))




  # ------------
  # PCA SCATTERPLOT
  # ------------

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

  if(directory == tempdir()){warning("Files have been saved to temporary directory and will be deleted when this R session is closed. To save locally, input a local directory in which to save files into the `directory` argument.")}

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
                        names_to = "pca_type",
                        values_to = "gcm") %>%
    dplyr::filter(ifelse(num_cf == 2,
                         .data$pca_type %in% c("PC1 Max", "PC1 Min"),
                         .data$pca_type %in% c("PC1 Max", "PC1 Min", "PC2 Max", "PC2 Min")))

  pca_cf_gcm <- future_all %>%
    dplyr::full_join(pc_models, by = "gcm")

  readr::write_csv(pca_cf_gcm, here::here(directory,
                                          paste(SiteID,
                                                "future_means_pca.csv",
                                                sep = "_")))

  return(pca_cf_gcm)

}
