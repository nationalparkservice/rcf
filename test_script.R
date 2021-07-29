## script to test package

my_directory <- "C:/Users/jnchr/Documents/test"
raw_data <- readr::read_csv('C:/Users/jnchr/Documents/test/BAND.csv')

# ----------------
# CALC THRESHOLDS
# ----------------

calc_thresholds("BAND", data = raw_data, directory = my_directory, units = "imperial")

thresholds <- readr::read_csv('C:/Users/jnchr/Documents/test/BAND_thresholds.csv')

# ----------------
# CF QUADRANT
# ----------------

cf_quadrant("BAND", data = thresholds, year = 2040, summarize_by = "year", method = "quadrant", directory = my_directory)

cf_quadrant("BAND", data = thresholds, year = 2040, summarize_by = "month", method = "quadrant", directory = my_directory)

cf_quadrant("BAND", data = thresholds, year = 2040, summarize_by = "season", method = "quadrant", directory = my_directory)

cf_quadrant("BAND", data = thresholds, year = 2040, summarize_by = "year", method = "corner", directory = my_directory)

cf_quadrant("BAND", data = thresholds, year = 2040, summarize_by = "month", method = "corner", directory = my_directory)

cf_quadrant("BAND", data = thresholds, year = 2040, summarize_by = "season", method = "corner", directory = my_directory)

# -------------
# SUMMARIZE FOR PCA
# -------------

summarize_for_pca("BAND", data = thresholds, 2040, directory = my_directory)

pca_summary <- readr::read_csv('C:/Users/jnchr/Documents/test/BAND_pca_summary.csv')

# -------------
# CF PCA
# -------------

cf_pca("BAND", data = pca_summary, variables = "all_threshold", directory = my_directory)

pca_data <- readr::read_csv('C:/Users/jnchr/Documents/test/BAND_future_means_pca.csv')

# -------------
# PCA THRESHOLDS
# -------------

pca_thresholds("BAND", pca_data = pca_data, all_data = thresholds, year = 2040, summarize_by = "year", directory = my_directory)

pca_thresholds("BAND", pca_data = pca_data, all_data = thresholds, year = 2040, summarize_by = "month", directory = my_directory)

pca_thresholds("BAND", pca_data = pca_data, all_data = thresholds, year = 2040, summarize_by = "season", directory = my_directory)

# variables if needed
# year = 2040
# rh_exists <-  any(names(data) == "rhmin")
# data <- pca_summary
# all_data <- thresholds
# variables = c("precip_daily", "tmin", "tmax", "tavg", "rhmin", "rhmax", "heat_index_ec", "heat_index_dan", "temp_over_95_pctl", "temp_over_99_pctl", "temp_over_95_pctl_length", "temp_under_freeze", "temp_under_freeze_length", "temp_under_5_pctl", "no_precip" , "no_precip_length", "precip_95_pctl", "precip_99_pctl", "precip_moderate", "precip_heavy", "freeze_thaw", "gdd", "gdd_count", "not_gdd_count", "frost", "grow_len")
