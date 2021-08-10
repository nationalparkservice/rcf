#' Download MACA data from the cft package
#'
#' Downloads data from the cft package (https://www.earthdatascience.org/cft/)
#' to your parent directory.
#'
#' @param SiteID  chosen name to use in file names, attributes, and
#'  directories. (character)
#' @param latitude latitude of point of interest (spatial)
#' @param longitude longitude of point of interest (spatial)
#' @param units the unit type that will be used,
#' defaults to "imperial" ("imperial" or "metric")
#' @param directory where to save files to. Per CRAN guidelines, this
#' defaults to a temporary directory and files created will be lost after
#' R session ends. Specify a path to retain files.
#'
#' @return one data frame - SiteID.csv - to be used
#' with other functions in this package(tibble)
#' @export
#'
#' @examples
#' \dontrun{
#' rcf_data(SiteID = "SCBL", latitude = 41.83476, longitude =  -103.707)
#'}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data

rcf_data <- function(SiteID,
                     latitude,
                     longitude,
                     units = "imperial",
                     directory = tempdir()){

  suppressMessages(if(!file.exists(".here")) here::set_here(directory))

  # Now can only use spatial object (not park name)
  Site_coordinates <- data.frame(PARK=SiteID,lat=latitude,lon=longitude)
  sp::coordinates(Site_coordinates) <- ~lon+lat
  sp::proj4string(Site_coordinates) <- "+proj=longlat +datum=NAD83 +no_defs " #same proj4string used in NPS_boundary_centroids.shp

  # download data
  file_refs <- cft::cftdata(aoi = Site_coordinates,
                       area_name = SiteID,
                       years = c(1950,2099),
                       models = c('bcc-csm1-1','bcc-csm1-1-m','BNU-ESM','CanESM2','CCSM4',
                                  'CNRM-CM5','CSIRO-Mk3-6-0','GFDL-ESM2G','GFDL-ESM2M',
                                  'HadGEM2-CC365','HadGEM2-ES365', 'inmcm4','IPSL-CM5A-MR',
                                  'IPSL-CM5A-LR','IPSL-CM5B-LR', 'MIROC5','MIROC-ESM',
                                  'MIROC-ESM-CHEM','MRI-Cgcm3','NorESM1-M'),
                       local_dir = directory,
                       parameters = c("pr", "tasmax", "tasmin","rhsmax","rhsmin"),
                       scenarios = c("rcp45", "rcp85"),
                       ncores = parallel::detectCores() / 2)

  df1 <- cft::cft_df(file_refs, ncores = parallel::detectCores() / 2)

  ######################## MANIPULATE INTO DF FOR PLOT_TABLE SCRIPT #####################
  if(units == "imperial"){

  df <- df1 %>%
    dplyr::mutate(precip = .data$pr/25.4,
                  # data source needs to be specified using rlang::.data
                  # error `no visible binding for global variable x` will be thrown
           tmax = .data$tasmax * (9/5) - 459.67,
           # decided to not label as tmax_f because it could cause conflicts
           # if want to name as tmax_f will need to incorporate if statements
           # to not cause any issues down the line if users choose "metric"
           tmin = .data$tasmin * (9/5) - 459.67,
           tavg = (.data$tmax + .data$tmin) / 2,
           gcm = paste(.data$model, .data$rcp, sep = "."),
           date = as.POSIXlt(.data$date,format="%Y-%m-%d"),
           yr = lubridate::year(date),
           units = "imperial") %>%
    dplyr::rename(rhmax = .data$rhsmax,
                  rhmin = .data$rhsmin) %>%
    dplyr::select(.data$gcm, .data$date, .data$yr, .data$precip, .data$tmin, .data$tmax, .data$tavg, .data$rhmin, .data$rhmax, .data$units)
  #rename to follow previously written code
  } # close imperial if statement

  if(units == "metric"){
    df <- df1 %>%
      dplyr::mutate(precip = .data$pr,
                    # data source needs to be specified using rlang::.data
                    # error `no visible binding for global variable x` will be thrown
             tmax = .data$tasmax - 273.1,
             tmin = .data$tasmin - 273.1,
             tavg = (.data$tmax + .data$tmin) / 2,
             gcm = paste(.data$model, .data$rcp, sep = "."),
             date = as.POSIXlt(.data$date,format="%Y-%m-%d"),
             yr = lubridate::year(date),
             units = "metric") %>%
      dplyr::rename(rhmax = .data$rhsmax,
                  rhmin = .data$rhsmin) %>%
      dplyr::select(.data$gcm, .data$date, .data$yr, .data$precip, .data$tmin, .data$tmax, .data$tavg, .data$rhmin, .data$rhmax, .data$units)
    #rename to follow previously written code
  } # close metric if statement

  if(directory == "tempdir()"){warning("Files have been saved to temporary directory and will be deleted when this R session is closed. To save locally, input a local directory in which to save files into the `directory` argument.")}

         readr::write_csv(df, here::here(directory,
                                  paste0(SiteID, ".csv")))


    do.call(file.remove, list(list.files(paste(directory,SiteID,sep="/"), full.names = TRUE)))
    print("Files from cft package removed. Raw data has been saved into a large csv. DO NOT edit this csv in excel, it will cause data loss and miscalculations or malfunctions the proceeding functions.")

} #close function
