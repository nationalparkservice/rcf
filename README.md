
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

# Reproducible Climate Futures (rcf)

## Overview

\[Amber to fill in need for divergent, plausible, relevant CFs, with
citation to Lawrence et al. for justification for this approach and
methodology. Include brief description of 3 approaches for generating
CFs and when they should be used.\]

This package aims to make acquiring and working with CMIP5 [MACA
v2-METDATA](http://www.climatologylab.org/maca.html) downscaled climate
data faster and easier and to provide a number of summary statistics
that can be used to visualize different climate futures. Ultimately,
having access to this data supports planning efforts that aim to
incorporate climate change.

## Installation

Until approval on CRAN, you should download the development version of
`rcf`

You can install the released version of rcf from
[CRAN](https://CRAN.R-project.org) with:

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nationalparkservice/rcf")
```

<!-- Once CRAN is approved, provide instructions for installation from CRAN, for now commented out -->
<!-- ``` r -->
<!-- install.packages("rcf") -->
<!-- ``` -->

``` r
library(tidyverse)
#> -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
#> v ggplot2 3.3.5     v purrr   0.3.4
#> v tibble  3.1.4     v dplyr   1.0.7
#> v tidyr   1.1.3     v stringr 1.4.0
#> v readr   2.0.1     v forcats 0.5.1
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(rcf)
```

## Usage

The first step in creating climate futures is downloading downscaled
climate data for your location of interest. This package uses the [cft
package](https://github.com/earthlab/cft), created by the North Central
Climate Adaptation Science Center and EarthLab. Data can be downloaded
from a specific point by inputing coordinates into the `rcf_data()`
function. If you would like to download and summarize spatial explicit
data (i.e. from multiple grid cells) or would like more information on
the cft package, there is a detailed vignette on the project GitHub
page.

*Note: check for cft package updates periodically.*

Depending on internet connections and processing power, download time
for a single grid cell averages about 80 minutes. The simplest and
fastest method for generating climate futures is to download a single
grid cell using the `rcf_data()` function.

``` r
# raw_data <- rcf_data(SiteID = "BAND",
#                      latitude = 35.75758546,
#                      longitude = -106.3054344,
#                      units = "imperial")
```

Data used in this vignette can be downloaded
[here](https://irmadev.nps.gov/DataStore/Reference/Profile/2286572) and
read in using the following code:

``` r
data_file_location <- "Directory where you stored data"
raw_data <- read.csv(paste0(raw_file_location,"/BAND.csv"))
```

Calculate threshold values using `calc_thresholds()` and summarize them
by month, season or year as well as by quadrant or the most extreme
model in each quadrant using `cf_quadrant()`.

``` r
thresholds <- calc_thresholds("BAND", data = raw_data, units = "imperial")
#> Adding missing grouping variables: `gcm`
#> Warning in calc_thresholds("BAND", data = raw_data, units = "imperial"): Files
#> have been saved to temporary directory and will be deleted when this R session
#> is closed. To save locally, input a local directory in which to save files into
#> the `directory` argument.
#> Warning in calc_thresholds("BAND", data = raw_data, units = "imperial"):
#> thresholds.csv generated successfully. DO NOT edit this csv in excel. File is
#> too large and data will be lost, causing errors in future calculations.
quadrant_year <- cf_quadrant("BAND", data = thresholds, future_year = 2040, summarize_by = "year", method = "quadrant")
#> Warning in cf_quadrant("BAND", data = thresholds, future_year = 2040,
#> summarize_by = "year", : Files have been saved to temporary directory and
#> will be deleted when this R session is closed. To save locally, input a local
#> directory in which to save files into the `directory` argument.
```

From here we can use `ggplot2` to visualize any variables and how they
compare between the 4 climate futures as well as between past and
future.

``` r
quadrant_year_future <- quadrant_year %>%
filter(time %in% c("Future"))

ggplot(data = quadrant_year_future, aes(x = cf, y = freeze_thaw)) +
geom_boxplot(alpha = 0.4,
             aes(color = cf, fill = cf)) +
geom_jitter(alpha = 0.7,
            aes(color = cf, fill = cf)) +
scale_fill_viridis_d() +
scale_color_viridis_d() +
  labs(y = "Number of days per year",
       title = "Days that have a freeze thaw cycle by climate future") +
  theme(axis.title.x = element_blank()) +
  theme_minimal()
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="110%" />

## Explore further

For a more in-depth explanation of the `rcf` package and different ways
to download the data, you can follow along with An Introduction to the
Reproducible Climate Futures package(INSERT LINK).

## Data

<img src="man/figures/kable_table.png" width="90%" style="display: block; margin: auto;" />
