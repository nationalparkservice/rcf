# Reproducible Climate Futures (rcf)

## Overview

This package aims to make acquiring and working with [MACA v2](http://www.climatologylab.org/maca.html) climate data faster and easier and to provide a number of summary statistics that can be used to visualize different climate futures. Ultimately, having access to this data supports planning efforts that aim to incorporate climate change.

## Installation

Until approval on CRAN, you can download the development version of `rcf`

```{r}
install.pagages("devtools")
devtools::install_github("nationalparkservice/rcf")

library(tidyverse)
library(rcf)
```
## Usage

Download data using the `rcf_data()` function to start visualizing climate futures

```{r}
raw_data <- rcf_data(SiteID = "BAND",
                     latitude = 35.75758546,
                     longitude = -106.3054344,
                     units = "imperial")
```

Calculate threshold values using `calc_thresholds()` and summarize them by month, season or year as well as by quadrant or the most extreme model in each quadrant usind `cf_quadrant()`.

```{r}
thresholds <- calc_thresholds("BAND", data = raw_data, units = "imperial")

quadrant_year <- cf_quadrant("BAND", data = thresholds, future_year = 2040, summarize_by = "year", method = "quadrant")
```

From here we can use `ggplot` to visualize any variables and how they compare between the 4 climate futures as well as between past and future.

```{r}
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
       title = "Days that have a freeze thaw cycle by climate future")
  theme(axis.title.x = element_blank())
```
## Explore further

For a more in-depth explanation of the `rcf` package and different ways to download the data, you can follow along with [An Introduction to the Reproducible Climate Futures package](https://github.com/nationalparkservice/rcf/tree/main/vignettes).

## Data

```{r, echo=FALSE, message=FALSE, warning=FALSE}
library(kableExtra)

names_table <- data.frame(name = c("Minimum temperature", "Maximum temperature", "Precipitation", "Minimum relative humidity", "Maximum relative humidity", "Date", "Year", "Month", "Day of Year", "Heat Index", "Heat Index Extreme Caution", "Heat Index Dangerous", "Temperature over the 95th percentile", "Temperature over the 99th percentile", "95th percentile length", "Temperature below freezing", "Below freezing length", "Temperature below the 5th percentile", "No precipitation", "No Precipitation Length", "Precipitation above the 95th percentile", "Precipitation above the 99th percentile", "Moderate Precipitation", "Heavy precipitation", "Freeze thaw", "Growing degree day", "Growing degree day length", "Non growing degree day length", "Frost", "Growing season length"), df_name = c("tmin", "tmax", "precip", "rhmin", "rhmax", "date", "yr", "month", "doy", "heat_index", "heat_index_ec", "heat_index_dan", "temp_over_95_pctl", "temp_over_99_pctl", "temp_over_95_pctl_length", "temp_under_freeze", "temp_under_freeze_length", "temp_under_5_pctl", "no_precip", "no_precip_length", "precip_95_pctl", "precip_99_pctl", "precip_moderate", "precip_heavy", "freeze_thaw", "gdd", "gdd_count", "not_gdd_count", "frost", "grow_length"), calculation = c("Downloaded data", "Downloaded data", "Downloaded data", "Downloaded data", "Downloaded data", "Downloaded data", "Downloaded data", "Downloaded data", "Downloaded data", "Heat index calculation", "Temperature between 90-102F", "Temperature between 103-124F", "Temperature exceeds historic 95th pctl", "Temperature exceeds historic 99th pctl", "Consecutive days with temperature over the 95th percentile", "Temperature below 32F or 0C", "Consecutive days with temperature below freezing", "Temperature falls below historic 5th pctl", "Precipitation less than 0.04 in or 1 mm", "Consecutive days with no precipitation", "Precipitation exceeds historic 95th pctl", "Precipitation exceeds historic 99th pctl", "Precipitation greater than 1 in or 25 mm", "Precipitation greater than 2 in or 50 mm", "Minimum temperature below 28F or -2.2C, maximum temperature above 34F or 1.1C", "Temperature exceeds 41F or 5C", "Consecutive growing degree days","Consecutive non growing degree days", "Growing degree day with minimum temperature below 32F or 0C", "Growing season length"))

names_table %>% 
  kable(col.names = c("Variable Name",
                      "Column Name",
                      "Variable Calculation")) %>% 
# Column names - can't change row names in kable.
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = F,
                position = "center") %>% 
  column_spec(3, width = "20em")
# Full width (F), centered table.

```

