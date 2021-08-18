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
