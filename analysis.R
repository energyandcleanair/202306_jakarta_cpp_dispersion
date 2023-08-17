options("rgdal_show_exportToProj4_warnings"="none")

library(tidyverse)
library(sf)
library(tictoc)
library(lubridate)
library(plotly)
library(basemaps)
library(creatrajs)
library(glue)
library(ggplot2)
library(data.table)
readRenviron(".Renviron")

source("./plot.R")
source("./contribution.R")
source("./utils.R")
source("./dispersion.R")
source("./data.R")
source("./diagnostics.R")

remove_small_cache_files(min_bytes=2.8 * 2^20)
plants <- data.get_plants(as_sf = T)
receptors <- data.get_receptors(as_sf = T) %>%
  filter(grepl("Jakarta", Region))
duration_hours <- 120
duration_days <- ceiling(duration_hours / 24)
date_from <- as.Date("2023-01-01")
date_to <- as.Date("2023-08-11")
# date_to <- get_latest_continous_date(plants = plants)
met_type <- 'gdas1'

dates <- seq.Date(date_from, date_to, by = "day")


# For memory reasons, we can only process a few dates at a time
# -> rolling date_range

# bbox_modes <- c("receptors", "indonesia")
# bbox_modes <- c("receptors") #, "indonesia")
bbox_modes <- c("plant_receptors")
height_ms <- c(10, 50)

for (bbox_mode in bbox_modes) {
  for (height_m in height_ms){

    date_range <- seq(date_to - duration_days, by = "day", date_to)
    dispersions <- NULL
    contributions <- NULL

    while (min(date_range) >= date_from) {
      print(glue("{min(date_range)} ====> {date_from}"))
      missing_dates <- if (is.null(dispersions)) {
        date_range
      } else {
        # all dates that are not in dispersions$date_reception
        date_range[!date_range %in% as.Date(dispersions$date_emission)]
      }

      dispersions <- bind_rows(
        dispersions,
        get_dispersions(
          plants = plants,
          dates = missing_dates,
          cache_only = F,
          met_type=met_type
        )
      ) %>%
        filter(as.Date(date_emission) %in% date_range)


      # Plot dispersions
      # plot_dispersions(
      #   dispersions = dispersions,
      #   plants = plants,
      #   dates = dates,
      #   folder = "results"
      # )

      # Diagnostics
      # diagnose_dispersions(dispersions)

      # Cache
      # saveRDS(dispersions, 'cache/dispersions.RDS')
      # dispersions <- readRDS('cache/dispersions.RDS')

      range_contributions <- get_contributions(
        dispersions = dispersions,
        plants = plants %>% mutate(emissions_t=1),
        receptors = receptors,
        height_m = height_m,
        density_res = 1000,
        bbox_mode = bbox_mode,
        parallel=T,
        return_rasters = F
      )

      contributions <- bind_rows(contributions, range_contributions)

      # Move date_range by one day
      date_range <- date_range - 1

      # Overwrite every day... for debugging purposes
      saveRDS(contributions, glue("results/normalised_contributions_{met_type}_{height_m}m_{bbox_mode}.RDS"))
    }
  }
}


# contributions_receptors <- readRDS('results/contributions_receptors.RDS')
# contributions_indonesia <- readRDS('results/contributions_indonesia.RDS')
#
# plot_contributions(contributions_receptors, suffix="_receptors")
# plot_contributions(contributions_indonesia, suffix="_indonesia")
