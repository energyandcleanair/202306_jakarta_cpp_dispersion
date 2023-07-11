library(tidyverse)
library(sf)
library(tictoc)
library(lubridate)
library(plotly)
library(basemaps)
library(creatrajs)
library(glue)
library(ggplot2)

readRenviron(".Renviron")

source("./plot.R")
source("./contribution.R")
source("./utils.R")
source("./dispersion.R")
source("./data.R")
source("./diagnostics.R")



plants <- data.get_plants(as_sf = T)
receptors <- data.get_receptors(as_sf = T)
duration_hours <- 120
duration_days <- ceiling(duration_hours / 24)
date_from <- as.Date("2023-01-01")
date_to <- get_latest_continous_date(plants = plants)
date_to <- "2023-01-31"

dates <- seq.Date(date_from, date_to, by = "day")
options("rgdal_show_exportToProj4_warnings" = "none")


# For memory reasons, we can only process a few dates at a time
# -> rolling date_range

bbox_modes <- c("receptors", "indonesia")

for (bbox_mode in bbox_modes) {

  date_range <- seq(date_from, date_from + duration_days, by = "day")
  dispersions <- NULL
  contributions <- NULL

  while (max(date_range) <= date_to) {
    print(glue("{max(date_range)} / {date_to}"))
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
        cache_only = T
      )
    ) %>%
      filter(as.Date(date_emission) %in% date_range)


    # Plot dispersions
    # plot_dispersions(
    #   dispersions = dispersions,
    #   plants = plants,
    #   dates = sample(dates, 3),
    #   folder = "results"
    # )

    # Diagnostics
    # diagnose_dispersions(dispersions)

    # Cache
    # saveRDS(dispersions, 'cache/dispersions.RDS')
    # dispersions <- readRDS('cache/dispersions.RDS')

    range_contributions <- get_contributions(
      dispersions = dispersions,
      plants = plants,
      receptors = receptors,
      height_m = 10,
      density_res = 1000,
      bbox_mode = bbox_mode
    )

    contributions <- bind_rows(contributions, range_contributions)

    # Move date_range by one day
    date_range <- date_range + 1

    # Overwrite every day... for debugging purposes
    saveRDS(contributions, glue("results/contributions_{bbox_mode}.RDS"))

  }
}


contributions_receptors <- readRDS('results/contributions_receptors.RDS')
contributions_indonesia <- readRDS('results/contributions_indonesia.RDS')

plot_contributions(contributions_receptors, suffix="_receptors")
plot_contributions(contributions_indonesia, suffix="_indonesia")
