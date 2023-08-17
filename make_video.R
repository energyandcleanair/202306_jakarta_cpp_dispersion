library(tidyverse)
library(sf)
library(tictoc)
library(lubridate)
library(plotly)
library(basemaps)
library(creatrajs)
library(glue)
library(ggplot2)
library(magick)
library(rcrea)
library(ggdark)

readRenviron(".Renviron")
source("./plot.R")
source("./contribution.R")
source("./utils.R")
source("./dispersion.R")
source("./data.R")
source("./diagnostics.R")
source("./video.R")

options(mc.cores = parallel::detectCores()-2)
crs_utm <- 32748

plants <- data.get_plants(as_sf = T)
receptors <- data.get_receptors(as_sf = T) %>%
  filter(grepl("Jakarta", Region))

duration_hours <- 120
duration_days <- ceiling(duration_hours / 24)
date_from <- as.Date("2023-07-01")
date_to <- as.Date("2023-07-10")
# date_to <- get_latest_continous_date(plants = plants)
# date_to <- "2023-01-31"

dates <- seq.Date(date_from, date_to, by = "day")
options("rgdal_show_exportToProj4_warnings" = "none")

bbox_mode <- c("plant_receptors")
folder <- "results/video"

dispersions <- get_dispersions(
    plants = plants,
    dates = dates,
    cache_only = T
)

# saveRDS(dispersions, "cache/dispersions.RDS")
# dispersions <- readRDS("cache/dispersions.RDS")

contributions <- get_contributions(
  dispersions = dispersions,
  plants = plants,
  receptors = receptors,
  height_m = 10,
  density_res = 1000,
  bbox_mode = bbox_mode,
  return_rasters=T,
  parallel=F
)
#
# saveRDS(contributions, "cache/contributions.RDS")
contributions <- readRDS("cache/contributions.RDS")

rasters <- contributions$rasters

plot_contribution_contours(
  contours=contours,
  plants = plants,
  receptors=receptors,
  dates = NULL,
  bbox_mode = bbox_mode,
  folder = folder,
  buffer_km=1,
  force=F
)

plot_concentrations(folder=folder, date_from=date_from)

create_frames(folder=folder, date_from=date_from, force=T)


