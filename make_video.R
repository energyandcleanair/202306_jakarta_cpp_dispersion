options("rgdal_show_exportToProj4_warnings"="none")
options(mc.cores = parallel::detectCores()-3)

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
library(RColorBrewer)
library(ggnewscale)
library(data.table)
library(raster)
library(terra)

readRenviron(".Renviron")
source("./plot.R")
source("./contribution.R")
source("./utils.R")
source("./dispersion.R")
source("./data.R")
source("./diagnostics.R")
source("./video.R")
source("./contour.R")

height_m <- 100
crs_utm <- 32748
met_type <- 'gdas1'
bbox_mode <- c("plants_receptors")
folder <- "results/video"
folder_frames <- "results/video/frames"
folder_concentrations <- "results/video/concentrations"
folder_contours <- "results/video/contours"
folder_video <- "results/video/video"

dir.create(folder)
dir.create(folder_frames)
dir.create(folder_concentrations)
dir.create(folder_contours)
dir.create(folder_video)


duration_hours <- 120
duration_days <- ceiling(duration_hours / 24)
date_from <- as.Date("2023-01-01")
date_to <- as.Date("2023-07-21")
dates <- seq.Date(date_from, date_to, by = "day")

plants <- data.get_plants(as_sf = T)
receptors <- data.get_receptors(as_sf = T) %>%
  filter(grepl("Jakarta", Region))


# Proceed by range of days to manage memory
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

  print("=== get dispersions ===")
  dispersions <- bind_rows(
    dispersions,
    get_dispersions(
      plants = plants,
      dates = missing_dates,
      cache_only = T,
      met_type=met_type
    )
  ) %>%
    filter(as.Date(date_emission) %in% date_range)

  print("=== get contributions ===")
  range_contributions <- get_contributions(
    dispersions = dispersions,
    plants = plants,
    receptors = receptors,
    height_m = height_m,
    density_res = 1000,
    bbox_mode = bbox_mode,
    return_rasters=T,
    parallel=T,
    cache_folder='cache',
    force=F
  )

  contributions <- flatten_contributions(
    list(contributions, range_contributions),
    return_rasters=T)

  print("=== raster to contours ===")
  contours <- rasters_to_contours(rasters=range_contributions$rasters, parallel=F)

  print("=== plotting contours ===")
  plot_contribution_contours(
    contours=contours,
    plants = plants,
    receptors=receptors,
    folder = file.path(folder, "contours"),
    buffer_km=50,
    force=F,
    conc_min = 10,
    conc_max = 100
  )

  print("=== creating frames ===")
  create_frames(folder_frames=file.path(folder, "frames"),
                folder_contours=file.path(folder, "contours"),
                folder_concentrations=file.path(folder, "concentrations"),
                date_from=date_from, force=F)
  # Move date_range by one day
  date_range <- date_range - 1

  # Overwrite every day... for debugging purposes
  print("=== saving ===")
  date_contribution <- gsub("-","",paste0(unique(date(range_contributions$contributions$date_reception)), collapse="_"))
  saveRDS(range_contributions, glue("results/contributions/contributions_{met_type}_{height_m}m_{bbox_mode}_withrasters_{date_contribution}.RDS"))
}


create_frames(folder_frames=file.path(folder, "frames"),
              folder_contours=file.path(folder, "contours"),
              folder_concentrations=file.path(folder, "concentrations"),
              date_from=date_from, force=F)

create_videos(folder_frames = folder_frames,
              folder_contours = folder_contours,
              folder_video = folder_video)
# create_frames(folder_frames=file.path(folder, "frames"),
#               folder_contours=file.path(folder, "contours"),
#               folder_concentrations=file.path(folder, "concentrations"),
#               date_from=date_from, force=F)

#   contributions <- readRDS(glue("results/contributions_{met_type}_{height_m}m_{bbox_mode}_withrasters.RDS"))


#    plot_contribution_contours(
#      contours=contours,
#      plants = plants,
#      receptors=receptors,
#      folder = folder,
#      buffer_km=50,
#      force=T,
#      conc_min = 10,
#      conc_max = 100
#    )

plot_concentrations(folder=file.path(folder,"concentrations"), date_from="2023-01-01")

