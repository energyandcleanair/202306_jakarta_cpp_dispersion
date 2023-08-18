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
bbox_mode <- c("plant_receptors")
folder <- "results/video"
duration_hours <- 120
duration_days <- ceiling(duration_hours / 24)
date_from <- as.Date("2023-07-01")
date_to <- as.Date("2023-08-10")
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

   crs_utm <- 32748
   met_type <- 'gdas1'
   bbox_mode <- c("plant_receptors")
   folder <- "results/video"
   duration_hours <- 120
   duration_days <- ceiling(duration_hours / 24)
   date_from <- as.Date("2023-07-01")
   date_to <- as.Date("2023-08-10")
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

     # Move date_range by one day
     date_range <- date_range - 1

     # Overwrite every day... for debugging purposes
     saveRDS(contributions, glue("results/contributions_{met_type}_{height_m}m_{bbox_mode}_withrasters.RDS"))
   }



















   #
   #
   #
   #
   # print("========= GET DISPERSIONS ==========")
   # dispersions <- get_dispersions(
   #     plants = plants,
   #     dates = dates,
   #     cache_only = T
   # )
   #
   # # saveRDS(dispersions, "cache/dispersions.RDS")
   # # dispersions <- readRDS("cache/dispersions.RDS")
   #
   # print("========= GET CONTRIBUTION ==========")
   # contributions <- get_contributions(
   #   dispersions = dispersions,
   #   plants = plants,
   #   receptors = receptors,
   #   height_m = 10,
   #   density_res = 1000,
   #   bbox_mode = bbox_mode,
   #   return_rasters=T,
   #   parallel=T,
   #   cache_folder='cache',
   #   use_cache=T
   # )
   #
   # # Clear memory
   # rm(dispersions)
   # gc()
   # saveRDS(contributions, "cache/contributions.RDS")
   # contributions <- readRDS("cache/contributions.RDS")


   contours <- rasters_to_contours(rasters=contributions$rasters, parallel=F)

   plot_contribution_contours(
     contours=contours,
     plants = plants,
     receptors=receptors,
     folder = folder,
     buffer_km=50,
     force=T,
     conc_min = 10,
     conc_max = 100
   )

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

   crs_utm <- 32748
   met_type <- 'gdas1'
   bbox_mode <- c("plant_receptors")
   folder <- "results/video"
   duration_hours <- 120
   duration_days <- ceiling(duration_hours / 24)
   date_from <- as.Date("2023-07-01")
   date_to <- as.Date("2023-08-10")
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

     range_contributions <- get_contributions(
       dispersions = dispersions,
       plants = plants,
       receptors = receptors,
       height_m = 100,
       density_res = 1000,
       bbox_mode = bbox_mode,
       return_rasters=T,
       parallel=T,
       cache_folder='cache',
       force=F
     )

     contours <- rasters_to_contours(rasters=range_contributions$rasters, parallel=F)

     plot_contribution_contours(
       contours=contours,
       plants = plants,
       receptors=receptors,
       folder = folder,
       buffer_km=50,
       force=T,
       conc_min = 10,
       conc_max = 100
     )

     contributions <- flatten_contributions(
       list(contributions, range_contributions),
       return_rasters=T)

     # Move date_range by one day
     date_range <- date_range - 1

     # Overwrite every day... for debugging purposes
     saveRDS(contributions, glue("results/contributions_{met_type}_{height_m}m_{bbox_mode}_withrasters.RDS"))
   }





   #
   #
   #
   #
   # print("========= GET DISPERSIONS ==========")
   # dispersions <- get_dispersions(
   #     plants = plants,
   #     dates = dates,
   #     cache_only = T
   # )
   #
   # # saveRDS(dispersions, "cache/dispersions.RDS")
   # # dispersions <- readRDS("cache/dispersions.RDS")
   #
   # print("========= GET CONTRIBUTION ==========")
   # contributions <- get_contributions(
   #   dispersions = dispersions,
   #   plants = plants,
   #   receptors = receptors,
   #   height_m = 10,
   #   density_res = 1000,
   #   bbox_mode = bbox_mode,
   #   return_rasters=T,
   #   parallel=T,
   #   cache_folder='cache',
   #   use_cache=T
   # )
   #
   # # Clear memory
   # rm(dispersions)
   # gc()
   # saveRDS(contributions, "cache/contributions.RDS")
   # contributions <- readRDS("cache/contributions.RDS")


   contours <- rasters_to_contours(rasters=contributions$rasters, parallel=F)

   plot_contribution_contours(
     contours=contours,
     plants = plants,
     receptors=receptors,
     folder = folder,
     buffer_km=50,
     force=T,
     conc_min = 10,
     conc_max = 100
   )

  contributions <- flatten_contributions(
    list(contributions, range_contributions),
    return_rasters=T)

  # Move date_range by one day
  date_range <- date_range - 1

  # Overwrite every day... for debugging purposes
  saveRDS(contributions, glue("results/contributions_{met_type}_{height_m}m_{bbox_mode}_withrasters.RDS"))
}



















#
#
#
#
# print("========= GET DISPERSIONS ==========")
# dispersions <- get_dispersions(
#     plants = plants,
#     dates = dates,
#     cache_only = T
# )
#
# # saveRDS(dispersions, "cache/dispersions.RDS")
# # dispersions <- readRDS("cache/dispersions.RDS")
#
# print("========= GET CONTRIBUTION ==========")
# contributions <- get_contributions(
#   dispersions = dispersions,
#   plants = plants,
#   receptors = receptors,
#   height_m = 10,
#   density_res = 1000,
#   bbox_mode = bbox_mode,
#   return_rasters=T,
#   parallel=T,
#   cache_folder='cache',
#   use_cache=T
# )
#
# # Clear memory
# rm(dispersions)
# gc()
# saveRDS(contributions, "cache/contributions.RDS")
# contributions <- readRDS("cache/contributions.RDS")


contours <- rasters_to_contours(rasters=contributions$rasters, parallel=F)

plot_contribution_contours(
  contours=contours,
  plants = plants,
  receptors=receptors,
  folder = folder,
  buffer_km=50,
  force=T,
  conc_min = 10,
  conc_max = 100
)

plot_concentrations(folder=folder, date_from="2023-07-10")

create_frames(folder=folder, date_from=date_from, force=T)


