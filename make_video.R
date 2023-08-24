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

height_m <- 1000
crs_utm <- 32748
met_type <- 'gdas1'
bbox_mode <- c("plants_receptors")
folder <- "results/video"
folder_frames <- glue("results/video/frames_{height_m}")
folder_concentrations <- glue("results/video/concentrations_{height_m}")
folder_contours <- glue("results/video/contours_{height_m}")
folder_video <- glue("results/video/video_{height_m}")

dir.create(folder)
dir.create(folder_frames)
dir.create(folder_concentrations)
dir.create(folder_contours)
dir.create(folder_video)

duration_hours <- 120
duration_days <- ceiling(duration_hours / 24)
date_from <- as.Date("2023-08-01")
date_to <- as.Date("2023-08-08")
dates <- seq.Date(date_from, date_to, by = "day")

plants <- data.get_plants(as_sf = T)
receptors <- data.get_receptors(as_sf = T) %>%
  filter(grepl("Jakarta", Region))


# Proceed by range of days to manage memory
date_range <- seq(date_to - duration_days, by = "day", date_to)
dispersions <- NULL
contributions <- NULL
range_contributions <- NULL


while (min(date_range) >= date_from) {

  print(glue("{min(date_range)} ====> {date_from}"))


  # date_contribution <- if(is.null(range_contributions)) max(date_range) else unique(date(range_contributions$contributions$date_reception))
  date_contribution <- max(date_range)
  if(length(date_contribution) != 1){
    stop("Something wrong with dates")
  }

  filepath_contours <- glue("results/contours/contours_{met_type}_{height_m}m_{bbox_mode}_withrasters_{gsub('-','',date_contribution)}.RDS")
  filepath_contributions <- glue("results/contributions/contributions_{met_type}_{height_m}m_{bbox_mode}_withrasters_{gsub('-','',date_contribution)}.RDS")

  if(file.exists(filepath_contours)){
    contours <- readRDS(filepath_contours)
  }else{

    if(file.exists(filepath_contributions)){
      range_contributions <- readRDS(filepath_contributions)
    }else{

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
          parallel=T,
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

      if(!all(unique(date(range_contributions$contributions$date_reception)) == date_contribution)){
        stop("Something wrong with dates")
      }

      print("=== saving ===")
      saveRDS(range_contributions, filepath_contributions)
    }

    print("=== raster to contours ===")
    contours <- rasters_to_contours(rasters=range_contributions$rasters, parallel=F)
    saveRDS(contours, filepath_contours)
  }

  print("=== plotting contours ===")
  alpha_power <- c(0.3)
  alpha_min <- c(0.2)
  conc_min <- c(1, 5)
  conc_max <- c(400)
  breaks_power <- c(0.1, 0.2)


  # Plot for each combination
  for (i in 1:length(alpha_power)) {
    for (j in 1:length(alpha_min)) {
      for (k in 1:length(conc_max)) {
        for (l in 1:length(conc_min)) {
          for (m in 1:length(breaks_power)) {
            folder_contours_ <- glue("results/video/contours_{height_m}/{alpha_power[i]}_{alpha_min[j]}_{conc_min[l]}_{conc_max[k]}_{breaks_power[m]}")
            plot_contribution_contours(
              contours=contours,
              plants = plants,
              receptors=receptors,
              folder = folder_contours_,
              buffer_km=50,
              force=F,
              conc_min = conc_min[l],
              conc_max = conc_max[k],
              alpha_power = alpha_power[i],
              alpha_min = alpha_min[j],
              breaks_power = breaks_power[m]
            )
          }
        }
      }
    }
  }

  # plot_contribution_contours(
  #   contours=contours,
  #   plants = plants,
  #   receptors=receptors,
  #   folder = folder_contours,
  #   buffer_km=50,
  #   force=T,
  #   conc_min = 10,
  #   conc_max = 300
  # )

  # print("=== creating frames ===")
  # create_frames(folder_frames=folder_frames,
  #               folder_contours=folder_contours,
  #               folder_concentrations=folder_concentrations,
  #               date_from=date_from,
  #               force=T)
  # Move date_range by one day
  date_range <- date_range - 1


}


folders_contours <- list.files(glue("results/video/contours_{height_m}"), full.names = T)
pbapply::pblapply(folders_contours, function(folder_contours){
  folder_frames = gsub("contours", "frames", folder_contours)
  folder_video = gsub("contours", "video", folder_contours)
  dir.create(folder_frames, recursive = T)
  dir.create(folder_video, recursive = T)
  create_frames(folder_frames=folder_frames,
                folder_contours=folder_contours,
                folder_concentrations=folder_concentrations,
                date_from=date_from, force=T)

  create_videos(folder_frames = folder_frames,
                folder_contours = folder_contours,
                folder_concentrations = folder_concentrations,
                folder_video = folder_video)
})


# Move videos to the same folder, and rename them with the suffix of the folder
pbapply::pblapply(folders_contours, function(folder_contours){
  folder_video = gsub("contours", "video", folder_contours)
  files <- list.files(folder_video, full.names = T)
  for(file in files){
    # Take the last folder, add it to the name of the video
    folder_new <- gsub('\\.','',folder_contours)
    extension <- paste0(".", tools::file_ext(file))
    file_new <- glue("{gsub(extension,'',basename(file))}_{basename(folder_new)}{extension}")
    file.copy(file, file.path("results/video", file_new))
  }
})



# create_frames(folder_frames=folder_frames,
#               folder_contours=folder_contours,
#               folder_concentrations=folder_concentrations,
#               date_from=date_from, force=T)
#
# create_videos(folder_frames = folder_frames,
#               folder_contours = folder_contours,
#               folder_concentrations = folder_concentrations,
#               folder_video = folder_video)

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

# plot_concentrations(folder=file.path(folder,"concentrations"), date_from="2023-01-01")

