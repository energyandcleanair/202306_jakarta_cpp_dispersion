library(tidyverse)
library(sf)
library(tictoc)
library(lubridate)
library(plotly)
library(basemaps)
library(creatrajs)

source('./plot.R')
source('./contribution.R')
source('./utils.R')
source('./dispersion.R')
source('./data.R')

readRenviron(".Renviron")

date_from <- as.Date("2023-01-08")
date_to <- as.Date("2023-01-30") #lubridate::today() - lubridate::days(3)
dates <- seq.Date(date_from, date_to, by="day")
duration_hours <- 120

plants <- data.get_plants()

# get_dispersions for each plant, clean memory after each plant
# using get_dispersions function
for(i in 1:nrow(plants)){

    location_id <- plants$plants[[i]]
    geometry <- plants$geometry[i]

    stack_height <- plants$stack_height[[i]]
    release_height_low <- plants$release_height_low[[i]]
    height <- round(release_height_low * 2 - stack_height)

    # pbapply::pblapply(dates, function(date){
      dispersion.get(dates=dates,
      location_id=location_id,
        geometry=geometry,
        met_type="gdas1",
        heights=height,
        duration_hour=duration_hours,
        direction="forward",
        timezone="UTC",
        res_deg=0.05,
        convert_to_raster = F,
        cache_folder='cache',
        parallel=T,
        mc.cores=max(1, parallel::detectCores() - 1))[[1]]
    # })

    gc()
}
