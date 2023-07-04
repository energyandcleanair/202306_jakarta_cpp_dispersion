library(tidyverse)
library(sf)
library(tictoc)
library(lubridate)
library(plotly)
library(basemaps)
library(creatrajs)
library(ggplot2)

readRenviron(".Renviron")

source('./plot.R')
source('./contribution.R')
source('./utils.R')
source('./dispersion.R')
source('./data.R')
source('./diagnostics.R')


date_from <- as.Date("2023-01-01")
date_to <- as.Date("2023-01-15")
dates <- seq.Date(date_from, date_to, by="day")
folder <- "results/jakarta"
dir.create(folder, F, T)
options("rgdal_show_exportToProj4_warnings"="none")

plants <- data.get_plants(as_sf=T)
receptors <- data.get_receptors(as_sf=T)

dispersions <- get_dispersions(
  plants = plants[seq(1),],
  dates = dates,
  cache_only=T)


# Plot dispersions
plot_dispersions(dispersions=dispersions,
                 plants=plants,
                 dates=sample(dates, 3),
                 folder='results')


# Diagnostics
diagnose_dispersions(dispersions)

# Cache
saveRDS(dispersions, 'cache/dispersions.RDS')
dispersions <- readRDS('cache/dispersions.RDS')

# Compute contributions
contributions <- get_contributions(
  dispersions=dispersions %>% filter(location_id=='Suralaya'),
  receptors=receptors,
  height_m=100
)

saveRDS(contributions, 'cache/contributions.RDS')
contributions <- readRDS('cache/contributions.RDS')

# Plot results
plot_contributions(contributions, folder='results')
