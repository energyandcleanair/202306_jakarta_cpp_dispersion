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
date_to <- as.Date("2023-01-08")
dates <- seq.Date(date_from, date_to, by="day")
folder <- "results/jakarta"
dir.create(folder, F, T)
options("rgdal_show_exportToProj4_warnings"="none")

plants <- data.get_plants(as_sf=T)
receptors <- data.get_receptors(as_sf=T)

dispersions <- get_dispersions(
  plants = plants,
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
contributions_100 <- get_contributions(
  dispersions=dispersions,
  receptors=receptors,
  height_m=100,
  density_res=100
)

contributions_1000 <- get_contributions(
  dispersions=dispersions,
  receptors=receptors,
  height_m=100,
  density_res=1000
)

saveRDS(contributions_1000, 'cache/contributions.RDS')
contributions <- readRDS('cache/contributions.RDS')

# Plot results
plot_contributions(contributions_100, folder='results', suffix='_100')
plot_contributions(contributions_1000, folder='results', suffix='_1000')
