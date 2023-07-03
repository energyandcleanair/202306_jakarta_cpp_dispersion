library(tidyverse)
library(sf)
library(tictoc)
library(lubridate)
library(plotly)
library(basemaps)
library(creatrajs)


readRenviron(".Renviron")

source('./plot.R')
source('./contribution.R')
source('./utils.R')
source('./dispersion.R')


date_from <- as.Date("2023-01-04")
date_to <- as.Date("2023-02-04") #lubridate::today() - lubridate::days(3)
dates <- seq.Date(date_from, date_to, by="day")
duration_hours <- 120
folder <- "results/jakarta"
dir.create(folder, F, T)


plants <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSjPfeBx-hT_iHIMxe4gBu5iydtY4LKIUAZvlsqMIrFCccVpXvTH_y2lTNwFnoekz4UF03SarOGs_Qq/pub?gid=254805418&single=true&output=csv')
plants_sf <- sf::st_as_sf(plants, coords=c('Longitude', 'Latitude'), crs=4326)

# Create a receptor_sf in Jakarta and Bali
receptor_sf <- tibble(
    location_id=c('jakarta', 'bali'),
    lon=c(106.8272, 115.1628),
    lat=c(-6.1751, -8.4095)
) %>%
    sf::st_as_sf(coords=c('lon', 'lat'), crs=4326)


d <- get_dispersions(plants_sf = plants_sf[seq(1),],
                dates = dates
                )


contribution <- get_contribution(
  dispersion=d,
  receptor_sf=receptor_sf,
  height_m=100
)

ggplot(contribution ) +
  geom_line(aes(date_reception, density, col=location_id))
