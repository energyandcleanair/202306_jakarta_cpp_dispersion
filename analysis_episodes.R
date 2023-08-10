# Analysing the PM2.5 episodes in 2023

library(tidyverse)
library(sf)
library(tictoc)
library(lubridate)
library(plotly)
library(basemaps)
library(creatrajs)
library(glue)
library(ggplot2)
library(tidytext)


readRenviron(".Renviron")
readRenviron("~/development/crea/deweather/.Renviron")

source("./plot.R")
source("./contribution.R")
source("./utils.R")
source("./dispersion.R")
source("./data.R")
source("./diagnostics.R")

folder = "results/20230808_analysis_jakarta"
dir.create(folder, F, T)

# Air quality data --------------------------------------------------------
meas_stations <- read_csv("https://api.energyandcleanair.org/v1/measurements?format=csv&city_name=Jakarta&source=airnow&process_id=station_hour_mad,city_hour_mad&pollutant=pm25")
meas_stations %>%
  filter(date >= '2023-01-01') %>%
  filter(process_id == 'city_hour_mad') %>%
  rcrea::utils.running_average(24, average_by="hour", min_values = 20) %>%
  ggplot() +
  geom_line(aes(date, value, col=location_id), show.legend = F) +
  rcrea::theme_crea() +
  rcrea::scale_color_crea_d() +
  labs(title='PM2.5 levels in Jakarta',
       subtitle='24-hour running average',
       x=NULL,
       y='µg/m3',
       caption='Source: AirNow. Value represents the average of Jakarta South and Jakarta Central stations.') +
  rcrea::scale_y_crea_zero() +
  scale_x_datetime(date_breaks = '1 month',
                   date_labels = '%b %y')

ggsave(file.path(folder, "pm25_2023.png"), width=10, height=6)





meas_stations %>%
  filter(process_id == 'city_hour_mad') %>%
  filter(date >= '2020-01-01') %>%
  rcrea::utils.running_average(24, average_by="hour", min_values = 20) %>%
  mutate(date000 = `year<-`(date, 2000)) %>%
  ggplot() +
  geom_line(aes(date000, value, col=factor(year(date))), show.legend = F) +
  rcrea::theme_crea() +
  scale_color_brewer(palette='Reds') +
  labs(title='PM2.5 levels in Jakarta',
       subtitle='24-hour running average',
       x=NULL,
       y='µg/m3',
       caption='Source: AirNow. Value represents the average of Jakarta South and Jakarta Central stations.') +
  rcrea::scale_y_crea_zero() +
  scale_x_datetime(date_breaks = '1 month',
                   date_labels = '%b')

ggsave(file.path(folder, "pm25_2020_2023.png"), width=10, height=6)



# Deweather ---------------------------------------------------------------
# dew <- creadeweather::deweather(location_id='jakarta_idn.7_1_id',
#                                 source='airnow',
#                                 poll = 'pm25',
#                                 deweather_process_id = 'default_anomaly_2018_2099',
#                                 upload_results = T)





# Dates of interest ------------------------------------------------------------
dates_of_interest <- meas_stations %>%
  filter(date >= '2023-01-01') %>%
  filter(process_id == 'city_hour_mad') %>%
  group_by(date=date(date)) %>%
  summarise(value=mean(value),
            count=n()) %>%
  arrange(desc(value)) %>%
  filter(count > 12) %>%
  head(5) %>%
  write_csv(file.path(folder, "2023_episodes.csv")) %>%
  pull(date)

# dates_of_interest <- "2023-08-06"


plants <- data.get_plants(as_sf = T)
receptors <- data.get_receptors(as_sf = T) %>%
  filter(grepl("Jakarta", City))

# library(mapview)
# mapview(receptors)

# Compute contributions ---------------------------------------------------
duration_hours <- 120
duration_days <- ceiling(duration_hours / 24)

# Take all dates of interest and the duration_days before
dates_dispersion <- lapply(dates_of_interest, function(date_of_interest) {
  date_from <- date(date_of_interest) - days(duration_days)
  date_to <- date(date_of_interest)
  seq.Date(date_from, date_to, by = "day")}) %>%
  do.call(c, .) %>%
  unique()


options("rgdal_show_exportToProj4_warnings" = "none")
met_type <- 'gdas1'
bbox_mode <- c("plant_receptors")


dispersions <- get_dispersions(
  plants = plants,
  dates = dates_dispersion,
  cache_only = T,
  met_type = met_type
)

# # Plot dispersions
plot_dispersions(
  dispersions = dispersions,
  plants = plants,
  receptors=receptors,
  dates = dates_dispersion,
  folder = folder,
  bbox_mode = "plants_receptors",
  buffer_km=100,
  particle_modulo_10=c(1,3,5,7,9),
  every_n_hour=2,
  particle_size=0.0007
)


# Diagnostics
# diagnose_dispersions(dispersions)

# Cache
# saveRDS(dispersions, 'cache/dispersions.RDS')
# dispersions <- readRDS('cache/dispersions.RDS')

contributions <- get_contributions(
  dispersions = dispersions,
  plants = plants,
  receptors = receptors,
  height_m = 10,
  density_res = 1000,
  bbox_mode = bbox_mode,
  buffer_km=100,
  force_valid_dates = dates_of_interest,
  diagnostics_folder = "diagnostics"
)

# Move date_range by one day
date_range <- date_range + 1

saveRDS(contributions, glue(file.path(folder,"contributions.RDS")))
contributions <- readRDS(glue(file.path(folder,"contributions.RDS")))
# contributions_receptors <- readRDS('results/contributions_receptors.RDS')
# contributions_indonesia <- readRDS('results/contributions_indonesia.RDS')



# Plot contributions for each episode -------------------------------------

contributions %>%
  filter(date(date_reception) %in% dates_of_interest) %>%
  group_by(episode=factor(date(date_reception)), plant_id) %>%
  summarise(
    contribution_µg_m3=mean(contribution_µg_m3, na.rm=T),
    count=n()
  ) %>%
  ungroup() %>%
  mutate(plant_id=gsub(" power station","", plant_id, ignore.case = T)) %>%
  ggplot(aes(y=reorder_within(plant_id, contribution_µg_m3, episode),
             x=contribution_µg_m3)) +
  geom_bar(aes(fill=plant_id),
           stat='identity',
           show.legend = F) +
  geom_text(aes(label=round(contribution_µg_m3,1)),
            # hjust=1.1,
            hjust=-0.1,
            size=3,
            # color='white'
            ) +
  facet_wrap(~episode, scales='free_y') +
  rcrea::theme_crea() +
  scale_y_reordered() +
  scale_x_continuous(limits=c(0, NA), expand = expansion(c(0, 0.12))) +
  labs(title='Contribution of coal-fired power plants to Jakarta PM2.5 levels',
       # subtitle='Values for 6 August 2023',
       caption='Source: CREA analysis.',
       y=NULL,
       x="µg/m3") +
  rcrea::scale_fill_crea_d()

ggsave(file.path(folder, "contribution_2023_episodes.png"), width=12, height=8)

