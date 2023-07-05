

# List cached files by plant
files <- list.files('cache', '*.RDS')
files %>%
  str_split('\\.') %>%
  # bind into a dataframe
  map_df(~as.data.frame(t(.))) %>%
  # rename columns
  set_names(c('type', 'plant', 'net', 'height', 'duration_hours', 'date', 'extension')) %>%
  group_by(plant, month=floor_date(as.Date(date, format='%Y%m%d'), 'month')) %>%
  count()


  unlist() -> file_info
location_id=file_info[2]


library(tidyverse)
source('./data.R')
plants <- data.get_plants()

# list files in cache that are under 200kb
list.files('cache', pattern='dispersion*', full.names = T) %>%
  file.info() %>%
  filter(size < 3e6) -> small_files


# For each small file extract the date
dates <- small_files %>%
  rownames_to_column() %>%
  mutate(date = str_extract(rowname, '[0-9]{8}')) %>%
  select(date) %>%
  distinct() %>%
  pull()

date_to_filename <- function(date){
  # generate gdas1.jul13.w2
  date <- as.Date(date, format='%Y%m%d')
  monthyear <- tolower(format(date, '%b%y'))
  day <- as.numeric(format(date, '%d'))
  # integer division
  week <- floor(day / 7) + 1
  file.path(
    Sys.getenv('DIR_HYSPLIT_MET'),
    paste0('gdas1.', monthyear, '.w', week))
}

filenames <- dates %>% date_to_filename() %>% unique()
file.remove(filenames)
file.remove(rownames(small_files))



file <- rownames(small_files)[87]
# e.g. cache/suralaya/dispersion.suralaya.gdas1.310.120.20230520.RDS
# extract location, height and date from file name
file %>%
  str_extract('cache/dispersion\\.[a-z]+\\.[a-z0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]{8}\\.RDS') %>%
  str_split('\\.') %>%
  unlist() -> file_info

height=as.numeric(file_info[4])
date=as.Date(file_info[6], format='%Y%m%d')
location_id=file_info[2]
duration_hours=as.numeric(file_info[5])
plant = plants %>% filter(tolower(plants) == !!location_id)
geometry = plant$geometry

# Compute dispersion
library(creatrajs)
file.remove(file)
d <- dispersion.get(dates=date,
                                  location_id=location_id,
                                  geometry=geometry,
                                  met_type='gdas1',
                                  heights=height,
                                  duration_hour=duration_hours,
                                  direction="forward",
                                  timezone="UTC",
                                  convert_to_raster = F,
                                  cache_folder='cache',
                                  parallel=F)[[1]]

