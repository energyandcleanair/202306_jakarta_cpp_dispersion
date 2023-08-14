get_cache_infos <- function(){

  file_names <- list.files('cache', pattern='dispersion*', full.names = F)

  # For each small file extract the date and the location_id
  #e.g. from "dispersion.suralaya.gdas1.310.120.20230616.RDS"
  tibble(file=file_names) %>%
    mutate(date = str_extract(file, '[0-9]{8}'),
           location_id = str_extract(file, "(?<=dispersion\\.)[^.]+"),  # Extracts the location_id
           met_type = str_extract(file, paste0("(?<=\\.", location_id, "\\.)", "[^.]+"))  # Extracts the model using location_id
          ) %>%
    select(date, location_id, met_type) %>%
    # convert to date
    mutate(date = as.Date(date, format='%Y%m%d'))
}

get_latest_continous_date <- function(plants, met_type='gdas1', date_from="2023-01-01"){

  # Get longest date for which we have data for all plants for consecutive days since date_from
  # e.g. if we have data for each plant in 2023-01-01, 2023-01-02, 2023-01-03, 2023-01-05, 2023-01-06
  # then return "2023-01-03"
  get_cache_infos() %>%
    # filter by date
    filter(date >= as.Date(date_from)) %>%
    filter(met_type == !!met_type) %>%
    # count number of unique plants for each date
    group_by(date) %>%
    summarise(n_plants = n_distinct(location_id)) %>%
    # filter by number of plants
    filter(n_plants == nrow(plants)) %>%
    # sort by date
    arrange(date) %>%
    # ensure they are consecutive
    mutate(date_diff = date - lag(date, default = as.Date(date_from))) %>%
    filter(date_diff > 1 | date==max(date)) %>%  # Find rows where the difference is more than 1 day (i.e., there's a gap)
    summarise(latest_date_no_gap = if(any(date_diff > 1)){lag(date, default = date(date_from))[1]}else{min(date)}) %>%
    pull(latest_date_no_gap)
}


remove_small_cache_files <- function(cache_folder='cache', min_bytes=200){
    # remove small cache files
    cache_files <- list.files(cache_folder, full.names = TRUE)
    for (cache_file in cache_files){
        if (file.size(cache_file) < min_bytes){
          file.remove(cache_file)
        }
    }
}

remove_incomplete_cache_and_met_files <- function(){

  # Count rows in files
  file_rows <- pbapply::pblapply(
    list.files('cache', pattern='dispersion*', full.names = T),
    function(file){
      tibble(file=file,
             n=nrow(readRDS(file)))
    })

  file_rows <- file_rows %>% bind_rows()

  incomplete_files <- file_rows %>%
    filter(n < 0.9 * max(n))

  # For each small file extract the date
  dates <- incomplete_files %>%
    mutate(date = str_extract(file, '[0-9]{8}')) %>%
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
  file.remove(incomplete_files$file)

}
