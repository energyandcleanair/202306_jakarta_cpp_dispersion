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
