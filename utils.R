remove_small_cache_files <- function(cache_folder='cache', min_bytes=200){
    # remove small cache files
    cache_files <- list.files(cache_folder, full.names = TRUE)
    for (cache_file in cache_files){
        if (file.size(cache_file) < min_bytes){
          file.remove(cache_file)
        }
    }
}
