get_contribution <- function(dispersion, receptor_sf, height_m){

  # Compute density contour
  d_filtered = dispersion %>% filter(height <= height_m)

  density_hours <- pbapply::pblapply(split(d_filtered, d_filtered$date_reception),
         function(d_hour){
           MASS::kde2d(d_hour$lon, d_hour$lat, n=c(100, 100))
         })

  raster_hours <- lapply(names(density_hours), function(date_reception){
    r <- raster::raster(density_hours[[date_reception]], crs=4326)
    attr(r, "date_reception") <- date_reception
    return(r)
    })

  receptor_density_hours <- lapply(raster_hours, function(r){
    receptor_density_hour = tibble(receptor_sf) %>% select(-c(geometry))
    receptor_density_hour$density <- raster::extract(r, receptor_sf)
    receptor_density_hour$date_reception <- as.POSIXct(attr(r, "date_reception"))
    return(receptor_density_hour)
  }) %>%
    bind_rows()

  ggplot(receptor_density_hours) +
    geom_line(aes(date_reception, density, col=location_id))

  return(receptor_density_hours)
}
