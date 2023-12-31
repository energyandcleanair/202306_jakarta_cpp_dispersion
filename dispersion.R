get_dispersions <- function(plants, dates,
                            cache_folder='cache',
                            cache_only=F,
                            parallel=F,
                            plot=F,
                            plot_folder='results',
                            met_type='gdas1',
                            duration_hours=120){

  pbapply::pblapply(seq(nrow(plants)), function(i){

    location_id <- plants$plants[[i]]
    print(location_id)
    geometry <- plants$geometry[i]

    stack_height <- plants$stack_height[[i]]
    release_height_low <- plants$release_height_low[[i]]
    height <- round(release_height_low * 2 - stack_height)

    pbapply::pblapply(dates, function(date){
      if(cache_only){
        file_cache <- file.path(cache_folder,
                                   creatrajs::dispersion.cache_filename(location_id,
                                                             met_type,
                                                             height,
                                                             duration_hours,
                                                             date, "RDS"))
        if(!file.exists(file_cache)){
          return(NULL)
        }
      }

      dispersion <- dispersion.get(dates=date,
                                   location_id=location_id,
                                   geometry=geometry,
                                   met_type=met_type,
                                   heights=height,
                                   duration_hour=duration_hours,
                                   direction="forward",
                                   timezone="UTC",
                                   res_deg=0.05,
                                   convert_to_raster = F,
                                   cache_folder=cache_folder,
                                   parallel=F,
                                   mc.cores=max(parallel::detectCores()-1,1))[[1]]

      if(nrow(dispersion)==0){
        return(NULL)
      }

      # # dplyr version (slow)
      # dispersion <- dispersion %>%
      #   mutate(date_reception = as.Date(date) + lubridate::hours(hour)) %>%
      #   group_by(particle_i) %>%
      #   mutate(date_emission=min(as.Date(date) + lubridate::hours(hour - 1))) %>%
      #   ungroup() %>%
      #   mutate(age = date_reception - date_emission) %>%
      #   # Keep all particles for the exact same time so that
      #   # At any point in time the number of particles is constant
      #   filter(age <= duration_hours)

      # data.table version (fast)
      dispersion <- as.data.table(dispersion)
      dispersion[, date := as.POSIXct(date)]
      dispersion[, date_reception := date + (hour * 3600)]
      dispersion[, date_emission := min(date + ((hour - 1) * 3600)), by = particle_i]
      dispersion[, date := NULL]
      dispersion[, age := as.numeric(difftime(date_reception, date_emission, units = "hours"))]
      dispersion <- dispersion[age <= duration_hours]
      dispersion <- tibble(dispersion)

      if(plot){
        plot_dispersion(data=dispersion,
                        location_id=location_id,
                        geometry=geometry,
                        date=date,
                        plot_folder=plot_folder)
      }

      return(dispersion)
    }) %>%
      bind_rows() %>%
      mutate(location_id = location_id)
    }) %>%
  bind_rows()

}
