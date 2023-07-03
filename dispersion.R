get_dispersions <- function(plants_sf, dates, cache_folder='cache', plot=F, plot_folder='results'){

  pbapply::pblapply(seq(nrow(plants_sf)), function(i){

    location_id <- plants_sf$plants[[i]]
    print(location_id)
    geometry <- plants_sf$geometry[i]

    stack_height <- plants_sf$stack_height[[i]]
    release_height_low <- plants_sf$release_height_low[[i]]
    height <- round(release_height_low * 2 - stack_height)

    pbapply::pblapply(dates, function(date){
      dispersion <- dispersion.get(dates=date,
                                   location_id=location_id,
                                   geometry=geometry,
                                   met_type="gdas1",
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

      dispersion <- dispersion %>%
        mutate(date_reception = as.Date(date) + lubridate::hours(hour - 1))

      emission_date <- dispersion %>%
        group_by(particle_i) %>%
        summarise(date_emission=min(as.Date(date) + lubridate::hours(hour - 1)))

      dispersion <- dispersion %>%
        left_join(emission_date)

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
