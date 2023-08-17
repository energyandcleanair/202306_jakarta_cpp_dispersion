#' Computes the contribution of each emittor to each receptor
#' based on dispersions
#'
#' @param dispersions
#' @param receptors
#' @param height_m
#' @param tz
#'
#' @return concentration in µg/m3 for an emission of 1 µg
#' @export
#'
#' @examples
get_contributions <- function(dispersions,
                              receptors,
                              plants,
                              height_m=10,
                              tz="Asia/Jakarta",
                              density_res=1000,
                              duration_hours=120,
                              return_rasters=F,
                              bbox_mode="receptors",
                              buffer_km=100,
                              crs_utm = 32748,
                              diagnostics_folder=NULL,
                              force_valid_dates=NULL,
                              parallel=T,
                              cores=parallel::detectCores()-2){

  if(length(unique(dispersions$location_id)) > 1){
    # Run and concatenate for each plant
    contributions <- pbapply::pblapply(
      unique(dispersions$location_id),
      function(location_id){
        get_contributions(dispersions %>%
                          filter(location_id == !!location_id),
                          receptors,
                          plants=plants,
                          height_m=height_m,
                          tz=tz,
                          bbox_mode=bbox_mode,
                          buffer_km=buffer_km,
                          crs_utm=crs_utm,
                          diagnostics_folder=diagnostics_folder,
                          force_valid_dates=force_valid_dates,
                          return_rasters=return_rasters,
                          parallel=parallel,
                          cores=cores)
        })

    if(!return_rasters){
      contributions <- bind_rows(contributions)
    }else{
      contributions <- unlist(contributions, recursive=F)
    }
    return(contributions)
  }

  # Select lapply based on parallel
  if(parallel){
    lapply_fun <- function(x, ...){parallel::mclapply(x, ..., mc.cores=cores)}
  }else{
    lapply_fun <- lapply
  }


  # Get plant_id
  plant_id <- unique(dispersions$location_id)
  print(plant_id)
  plant <- plants %>%
    filter(plants==!!plant_id)

  if(nrow(plant)==0){
    stop(glue("Missing data for {plant_id}"))
  }

  bbox_utm <- data.get_bbox(mode=bbox_mode,
                            receptors=receptors,
                            plants=plants,
                            plant=plant,
                            buffer_km=buffer_km,
                            crs = crs_utm
  )

  # Rename for debugging convenience
  plant_dispersion <- dispersions

  # We use local hour as a grouping date
  date_group <- function(date, tz=tz, freq="hour"){
    # Convert hour in UTC to hour or day in local timezone
    date <- as.POSIXct(date, tz="UTC")
    date <- as.POSIXct(date, tz=tz)
    if(freq=="day"){
      date <- as.Date(date)
    }
    return(date)
  }

  # Only keep dates with "sufficient" data
  count <- plant_dispersion %>%
    group_by(date_group=date(date_group(date_reception))) %>%
    dplyr::summarise(count=n())

  valid_dates <- count$date_group[count$count == max(count$count)]
  if(!is.null(force_valid_dates)){
    valid_dates <- force_valid_dates
  }

  # Remove last hour which is the first hour of the next date
  # valid_dates <- valid_dates[valid_dates != max(valid_dates)]
  # if(length(valid_dates) != 1){
  #   "Missing data"
  # }

  plant_dispersion <- plant_dispersion %>%
    filter(date(date_group(date_reception)) %in% valid_dates)

  # Build density underneath height_m
  # and within bbox
  particles <- plant_dispersion %>%
    filter(height <= height_m) %>%
    st_as_sf(coords=c("lon", "lat"), crs=4326) %>%
    st_transform(crs_utm) %>%
    # only keep particles within bbox
    st_intersection(st_as_sfc(bbox_utm)) %>%
    mutate(x = st_coordinates(.)[,1],
           y = st_coordinates(.)[,2])

  # ratio of particles considered vs particles emitted
  ratio <- nrow(particles) / nrow(plant_dispersion)

  # Convert particles to densities
  densities <- lapply_fun(
    split(particles, date_group(particles$date_reception)),
    function(particles_received_day){
      print(unique(particles_received_day$date_reception))

      tryCatch({
        MASS::kde2d(particles_received_day$x,
                    particles_received_day$y,
                    n=c(density_res, density_res),
                    lims=bbox_utm[c(1,3,2,4)])
      }, error=function(x){
        print(glue("Failed to build density"))
        return(NULL)
      })
    }
  )

  # Remove NULL from densities
  densities <- densities[!sapply(densities, is.null)]

  # Factors to scale to µg/m3
  hours_per_year = 365 * 24
  µg_per_tonne = 1e12
  emissions_t <- plant$emissions_t

  rasters <- lapply_fun(names(densities), function(date_reception){

    k <- densities[[date_reception]]

    r <- raster::raster(k, crs=crs_utm)

    # r is in density per m2 as shown below
    # sum((r * raster::xres(r) * raster::yres(r))[])

    # Bring to volumetric density
    r <- r / height_m

    # Correct for particles above height_m and outside bbox that we ignored
    r <- r * ratio

    # Convert to µg/m3
    r <- r * emissions_t * µg_per_tonne / hours_per_year * duration_hours

    attr(r, "date_reception") <- date_reception
    return(r)
    })


  receptors_utm <- receptors %>%
    sf::st_transform(crs_utm)

  plant_utm <- plants %>%
    filter(plants==!!plant_id) %>%
    sf::st_transform(crs=crs_utm)

  if(!is.null(diagnostics_folder)){
    dir.create(diagnostics_folder, showWarnings = FALSE)
    for(i in seq_along(rasters)){
      # plot raster in a png
      png(paste0(diagnostics_folder, "/",
                 plant_id, "_",
                 attr(rasters[[i]],"date_reception"), ".png"))
      raster::plot(rasters[[i]])
      raster::plot(receptors_utm, col='black', add=T)
      # Plot the plant with a filled triangle
      raster::plot(plant_utm, col='red', add=T, pch=17)
      dev.off()
    }
  }

  # Extract contributions at locations
  receptor_densities <- lapply_fun(rasters, function(r){
    receptor_density = tibble(receptors) %>% dplyr::select(-c(geometry))
    receptor_density$contribution_µg_m3 <- raster::extract(r, receptors_utm)
    receptor_density$date_reception <- as.POSIXct(attr(r, "date_reception"))
    receptor_density <- receptor_density %>% rename(receptor_id=id)
    return(receptor_density)
  }) %>%
    bind_rows() %>%
    mutate(plant_id = !!plant_id)

  # Aggregate and add infos
  contributions <- receptor_densities %>%
    group_by(receptor_id, plant_id, date_reception) %>%
    summarise(contribution_µg_m3 = sum(contribution_µg_m3)) %>%
    left_join(as.data.frame(receptors) %>%
                rename(receptor_id=id),
              by='receptor_id')

  if(return_rasters){
    return(list(rasters=rasters, contributions=contributions))
  }else{
    return(contributions)
  }
}
