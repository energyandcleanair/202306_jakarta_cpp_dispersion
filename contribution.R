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
                              height_m=100,
                              tz="Asia/Jakarta",
                              density_res=1000,
                              duration_hours=120,
                              return_rasters=F,
                              bbox_mode="receptors",
                              crs_utm = 32748,
                              diagnostics_folder=NULL,
                              force_valid_dates=NULL){

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
                          crs_utm=crs_utm,
                          diagnostics_folder=diagnostics_folder)
    }) %>%
      bind_rows()

    return(contributions)
  }

  bbox_utm <- data.get_bbox(mode=bbox_mode,
                            receptors=receptors,
                            crs = crs_utm)

  # Get plant_id
  plant_id <- unique(dispersions$location_id)
  print(plant_id)

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
    summarise(count=n())

  valid_dates <- count$date_group[count$count == max(count$count)]
  if(!is.null(force_valid_dates)){
    valid_dates <- intersect(force_valid_dates, unique(date(plant_dispersion$date_reception)))
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
  densities <- lapply(
    split(particles, date_group(particles$date_reception)),
    function(particles_received_day){
      print(unique(particles_received_day$date_reception))

      MASS::kde2d(particles_received_day$x,
                  particles_received_day$y,
                  n=c(density_res, density_res),
                  lims=bbox_utm[c(1,3,2,4)])
      })

  # if(!is.null(diagnostics_folder)){
  #   dir.create(diagnostics_folder, showWarnings = FALSE, recursive = T)
  #   for(i in seq_along(densities)){
  #     # plot density in a png
  #     png(paste0(diagnostics_folder, "/",
  #                plant_id, "_density_",
  #                names(densities)[[i]], ".png"))
  #     image(densities[[i]])
  #     dev.off()
  #   }
  # }


  rasters <- lapply(names(densities), function(date_reception){

    k <- densities[[date_reception]]

    r <- raster::raster(k, crs=crs_utm)
    # r is in density per m2 as shown below
    sum((r * raster::xres(r) * raster::yres(r))[])

    # Bring to volumetric density
    r <- r / height_m

    # Correct for particles above height_m and outside bbox that we ignored
    r <- r * ratio

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



  # Extract densities at locations
  receptor_densities <- lapply(rasters, function(r){
    receptor_density = tibble(receptors) %>% dplyr::select(-c(geometry))
    receptor_density$contribution <- raster::extract(r, receptors_utm)
    receptor_density$date_reception <- as.POSIXct(attr(r, "date_reception"))
    receptor_density$unit <- "m-3"
    receptor_density <- receptor_density %>% rename(receptor_id=id)
    return(receptor_density)
  }) %>%
    bind_rows() %>%
    mutate(plant_id = !!plant_id)

  hours_per_year = 365 * 24
  µg_per_tonne = 1e12

  # Convert to µg
  contributions <- receptor_densities %>%
    left_join(
      as.data.frame(plants) %>%
        select(plant_id=plants, emissions_t),
      by='plant_id') %>%
    mutate(contribution_µg_m3 = contribution * emissions_t * µg_per_tonne / hours_per_year * duration_hours) %>%
    group_by(receptor_id, plant_id, date_reception) %>%
    summarise(contribution_µg_m3 = mean(contribution_µg_m3))

  # Add receptor infos
  contributions <- contributions %>%
    left_join(as.data.frame(receptors) %>%
                rename(receptor_id=id),
              by='receptor_id')

  if(return_rasters){
    return(list(rasters=rasters, contributions=contributions))
  }else{
    return(contributions)
  }
}
