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
                              duration_hours=120){

  # Assert there is only one plant
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
                          tz=tz)
    }) %>%
      bind_rows()

    return(contributions)
  }

  crs_utm = 32748
  bbox_utm <- sf::st_bbox(receptors) %>%
    sf::st_as_sfc() %>%
    sf::st_transform(crs_utm) %>%
    sf::st_buffer(100e3) %>% # Add 100km
    sf::st_bbox()

  # Get plant_id
  plant_id <- unique(dispersions$location_id)
  print(plant_id)

  # Rename for debugging convenience
  plant_dispersion <- dispersions

  # We use local day as a grouping date
  date_group <- function(date, tz=tz, freq="hour"){
    # Convert hour in UTC to hour or day in local timezone
    date <- as.POSIXct(date, tz="UTC")
    date <- as.POSIXct(date, tz=tz)
    if(freq=="day"){
      date <- as.Date(date)
    }
    return(date)
  }

  # Only keep complete dates
  count <- plant_dispersion %>%
    group_by(date_group=date_group(date_reception)) %>%
    summarise(count=n())

  valid_dates <- count$date_group[count$count == max(count$count)]
  plant_dispersion <- plant_dispersion %>%
    filter(date_group(date_reception) %in% valid_dates)

  # Build density underneath height_m
  particles <- plant_dispersion %>%
    filter(height <= height_m) %>%
    st_as_sf(coords=c("lon", "lat"), crs=4326) %>%
    st_transform(crs_utm) %>%
    st_as_sf() %>%
    mutate(x = st_coordinates(.)[,1],
           y = st_coordinates(.)[,2])

  # Convert particles to densities
  densities <- pbapply::pblapply(
    split(particles, date_group(particles$date_reception)),
    function(particles_received_day){

      MASS::kde2d(particles_received_day$x,
                  particles_received_day$y,
                  n=c(density_res, density_res),
                  lims=bbox_utm[c(1,3,2,4)])
      })


  rasters <- lapply(names(densities), function(date_reception){

    k <- densities[[date_reception]]

    r <- raster::raster(k, crs=crs_utm)
  # r is in density per m2 as shown below
    sum((r * raster::xres(r) * raster::yres(r))[])

    # Bring to volumetric density
    r <- r / height_m

    # Correct for particles above height_m that we ignored
    ratio_height <- dispersions %>%
      filter(date_group(date_reception) == !!date_reception) %>%
      summarise(
        ratio = sum(height <= !!height_m) / n()
      ) %>%
      pull(ratio)

    r <- r * ratio_height


    # # Ratio emission
    # # How many emitted particles are considered in that reception date
    # # vs how many is emitted in a day or hour
    plant_dispersion %>%
      filter(date_group(date_reception)==!!date_reception) %>%
      group_by(date_group(date_emission)) %>%
      summarise(n_distinct=n_distinct(particle_i), n=n())

    # Correct for number of simulation days or hours
    # simulation_days <- duration_hours / 24
    # r <- r / simulation_days
    # r <- r / duration_hours

    attr(r, "date_reception") <- date_reception
    return(r)
    })


  receptors_utm <- receptors %>%
    sf::st_transform(crs_utm)

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
        select(plant_id=plants, emissions_t)) %>%
    mutate(contribution_µg_m3 = contribution * emissions_t * µg_per_tonne / hours_per_year * duration_hours) %>%
    group_by(receptor_id, plant_id, date_recepetion=date(date_reception)) %>%
    summarise(contribution_µg_m3 = mean(contribution_µg_m3))

  # Add receptor infos
  contributions <- contributions %>%
    left_join(as.data.frame(receptors) %>%
                rename(receptor_id=id))

  return(contributions)
}
