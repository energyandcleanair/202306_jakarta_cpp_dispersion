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
get_contributions <- function(dispersions, receptors, height_m=100, tz="Asia/Jakarta", density_res=1000){

  crs_utm = 32748

  # Assert there is only one plant
  if(length(unique(dispersions$location_id)) > 1){
    # Run and concatenate for each plant
    contributions <- pbapply::pblapply(
      unique(dispersions$location_id),
      function(location_id){
        get_contributions(dispersions %>%
                          filter(location_id == !!location_id),
                          receptors,
                          height_m=height_m,
                          tz=tz)
    }) %>%
      bind_rows()

    return(contributions)
  }

  # Get plant_id
  plant_id <- unique(dispersions$location_id)
  print(plant_id)

  # Rename for debugging convenience
  plant_dispersion <- dispersions

  # We use local day as a grouping date
  date_group <- function(date, tz=tz, freq="day"){
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
    function(d_day){
      MASS::kde2d(d_day$x, d_day$y, n=c(density_res, density_res),
                  lims=c(min(d_day$x), max(d_day$x),
                         min(d_day$y), max(d_day$y)) *
                    c(0.9, 1.1, 0.9, 1.1))
      })

  rasters <- lapply(names(densities), function(date_reception){
    k <- densities[[date_reception]]
    r <- raster::raster(k, crs=crs_utm)
    # r is in density per m2 as shown below
    sum((r * raster::xres(r) * raster::yres(r))[])

    # Correct for particles above height_m that we ignored
    ratio <- dispersions %>%
      filter(date_group(date_reception) == !!date_reception) %>%
      summarise(
        ratio = sum(height <= !!height_m) / n()
      ) %>%
      pull(ratio)

    r <- r * ratio

    # Bring to volumetric density
    r <- r / height_m

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
    receptor_density$unit <- "day.m-3"
    receptor_density <- receptor_density %>% rename(receptor_id=id)
    return(receptor_density)
  }) %>%
    bind_rows() %>%
    mutate(plant_id = !!plant_id)

  return(receptor_densities)
}
