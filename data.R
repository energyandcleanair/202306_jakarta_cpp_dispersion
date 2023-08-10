data.get_plants <- function(as_sf=T){
  plants <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSjPfeBx-hT_iHIMxe4gBu5iydtY4LKIUAZvlsqMIrFCccVpXvTH_y2lTNwFnoekz4UF03SarOGs_Qq/pub?gid=254805418&single=true&output=csv')
  if(as_sf){
    plants <- sf::st_as_sf(plants, coords=c('Longitude', 'Latitude'), crs=4326)
  }
  return(plants)
}

data.get_receptors <- function(as_sf=T){

  receptors <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSoPNMTVaDBuaw1uwS0tXzZ2a7dRBP-pCgqRqYAMUy7geJoRC979fcLS3kp8r6OVL6euuhj4acN6_Ls/pub?gid=0&single=true&output=csv') %>%
    rename(
      lat = `Latitude`,
      lon = `Longitude`
    )

  if(as_sf){
    receptors <- receptors %>% sf::st_as_sf(coords=c('lon', 'lat'), crs=4326)
  }
  return(receptors)
}

data.get_bbox <- function(
  mode="indonesia",
  receptors=data.get_receptors(),
  plants=data.get_plants(),
  plant=NULL,
  buffer_km=100,
  crs=3857){

  if(mode=="indonesia"){
     lon_min <- 95.293026
     lon_max <- 141.021805
     lat_min <- -10.359987
     lat_max <- 5.479820
     sf::st_polygon(list(rbind(c(lon_min, lat_min),
                                   c(lon_max, lat_min),
                                   c(lon_max, lat_max),
                                   c(lon_min, lat_max),
                                   c(lon_min, lat_min)))) %>%
       sf::st_sfc(crs=4326) %>%
       sf::st_transform(crs) %>%
       sf::st_bbox()
  }else if(mode=="receptors"){
    receptors %>%
      sf::st_transform(crs=3857) %>%
      sf::st_buffer(buffer_km * 1000) %>%
      sf::st_transform(crs) %>%
      sf::st_bbox()
  }else if(mode=="plant_receptors"){
    bind_rows(
      plant,
      receptors) %>%
      sf::st_transform(crs=3857) %>%
      sf::st_buffer(buffer_km * 1000) %>%
      sf::st_transform(crs) %>%
      sf::st_bbox()
  }else if(mode=="plants_receptors"){
    bind_rows(
      plants,
      receptors) %>%
      sf::st_transform(crs=3857) %>%
      sf::st_buffer(buffer_km * 1000) %>%
      sf::st_transform(crs) %>%
      sf::st_bbox()
  }
}
