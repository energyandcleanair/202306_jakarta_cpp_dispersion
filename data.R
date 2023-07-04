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
