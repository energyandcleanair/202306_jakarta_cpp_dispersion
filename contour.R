rasters_to_contours <- function(rasters, ncol=1000, nrow=1000, parallel=T){


  date_receptions <- unlist(unname(lapply(rasters, function(x) attr(x, "date_reception"))))
  bbox <- data.get_bbox("plants_receptors", plants = plants, receptors=receptors, crs = crs_utm)
  raster_grid <- raster::raster(xmn=bbox$xmin,
                                xmx=bbox$xmax,
                                ymn=bbox$ymin, ymx=bbox$ymax, crs=crs_utm,
                                ncol=ncol,
                                nrow=nrow)
  lapply_fun <- if(parallel){
    pbmcapply::pbmclapply
  }else{
    pbapply::pblapply
  }

  rasters_agg <- lapply_fun(
    split(unname(rasters), date_receptions),
    function(x){
      terra::rast(
        lapply(x, function(x){
          terra::rast(x) %>%
            terra::resample(terra::rast(raster_grid), method="near")
        })) %>%
        # sum rasters
        terra::app(., fun=sum, na.rm=T)
    })



  polygons <- lapply_fun(names(rasters_agg), function(date_reception){
    sf::st_as_sf(terra::as.polygons(rasters_agg[[date_reception]], trunc=T)) %>%
      mutate(date_reception=as.POSIXct(date_reception))
  }) %>%
    bind_rows()

  return(polygons)

}
