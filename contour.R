rasters_to_contours <- function(){


  date_receptions <- unlist(unname(lapply(rasters, function(x) attr(x, "date_reception"))))
  bbox <- data.get_bbox("plants_receptors", plants = plants, receptors=receptors, crs = crs_utm)
  res <- 5000
  raster_grid <- raster::raster(xmn=bbox$xmin,
                                xmx=bbox$xmax,
                                ymn=bbox$ymin, ymx=bbox$ymax, crs=crs_utm,
                                ncol=res,
                                nrow=res)


  rasters_agg <- lapply(
    split(unname(rasters), date_receptions),
    function(x){
      terra::rast(
        lapply(x, function(x){
          terra::rast(x) %>%
            terra::resample(terra::rast(raster_grid), method="near")
        })) %>%
        # sum rasters
        terra::app(., fun=sum)
    })

  polygons <- pbmcapply::pbmclapply(names(rasters_agg), function(date_reception){
    sf::st_as_sf(terra::as.polygons(rasters_agg[[date_reception]], trunc=T)) %>%
      mutate(date_reception=as.POSIXct(date_reception))
  }) %>%
    bind_rows()

}
