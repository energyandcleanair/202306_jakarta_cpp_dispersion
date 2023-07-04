plot_dispersions <- function(dispersions,
                             plants,
                             dates=NULL,
                             location_id=NULL,
                             size_km=5000,
                             folder='results'){

  if(!is.null(location_id)){
    dispersions <- dispersions %>%
      filter(tolower(location_id)==tolower(!!location_id))
  }else{
    location_ids <- unique(dispersions$location_id)
    # Apply function to each location_id
    return(pbapply::pblapply(location_ids, function(location_id){
      plot_dispersions(dispersions=dispersions,
                       plants=plants,
                       dates=dates,
                       location_id=location_id,
                       size_km=size_km,
                       folder=folder)
    }))
  }

  geometry <- plants %>%
    filter(tolower(plants)==tolower(location_id)) %>%
    pull(geometry)

  if(is.null(dates)){
    dates <- unique(as.Date(dispersions$date_emission))
  }


  for(date in dates){

    data <- sf::st_as_sf(
      dispersions %>% filter(as.Date(date_emission) == !!date),
      coords=c('lon', 'lat'), crs=4326) %>%
      sf::st_transform(3857)

    # Reducing number of particles
    data_lite <- data %>%
      filter(as.numeric(particle_i) %% 10 %in% c(1),
              hour <= 120,
             as.numeric(hour) %% 2 == 0
             )

    # Indonesia bbox in 3857
    lon_min <- 95.293026
    lon_max <- 141.021805
    lat_min <- -10.359987
    lat_max <- 5.479820
    ext <- sf::st_polygon(list(rbind(c(lon_min, lat_min),
                                  c(lon_max, lat_min),
                                  c(lon_max, lat_max),
                                  c(lon_min, lat_max),
                                  c(lon_min, lat_min)))) %>%
      sf::st_sfc(crs=4326) %>%
      sf::st_transform(3857)

    left <- sf::st_coordinates(ext)[1,1]
    right <- sf::st_coordinates(ext)[2,1]
    bottom <- sf::st_coordinates(ext)[1,2]
    top <- sf::st_coordinates(ext)[3,2]

    # Centre on power plant
    # coords <- st_coordinates(sf::st_transform(geometry, 3857))
    # size <- size_km * 1000
    # left <- coords[1] - size/2
    # right <- coords[1] + size/2
    # bottom <- coords[2] - size/2
    # top <- coords[2] + size/2
    # square <- st_polygon(list(rbind(c(left, top),
    #                                 c(right, top),
    #                                 c(right, bottom),
    #                                 c(left, bottom),
    #                                 c(left, top))))
    # ext <- st_sfc(square, crs = 3857)

    mapbox_token <- "pk.eyJ1IjoiZGFubnloYXJ0b25vIiwiYSI6ImNrdnJpbXFzZTdxYzczMm1zbm1lMzhzd2oifQ.xutXbhbN3Zrl99lZGyf3Zg"

    set_defaults(map_service = "mapbox",
                 map_type = "dark",
                 map_token = mapbox_token)

    basemap_layer <- basemap_gglayer(ext, map_res = 10, force=F)

    plt <- ggplot() +
      basemap_layer +
      scale_fill_identity() +
      geom_sf(data=data_lite,
              size=0.0005,
              alpha=0.4,
              aes(col=hour),
              show.legend=F
      ) +
      geom_sf(data=geometry,
              fill='#8cc9D0',
              col='white',
              size=2,
              shape=24) +
      coord_sf(xlim = c(left, right),
               ylim = c(bottom, top), expand=F) +
      theme_void() +
      scale_color_distiller(palette='Reds') +
      annotate("text", x = Inf, y = Inf,
               label = as.Date(date),
               hjust = 1.1, vjust = 2,
               size = 4, colour = "#8cc9D0", family = "Source Sans Pro") +
      theme(panel.spacing = unit(c(0, 0, 0, 0), "cm"),
            panel.grid = element_blank(),
            axis.line=element_blank(),
            axis.text = element_blank(),
            axis.ticks=element_blank(),
            axis.title = element_blank(),
            panel.border=element_blank(),
            # panel.spacing = unit(0, "cm"),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.margin = margin(0, 0, 0, 0, "cm"))

    width <- 12
    # Automatically adjust height
    height <- width * (top-bottom)/(right-left)
    ggsave(file.path(folder, sprintf("%s_%s.png", tolower(location_id), format(as.Date(date), '%Y%m%d'))),
                     plot=plt, width=width, height=height, dpi = 300)
  }
}


plot_contributions <- function(contributions, folder='results'){

  dir.create(folder, F, T)


  ggplot(contributions %>%
           filter(grepl('Jakarta', receptor_id))) +
    geom_line(aes(date_reception, contribution, col=plant_id)) +
    facet_wrap(~receptor_id)

}
