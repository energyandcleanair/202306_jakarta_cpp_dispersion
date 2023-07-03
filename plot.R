plot_dispersion <- function(data, date, geometry, location_id, size_km=1500, plot_folder='.'){

  data_sf <- sf::st_as_sf(data, coords=c('lon', 'lat'), crs=4326) %>%
    sf::st_transform(3857)

  data_sf_lite <- data_sf %>%
    filter(as.numeric(particle_i) %% 10 %in% c(1),
           hour <= 120,
           as.numeric(hour) %% 2 == 0)

  coords <- st_coordinates(sf::st_transform(geometry, 3857))

  # define size of square in meters
  size <- size_km * 1000

  # define corners of square
  left <- coords[1] - size/2
  right <- coords[1] + size/2
  bottom <- coords[2] - size/2
  top <- coords[2] + size/2

  # create square polygon
  square <- st_polygon(list(rbind(c(left, top),
                                  c(right, top),
                                  c(right, bottom),
                                  c(left, bottom),
                                  c(left, top))))

  # create sf object
  ext <- st_sfc(square, crs = 3857)

  mapbox_token <- "pk.eyJ1IjoiZGFubnloYXJ0b25vIiwiYSI6ImNrdnJpbXFzZTdxYzczMm1zbm1lMzhzd2oifQ.xutXbhbN3Zrl99lZGyf3Zg"

  set_defaults(map_service = "mapbox",
               map_type = "dark",
               map_token=mapbox_token)

  basemap_layer <- basemap_gglayer(ext, map_res = 0.75, force=F)

  library(ggplot2)
  plt <- ggplot() +
    basemap_layer +
    scale_fill_identity() +
    geom_sf(data=data_sf_lite,
            size=0.001,
            alpha=0.4,
            aes(col=hour),
            show.legend=F
    ) +
    geom_sf(data=geometry,
            fill='#8cc9D0',
            col='white',
            size=4,
            shape=24) +
    # geom_sf(data=ext,
    #         col='blue',
    #         fill='transparent')+
    coord_sf(xlim = c(left, right),
             ylim = c(bottom, top), expand=F) +
    theme_void() +
    scale_color_distiller(palette='Reds') +
    annotate("text", x = Inf, y = Inf, label = date,
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

  ggsave(file.path(plot_folder, sprintf("%s_%s.png", location_id, date)), plot=plt, width=8, height=8, dpi = 300)
}
