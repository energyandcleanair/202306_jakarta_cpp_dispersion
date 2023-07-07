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
      dispersions %>% filter(as.Date(date_emission) == !!as.Date(date)),
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

    basemap_layer <- basemap_gglayer(ext, map_res = 0.4, dpi=1000, force=F)

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


plot_contributions <- function(contributions, folder='results', suffix=''){

  dir.create(folder, F, T)

  # Plot time series
  # filename <- file.path(folder, sprintf('contributions_ts%s.png', suffix))
  # ggplot(contributions %>%
  #          filter(grepl('Jakarta', receptor_id))) +
  #   geom_line(aes(date_reception, contribution, col=plant_id)) +
  #   facet_wrap(~receptor_id) +
  #   rcrea::theme_crea()
  #
  # ggsave(filename, width=12, height=8, dpi=300)

  # Average bar chart
  filename <- file.path(folder, sprintf('contributions_bar_region%s.png', suffix))


  # Only keep region names with more than three cities and
  # rename Region to "others" otherwise
  contributions <- contributions %>%
    mutate(region_short = case_when(
      Region %in% (contributions %>%
      distinct(Region, City, Kecamatan) %>%
      group_by(Region) %>%
      summarise(n=n()) %>%
      filter(n >= 3) %>%
      pull(Region)) ~ Region,
      T ~ "Others"))


  contributions %>%
    mutate(contribution_µg_m3=replace_na(contribution_µg_m3, 0)) %>%
    group_by(receptor_id, location_name=paste(City,"-",Kecamatan), region_short,
             plant_name=gsub(" power station", "", plant_id, ignore.case = T)) %>%
    summarise(contribution_µg_m3 = mean(contribution_µg_m3)) -> plot_data

  ggplot(plot_data) +
    geom_bar(aes(contribution_µg_m3,
                 fct_reorder(location_name, contribution_µg_m3, sum),
                 fill=fct_reorder(plant_name, contribution_µg_m3, sum)),
             stat='identity') +
    facet_grid(fct_reorder(region_short, -contribution_µg_m3, mean) ~ .,
               scales='free',
               space = "free") +
    rcrea::theme_crea() +
    # start x axis on zero
    scale_x_continuous(expand = expansion(mult=c(0, 0.1)),
                       limits = c(0, NA),
                       breaks=seq(1,100)) +
    rcrea::scale_fill_crea_d(name=NULL) +
    labs(x='µg/m³',
         y=NULL,
         title='Power plant contribution to air pollution in various locations',
         # subtitle='Ambient concentration attributed to individual power plants',
         fill=NULL,
         caption='Source: CREA analysis.') +
    # fill legend on two rows underneath in reverse order
    # Make it smaller
    guides(fill = guide_legend(nrow = 2, reverse=T, byrow=TRUE)) +
    theme(legend.position='bottom',
        # remove facet boxes
          panel.grid = element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.background = element_rect(fill = NA, color = NA),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_rect(colour='#8cc9D0'),
          strip.background = element_rect(colour='#8cc9D0', linetype='solid')) +
    theme(panel.grid.major.x = element_line(color='grey90')) +
        # reduce legend size
        theme(legend.text=element_text(size=8),
              legend.title=element_text(size=8),
              legend.key.size = unit(0.5, "cm"),
              legend.key.width = unit(0.5, "cm"),
              legend.key.height = unit(0.2, "cm"),
              legend.spacing.x = unit(0.1, "cm"),
              legend.spacing.y = unit(0.1, "cm"),
              legend.box.margin = margin(0, 0, 0, 0, "cm"),
              legend.margin = margin(0, 0, 0, 0, "cm")) -> plt


  print(plt)
  ggsave(filename, width=12, height=24, plot=plt)
  return(plt)

}
