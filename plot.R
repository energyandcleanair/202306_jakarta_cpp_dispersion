plot_dispersions <- function(dispersions,
                             plants,
                             receptors,
                             dates=NULL,
                             location_id=NULL,
                             bbox_mode="indonesia",
                             buffer_km=5000,
                             folder='results',
                             particle_size=0.0005,
                             # Parameters to reduce the number of particles shown
                             particle_modulo_10=c(1),
                             every_n_hour=2){

  if(!is.null(location_id)){
    dispersions <- dispersions %>%
      filter(tolower(location_id)==tolower(!!location_id))
  }else{
    location_ids <- unique(dispersions$location_id)
    # Apply function to each location_id
    return(pbapply::pblapply(location_ids, function(location_id){
      plot_dispersions(dispersions=dispersions,
                       plants=plants,
                       receptors=receptors,
                       dates=dates,
                       location_id=location_id,
                       folder=folder,
                       bbox_mode=bbox_mode,
                       buffer_km=buffer_km)
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
      filter(as.numeric(particle_i) %% 10 %in% particle_modulo_10,
              hour <= 120,
             as.numeric(hour) %% every_n_hour == 0
             )

    if(nrow(data_lite) == 0){
      next
    }

    bbox <- data.get_bbox(mode=bbox_mode,
                          receptors=receptors,
                          plants=plants,
                          plant=plant,
                          buffer_km=buffer_km,
                          crs = 3857
    )

    left <- bbox$xmin
    right <- bbox$xmax
    bottom <- bbox$ymin
    top <- bbox$ymax

    mapbox_token <- "pk.eyJ1IjoiZGFubnloYXJ0b25vIiwiYSI6ImNrdnJpbXFzZTdxYzczMm1zbm1lMzhzd2oifQ.xutXbhbN3Zrl99lZGyf3Zg"

    set_defaults(map_service = "mapbox",
                 map_type = "dark",
                 map_token = mapbox_token)

    basemap_layer <- basemap_gglayer(bbox, map_res = 0.7, dpi=300, force=F, maxpixels=5e6)

    # # Try ggmaps
    # # Convert bbox back to lat/lon
    # bbox_4326 <- sf::st_as_sf(
    #   sf::st_bbox(bbox) %>%
    #     sf::st_as_sfc() %>%
    #     sf::st_transform(4326)
    # ) %>%
    # sf::st_bbox()
    #
    # left <- bbox_4326$xmin
    # right <- bbox_4326$xmax
    # bottom <- bbox_4326$ymin
    # top <- bbox_4326$ymax
    #
    # register_google(key = "[your key]")
    # map_data <- get_openstreetmap(bbox_4326,
    #                     zoom = 12, # Change the zoom level as needed
    #                     source = "osm", # This is for OpenStreetMap
    #                     # maptype = "terrain"
    #                     ) # You can change the type as required
    #
    # plt <- ggmap(map_data)


    plt <- ggplot() +
      basemap_layer +
      scale_fill_identity() +
      geom_sf(data=data_lite,
              size=particle_size,
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
    ggsave(file.path(folder, sprintf("%s_%s.jpg", tolower(location_id), format(as.Date(date), '%Y%m%d'))),
                     plot=plt, width=width, height=height, dpi = 300)
  }
}


plot_contributions <- function(contributions, folder='results', suffix=''){
  plot_contributions_average_bar(contributions=contributions, folder=folder, suffix=suffix)
  plot_contributions_ts(contributions=contributions, folder=folder, suffix=suffix)
}

plot_contributions_average_bar <- function(contributions, folder='results', suffix=''){

  dir.create(folder, F, T)

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

  ggsave(filename, width=12, height=24, plot=plt)
  return(plt)

}

plot_contributions_ts <- function(contributions, folder='results', suffix=''){

  dir.create(folder, F, T)

  filename <- file.path(folder, sprintf('contributions_ts%s.png', suffix))

  contributions %>%
    group_by(Region) %>%
    filter(receptor_id == min(receptor_id)) %>%
    select(receptor_id, Region, City, Kecamatan, Address, plant_id, date_reception, value=contribution_µg_m3) %>%
    ungroup() %>%
    tidyr::complete(nesting(receptor_id, Region, City, Kecamatan, Address),
                    plant_id,
                    date_reception=seq(min(date_reception), max(date_reception), by="hour"),
                    fill=list(value=0)
                    ) %>%
    mutate(plant_name=gsub(" power station", "", plant_id, ignore.case = T)) -> plot_data



  plot_data %>%
    # 24-hr rolling average)
    group_by(receptor_id, Region, City, Kecamatan, Address, plant_name,
             # date_reception=as.POSIXct(date(date_reception))
             date_reception
             ) %>%
    # summarise(value=mean(value)) %>%
    arrange(date_reception) %>%
    mutate(value_24=zoo::rollmean(value, 24, fill=NA, align='right', na.rm=T)) %>%
    ungroup() %>%
    mutate(plant_name=fct_reorder(plant_name, value, sum),
           receptor_id=fct_reorder(receptor_id, -value, mean)) -> plot_data24


  ggplot(plot_data24) +
    geom_area(aes(date_reception, value, fill=plant_name)) +
    facet_wrap(~receptor_id, scales = 'free_y', ncol = 1) +
    rcrea::theme_crea() +
    # start x axis on zero
    scale_x_datetime(date_breaks='1 week',
                     date_minor_breaks = '1 day',
                     date_labels = "%d %b") +
    scale_y_continuous(expand = expansion(mult=c(0, 0.1)),
                       limits = c(0, NA)) +
    rcrea::scale_fill_crea_d(name=NULL) +
    labs(y='µg/m³',
         x=NULL,
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
    theme(panel.grid.major.x = element_line(color='grey90'),
          panel.grid.minor.x = element_line(color='grey90')
          ) +
    # facet title as left text, no background or border, color #35416C, bold
    theme(strip.text = element_text(size=8, face='bold', color='#35416C', hjust=0),
          strip.background = element_blank(),
          strip.placement = 'outside') +
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


  ggsave(filename, width=12, height=10, plot=plt)
  return(plt)

}
