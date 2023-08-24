plot_dispersions <- function(dispersions,
                             plants,
                             receptors,
                             dates = NULL,
                             location_id = NULL,
                             bbox_mode = "indonesia",
                             max_height_m = NULL,
                             buffer_km = 5000,
                             folder = "results",
                             particle_size = 0.0005,
                             frequency = "hour",
                             # Parameters to reduce the number of particles shown
                             particle_modulo_10 = c(1),
                             every_n_hour = 2,
                             max_hour=120,
                             prefix = NULL,
                             keep_n_hour=NULL,
                             add_title=T,
                             force=F
                             ) {
  if (!is.null(location_id)) {
    dispersions <- dispersions %>%
      filter(tolower(location_id) %in% tolower(!!location_id))
    if (is.null(prefix)) {
      prefix <- paste0("_", tolower(location_id))
    }
  } else {
    location_ids <- unique(dispersions$location_id)

    # Apply function to each location_id independently
    pbapply::pblapply(location_ids, function(location_id){
      plot_dispersions(dispersions=dispersions,
                       plants=plants,
                       receptors=receptors,
                       dates=dates,
                       location_id=location_id,
                       folder=folder,
                       bbox_mode=bbox_mode,
    particle_modulo_10=c(1),
    every_n_hour=2,
    prefix=prefix,
                       buffer_km=buffer_km)
    })

    # plot_dispersions(
    #   dispersions = dispersions,
    #   plants = plants,
    #   receptors = receptors,
    #   dates = dates,
    #   location_id = location_ids,
    #   folder = folder,
    #   bbox_mode = bbox_mode,
    #   buffer_km = buffer_km,
    #   prefix = "all",
    #   particle_modulo_10 = particle_modulo_10,
    #   every_n_hour = every_n_hour,
    #   max_height_m=max_height_m,
    #   force=force
    # )

    return(NULL)
  }

  geometry <- plants %>%
    filter(tolower(plants) %in% tolower(location_id)) %>%
    pull(geometry)

  to_date <- function(date){
    floor_date(date, unit=frequency)
  }

  if (is.null(dates)) {
    dates <- unique(to_date(dispersions$date_reception))
  }

  pbapply::pblapply(dates, function(date){

    filepath <- file.path(folder, sprintf("%s_%s.jpg", coalesce(prefix, ""), format(date, time_suffix_formats[[frequency]])))
    if(file.exists(filepath) & !force){
      return(filepath)
    }

    # filter date
    if(!is.null(keep_n_hour)){
      # To be consistent with the rolling average in measurements
      # we keep n hours of reception
      data <- dispersions %>%
        filter(to_date(date_reception) >= !!date - lubridate::hours(keep_n_hour),
               to_date(date_reception) <= !!date)
    }else{
      data <- dispersions %>% filter(to_date(date_reception) == !!date)
    }

    data <- data %>%
    sf::st_as_sf(
      coords = c("lon", "lat"), crs = 4326
    ) %>%
      sf::st_transform(3857)

    # Process / filter data
    data <- data %>%
      filter(
        as.numeric(particle_i) %% 10 %in% particle_modulo_10,
        as.numeric(hour) %% every_n_hour == 0
      )


    # Filter hour
    data_lite <- data %>%
      filter(hour <= max_hour)

    nrow_before <- nrow(data_lite)

    # Filer by height
    if(!is.null(max_height_m)) {
      data_lite <- data_lite %>%
        filter(height <= max_height_m)
    }

    if (nrow(data_lite) == 0) {
      next
    }

    bbox <- data.get_bbox(
      mode = bbox_mode,
      receptors = receptors,
      plants = plants,
      plant = plant,
      buffer_km = buffer_km,
      crs = 3857
    )

    left <- bbox$xmin
    right <- bbox$xmax
    bottom <- bbox$ymin
    top <- bbox$ymax

    readRenviron(".Renviron")
    mapbox_token <- Sys.getenv("MAPBOX_TOKEN")

    set_defaults(
      map_service = "mapbox",
      map_type = "dark",
      map_token = mapbox_token
    )

    basemap_layer <- basemap_gglayer(bbox, map_res = 0.7, dpi = 300, force = F, maxpixels = 5e6)

    plt <- ggplot() +
      basemap_layer +
      scale_fill_identity()


    # if (use_contour) {
    #
    #   prefix <- paste0("contour_", prefix)
    #
    #   # Making some adjustments for contouring
    #   library(ggnewscale)
    #
    #   data_lite <- data_lite %>%
    #     mutate(
    #       x = st_coordinates(.)[, 1],
    #       y = st_coordinates(.)[, 2]
    #     )
    #
    #   # To have more accurate / granular contours,
    #   # We only keep particles close by
    #   # Extend bbox by 10%
    #   width <- right - left
    #   height <- top - bottom
    #   data_lite <- data_lite %>%
    #     filter(
    #       x >= left - width * 0.1,
    #       x <= right + width * 0.1,
    #       y >= bottom - height * 0.1,
    #       y <= top + height * 0.1
    #     )
    #   nrow_after <- nrow(data_lite)
    #
    #   # Create palette YlOrRd with first level transparent
    #   # We need same breaks for all frames, hence the hard-coding for now
    #   breaks <- c(seq(0, 100, 5), 1000) * 1E-11 * 3
    #   library(RColorBrewer)
    #   colors <- c(colorRampPalette(brewer.pal(9, "YlOrRd"))(length(breaks)-1),
    #               "#000000")
    #   colors[1] <- "#00000000"
    #
    #
    #   # Scaling breaks: geom_density_2d uses kde::
    #   # that expresses a density i.e. integral sums to 1
    #   # For colour scheme to be consistent across dates,
    #   # we need to account for particles that went above height_m
    #   # or out of the box.
    #   ratio <- nrow_after / nrow_before
    #
    #   breaks <- breaks * ratio
    #
    #   plt <- plt +
    #     new_scale_fill() + # we need one colour scale for the basemap and one for the contour
    #     geom_density_2d_filled(
    #       data = data_lite,
    #       aes(x, y),
    #       show.legend = F,
    #       breaks = breaks,
    #       alpha = 0.5
    #     ) +
    #     scale_fill_manual(values = colors)
    # } else {
    plt <- plt +
      geom_sf(
        data = data_lite,
        size = particle_size,
        alpha = 0.4,
        aes(col = hour),
        show.legend = F,
        alpha = 0.5
      ) +
      scale_fill_identity()

    plt <- plt +
      geom_sf(
        data = geometry,
        fill = "#8cc9D0",
        col = "white",
        size = 2,
        shape = 24
      ) +
      coord_sf(
        xlim = c(left, right),
        ylim = c(bottom, top), expand = F
      ) +
      theme_void() +
      scale_color_distiller(palette = "Reds") +
      ggdark::dark_theme_dark() +
      theme(
        panel.spacing = unit(c(0, 0, 0, 0), "cm"),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.border = element_blank(),
        # panel.spacing = unit(0, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(t=0, r=0, b=0, l=0, "pt")
      )


    time_suffix_formats <- list(
      hour="%Y%m%d%H",
      day="%Y%m%d"
    )

    if(add_title){
      plt <- plt +
      annotate("text",
               x = -Inf, y = Inf,
               label = "   Coal-fired power plants potential PM2.5 contribution",
               hjust = 0,
               vjust = 2,
               size = 6, colour = "white", family = "Source Sans Pro"
      ) +
        annotate("text",
                 x = -Inf, y = Inf,
                 label = to_date(date),
                 hjust = -0.12, vjust = 5.5,

                 size = 4, colour = "white", family = "Source Sans Pro"
        )
    }

    # Automatically adjust height
    width <- 10
    height <- width * (top - bottom) / (right - left)
    ggsave(filepath,
      plot = plt, width = width, height = height, dpi = 300
    )
    })
}


plot_contributions <- function(contributions, folder = "results", suffix = "") {
  plot_contributions_average_bar(contributions = contributions, folder = folder, suffix = suffix)
  plot_contributions_ts(contributions = contributions, folder = folder, suffix = suffix)
}

plot_contributions_average_bar <- function(contributions, folder = "results", suffix = "") {
  dir.create(folder, F, T)

  # Average bar chart
  filename <- file.path(folder, sprintf("contributions_bar_region%s.png", suffix))


  # Only keep region names with more than three cities and
  # rename Region to "others" otherwise
  contributions <- contributions %>%
    mutate(region_short = case_when(
      Region %in% (contributions %>%
        distinct(Region, City, Kecamatan) %>%
        group_by(Region) %>%
        summarise(n = n()) %>%
        filter(n >= 3) %>%
        pull(Region)) ~ Region,
      T ~ "Others"
    ))


  contributions %>%
    mutate(contribution_µg_m3 = replace_na(contribution_µg_m3, 0)) %>%
    group_by(receptor_id,
      location_name = paste(City, "-", Kecamatan), region_short,
      plant_name = gsub(" power station", "", plant_id, ignore.case = T)
    ) %>%
    summarise(contribution_µg_m3 = mean(contribution_µg_m3)) -> plot_data

  ggplot(plot_data) +
    geom_bar(
      aes(contribution_µg_m3,
        fct_reorder(location_name, contribution_µg_m3, sum),
        fill = fct_reorder(plant_name, contribution_µg_m3, sum)
      ),
      stat = "identity"
    ) +
    facet_grid(fct_reorder(region_short, -contribution_µg_m3, mean) ~ .,
      scales = "free",
      space = "free"
    ) +
    rcrea::theme_crea() +
    # start x axis on zero
    scale_x_continuous(
      expand = expansion(mult = c(0, 0.1)),
      limits = c(0, NA),
      breaks = seq(1, 100)
    ) +
    rcrea::scale_fill_crea_d(name = NULL) +
    labs(
      x = "µg/m³",
      y = NULL,
      title = "Power plant contribution to air pollution in various locations",
      # subtitle='Ambient concentration attributed to individual power plants',
      fill = NULL,
      caption = "Source: CREA analysis."
    ) +
    # fill legend on two rows underneath in reverse order
    # Make it smaller
    guides(fill = guide_legend(nrow = 2, reverse = T, byrow = TRUE)) +
    theme(
      legend.position = "bottom",
      # remove facet boxes
      panel.grid = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = NA, color = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.border = element_rect(colour = "#8cc9D0"),
      strip.background = element_rect(colour = "#8cc9D0", linetype = "solid")
    ) +
    theme(panel.grid.major.x = element_line(color = "grey90")) +
    # reduce legend size
    theme(
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8),
      legend.key.size = unit(0.5, "cm"),
      legend.key.width = unit(0.5, "cm"),
      legend.key.height = unit(0.2, "cm"),
      legend.spacing.x = unit(0.1, "cm"),
      legend.spacing.y = unit(0.1, "cm"),
      legend.box.margin = margin(0, 0, 0, 0, "cm"),
      legend.margin = margin(0, 0, 0, 0, "cm")
    ) -> plt

  ggsave(filename, width = 12, height = 24, plot = plt)
  return(plt)
}

plot_contributions_ts <- function(contributions, folder = "results", suffix = "") {
  dir.create(folder, F, T)

  filename <- file.path(folder, sprintf("contributions_ts%s.png", suffix))

  contributions %>%
    group_by(Region) %>%
    filter(receptor_id == min(receptor_id)) %>%
    select(receptor_id, Region, City, Kecamatan, Address, plant_id, date_reception, value = contribution_µg_m3) %>%
    ungroup() %>%
    tidyr::complete(nesting(receptor_id, Region, City, Kecamatan, Address),
      plant_id,
      date_reception = seq(min(date_reception), max(date_reception), by = "hour"),
      fill = list(value = 0)
    ) %>%
    mutate(plant_name = gsub(" power station", "", plant_id, ignore.case = T)) -> plot_data



  plot_data %>%
    # 24-hr rolling average)
    group_by(
      receptor_id, Region, City, Kecamatan, Address, plant_name,
      # date_reception=as.POSIXct(date(date_reception))
      date_reception
    ) %>%
    # summarise(value=mean(value)) %>%
    arrange(date_reception) %>%
    mutate(value_24 = zoo::rollmean(value, 24, fill = NA, align = "right", na.rm = T)) %>%
    ungroup() %>%
    mutate(
      plant_name = fct_reorder(plant_name, value, sum),
      receptor_id = fct_reorder(receptor_id, -value, mean)
    ) -> plot_data24


  ggplot(plot_data24) +
    geom_area(aes(date_reception, value, fill = plant_name)) +
    facet_wrap(~receptor_id, scales = "free_y", ncol = 1) +
    rcrea::theme_crea() +
    # start x axis on zero
    scale_x_datetime(
      date_breaks = "1 week",
      date_minor_breaks = "1 day",
      date_labels = "%d %b"
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.1)),
      limits = c(0, NA)
    ) +
    rcrea::scale_fill_crea_d(name = NULL) +
    labs(
      y = "µg/m³",
      x = NULL,
      title = "Power plant contribution to air pollution in various locations",
      # subtitle='Ambient concentration attributed to individual power plants',
      fill = NULL,
      caption = "Source: CREA analysis."
    ) +
    # fill legend on two rows underneath in reverse order
    # Make it smaller
    guides(fill = guide_legend(nrow = 2, reverse = T, byrow = TRUE)) +
    theme(
      legend.position = "bottom",
      # remove facet boxes
      panel.grid = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = NA, color = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.border = element_rect(colour = "#8cc9D0"),
      strip.background = element_rect(colour = "#8cc9D0", linetype = "solid")
    ) +
    theme(
      panel.grid.major.x = element_line(color = "grey90"),
      panel.grid.minor.x = element_line(color = "grey90")
    ) +
    # facet title as left text, no background or border, color #35416C, bold
    theme(
      strip.text = element_text(size = 8, face = "bold", color = "#35416C", hjust = 0),
      strip.background = element_blank(),
      strip.placement = "outside"
    ) +
    # reduce legend size
    theme(
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8),
      legend.key.size = unit(0.5, "cm"),
      legend.key.width = unit(0.5, "cm"),
      legend.key.height = unit(0.2, "cm"),
      legend.spacing.x = unit(0.1, "cm"),
      legend.spacing.y = unit(0.1, "cm"),
      legend.box.margin = margin(0, 0, 0, 0, "cm"),
      legend.margin = margin(0, 0, 0, 0, "cm")
    ) -> plt


  ggsave(filename, width = 12, height = 10, plot = plt)
  return(plt)
}

plot_contribution_contours <- function(
    contours,
    plants,
    receptors,
    dates = NULL,
    location_id = NULL,
    bbox_mode = "plants_receptors",
    buffer_km = 50,
    folder = "results",
    frequency = "hour",
    prefix = NULL,
    add_title=T,
    force=F,

    # Scale parameter
    conc_max = 160,
    conc_min = 20,
    breaks_power = 0.5,

    alpha_power = 0.3,
    alpha_min = 0
) {

  geometry <- plants %>%
    pull(geometry)

  to_date <- function(date){
    floor_date(date, unit=frequency)
  }

  if (is.null(dates)) {
    dates <- unique(to_date(contours$date_reception))
  }

  time_suffix_formats <- list(
    hour="%Y%m%d%H",
    day="%Y%m%d"
  )

  pbapply::pblapply(dates, function(date){

    filepath <- file.path(folder, sprintf("contour_%s.jpg",  format(date, time_suffix_formats[[frequency]])))
    print(filepath)
    if(file.exists(filepath) & !force){
      return(filepath)
    }

    # filter date
    # if(!is.null(keep_n_hour)){
    #   # To be consistent with the rolling average in measurements
    #   # we keep n hours of reception
    #   data <- dispersions %>%
    #     filter(to_date(date_reception) >= !!date - lubridate::hours(keep_n_hour),
    #            to_date(date_reception) <= !!date)
    # }else{
    date_contours <- contours %>%
      filter(to_date(date_reception) == !!date) %>%
      sf::st_transform(3857)


    bbox <- data.get_bbox(
      mode = bbox_mode,
      receptors = receptors,
      plants = plants,
      plant = NULL,
      buffer_km = buffer_km,
      crs = 3857
    )

    left <- bbox$xmin
    right <- bbox$xmax
    bottom <- bbox$ymin
    top <- bbox$ymax

    readRenviron(".Renviron")
    mapbox_token <- Sys.getenv("MAPBOX_TOKEN")

    set_defaults(
      map_service = "mapbox",
      map_type = "dark",
      map_token = mapbox_token
    )

    basemap_layer <- basemap_gglayer(bbox, map_res = 0.7, dpi = 300, force = F, maxpixels = 5e6)

    # Setting an power scale in µg/m3
    breaks_ratios <- seq(0, 1, length.out =10) ^ breaks_power
    conc_breaks <- conc_min + (conc_max - conc_min) * breaks_ratios


    colors <- colorRampPalette(brewer.pal(9, "YlOrRd"))(length(conc_breaks)-2)
    colors <- c("#FFFFFF", colors, "#000000")

    alpha_values <- seq(alpha_min, 1, length.out = length(colors))
    alpha_values <- (alpha_values^alpha_power) * 0.7
    colors_alpha <- mapply(adjustcolor, colors, alpha = alpha_values)


    plt <- ggplot() +
      basemap_layer +
      scale_fill_identity() +
      new_scale_fill() + # we need one colour scale for the basemap and one for the contour
      geom_sf(data = date_contours %>%
                mutate(color=case_when(sum < conc_min ~ NA,
                                       sum > conc_max ~ conc_max,
                                       T ~ sum)),
              aes(fill = color),
              col="transparent",
              show.legend = F,
              # alpha = 0.5
              ) +
      geom_sf(
        data = geometry,
        fill = "#8cc9D0",
        col = "white",
        size = 2,
        shape = 24
      ) +
        scale_fill_gradientn(colors = colors_alpha,
                             values = scales::rescale(conc_breaks),
                             limits = c(conc_min, conc_max),
                             na.value = NA,
                             oob = scales::squish,
                             guide = "colourbar",
                             aesthetics = "fill"
        ) +
      # scale_fill_distiller(limits=c(conc_min, conc_max),
      #                      palette="YlOrRd",
      #                      direction=1,
      #                      na.value = NA) +
      coord_sf(
        xlim = c(left, right),
        ylim = c(bottom, top), expand = F
      ) +
      theme_void() +
      # scale_color_distiller(palette = "Reds") +
      ggdark::dark_theme_dark() +
      theme(
        panel.spacing = unit(c(0, 0, 0, 0), "cm"),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.border = element_blank(),
        # panel.spacing = unit(0, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(t=0, r=0, b=0, l=0, "pt")
      )

    if(add_title){
      plt <- plt +
        annotate("text",
                 x = -Inf, y = Inf,
                 label = "   Coal-fired power plants potential PM2.5 contribution",
                 hjust = 0,
                 vjust = 2,
                 size = 6, colour = "white", family = "Source Sans Pro"
        ) +
        annotate("text",
                 x = -Inf, y = Inf,
                 label = to_date(date),
                 hjust = -0.21, vjust = 5.5,

                 size = 4, colour = "white", family = "Source Sans Pro"
        )
    }

    # Automatically adjust height
    width <- 10
    height <- width * (top - bottom) / (right - left)
    ggsave(filepath,
           plot = plt, width = width, height = height, dpi = 300
    )
  })
}


plot_concentrations <- function(folder,
                                meas = NULL,
                                date_from = "2023-08-01",
                                use_cache = F,
                                date_type = "hour",
                                running_hours = 24,
                                cache_folder = "cache",
                                force=F) {

  if (is.null(meas)) {
    file_cache <- file.path(cache_folder, sprintf("concentration_%s.rds", date_type))
    if (use_cache && file.exists(file_cache)) {
      meas <- readRDS(file_cache)
    } else {
      process_id <- ifelse(date_type == "day", "city_day_mad", "city_hour_mad")
      meas <- read_csv(glue("https://api.energyandcleanair.org/v1/measurements?city_name=Jakarta&process_id={process_id}&source=airnow&date_from={date_from}&format=csv&pollutant=pm25"))
      saveRDS(meas, file_cache)
    }
  }

  meas <- meas %>%
    filter(date >= as.Date(date_from) - lubridate::hours(running_hours))

  if (date_type == "hour") {
    meas <- meas %>%
      dplyr::select(pollutant, date, value) %>%
      tidyr::complete(date = seq(min(date), max(date), by = "hour"),
                      pollutant,
                      fill=list(value=NA)) %>%
      rcrea::utils.running_average(average_width = running_hours, average_by = date_type)
  }

  dates <- meas %>%
    filter(date >= date_from) %>%
    arrange(date) %>%
    pull(date) %>%
    unique()

  # Sort descending
  dates <- dates[order(dates, decreasing = T)]

  pbapply::pblapply(dates, function(date) {

    filename <- file.path(folder, sprintf("concentration_%s_%s.jpg", date_type, strftime(date, ifelse(date_type == "day", "%Y%m%d", "%Y%m%d%H"))))
    if(file.exists(filename) & !force){
      return(filename)
    }

    ggplot(meas, aes(date, value)) +
      geom_line(col=rcrea::pal_crea[["Orange"]]) +
      geom_point(data=function(x){
        x %>% filter(date == !!date)
      },col=rcrea::pal_crea[["Light.blue"]],
      size=2) +
      geom_vline(xintercept = date,
                 data=NULL,
                 linewidth=0.3,
                 col=rcrea::pal_crea[["Light.blue"]]) +
      labs(
        title="PM2.5 concentration in Jakarta",
        y = "µg/m3",
        subtitle = ifelse(date_type == "hour", sprintf("%s-hour running average", running_hours), NULL),
        x = NULL
      ) +
      ggdark::dark_theme_gray() +
      theme(
        # title = element_text(colour=rcrea::pal_crea[["Light.blue"]])
      ) +
      rcrea::scale_y_crea_zero() +
      scale_x_datetime(date_labels = "%d %b") +
      theme(plot.margin = margin(t = 10, r = 10, b = 40, l = 10, unit = "pt")) -> plt


    rcrea::quicksave(filename, plot=plt, logo_negative=T, logo_scale=2,
                     logo_vjust=-0.3, logo_hjust=0.67, scale=1,
                     width=12, height=4, preview=F, dpi=300)
  })
}
