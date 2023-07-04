diagnose_dispersions <- function(dispersions,
                                 diagnostics_folder='diagnostics'){

  dir.create(diagnostics_folder, F, T)

  # Plot number of particles per day
  count <- dispersions %>%
    group_by(location_id, date_reception) %>%
    summarise(count=n())

  ggplot(count) +
    geom_line(aes(date_reception, count, col=location_id))

  ggsave(file.path(diagnostics_folder, 'dispersions_count.png'))

}
