library(tidyverse)
library(rcrea)

data <- read_csv('data/emission_inventory.csv')

data %>%
  filter(distance_km==200) %>%
  filter(poll %in% c("PM", "NOx", "SO2")) %>%
  ggplot() +
  geom_bar(stat='identity',
           aes(value_tonne, sector_long, fill=sector_long),
           show_legend=F) +
  facet_wrap(~poll)
