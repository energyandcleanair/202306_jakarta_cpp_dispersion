library(tidyverse)
library(rcrea)
library(tidytext)
library(stringr)


data <- read_csv('data/emission_inventory.csv')

data %>%
  filter(distance_km==200) %>%
  filter(poll %in% c("PM", "NOx", "SO2")) %>%
  group_by(poll) %>%
  keep_top_n() %>%
  mutate(sector_long=str_wrap(sector_long, width = 20)) %>%
  ggplot() +
  geom_bar(stat='identity',
           aes(value_tonne/1e3,
               tidytext::reorder_within(sector_long, value_tonne, poll), fill=sector_long),
           show.legend = F,
           width = 0.6) +
  tidytext::scale_y_reordered() +
  facet_wrap(~poll, scales='free_y') +
  rcrea::scale_fill_crea_d() +
  labs(title='Air pollutant emissions per sector within 200km of Jakarta',
       subtitle='Year: 2019. Thousand tonne per year.',
       x=NULL,
       y=NULL,
       caption="Source: CREA analysis.") +
  rcrea::theme_crea() +
  theme(plot.margin = margin(r=15, l=15, t=15, b=20)) +
  scale_x_continuous(limits=c(0, NA),
                     expand = expansion(mult=c(0, 0.1))) -> plt

plt



rcrea::quicksave("results/inventory.png", plot = plt,
                 height = 5,
                 width=10,
                 scale=1,
                 logo_scale=1.5)


keep_top_n <- function(df, n=5, others="Others"){

  df %>%
    arrange(desc(value_tonne)) %>%
    mutate(is_others = row_number() > n) %>%
    mutate(sector=case_when(is_others ~ others, T~sector),
           sector_long=case_when(is_others ~ others, T~sector_long),
           ) %>%
    group_by(poll, sector, sector_long, distance_km) %>%
    summarise(value_tonne=sum(value_tonne))
}


