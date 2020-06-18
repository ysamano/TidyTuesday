library(tidyverse)

census <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/census.csv')

data_graph <- census %>% 
  filter(region == "USA Total") %>% 
  pivot_longer(cols = c("black_free", "black_slaves"),
               names_to = "black_",
               values_to = "population")


p1 <- 
  ggplot(data_graph, aes(year, population, fill = black_)) +
  geom_col(width = 5) +
  scale_x_continuous(name = "Year",
                     breaks = seq(1790, 1870, by = 10)) +
  scale_y_continuous(name = "Population",
                     labels = scales::comma_format(),
                     n.breaks = 10,
                     expand = c(0, 1)) +
  scale_fill_manual(values = c("#008F77", "#072220"),
                    labels = c("Free", "Slaves")) +
  labs(title = "Black Population in the USA",
       subtitle = "1790 - 1870",
       caption = "Source: US Census's Archives | Graphic: @ysamano28") +
  theme_ybn_w(base_size = 11,
              title_family = "Merriweather Black",
              title_hjust = 0.5,
              subtitle_hjust = 0.5,
              subtitle_margin_b = 25,
              caption_margin_t = 25,
              plot_margin = margin(40, 60, 20, 60)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.spacing.x = unit(0.5, 'cm'),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "top")

ggsave("2020/week_25/black_population.png", p1, height = 11, width = 8.5, units = "in", dpi = 300)
