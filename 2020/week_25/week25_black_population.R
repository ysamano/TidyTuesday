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
  scale_x_continuous(breaks = seq(1790, 1870, by = 10)) +
  scale_y_continuous(name = "Population",
                     labels = scales::comma_format(),
                     n.breaks = 10,
                     expand = c(0, 1),
                     position = "right") +
  scale_fill_manual(values = c("#008F77", "#072220"),
                    labels = c("Free", "Slaves")) +
  labs(title = "Black Population in the USA",
       subtitle = "1790 - 1870",
       caption = "Source: US Census's Archives | Graphic: @ysamano28") +
  theme_ybn(title_size = 23,
            title_face = "bold",
            title_hjust = 0.5,
            caption_hjust = 0.5) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(hjust = 0),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.spacing.x = unit(0.5, 'cm'),
        legend.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.position = "top")

ggsave("2020/week_25/black_population.png", p1, height = 11, width = 8.5, units = "in", type = "cairo")
