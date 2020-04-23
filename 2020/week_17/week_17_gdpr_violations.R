
library(tidyverse)

gdpr_violations <- read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')

set.seed(250)

axis_ <- tibble(axis_x = runif(250),
                 axis_y = runif(250))

data_graph <- gdpr_violations %>% 
    select(name, price, controller) %>% 
    bind_cols(axis_) %>% 
    mutate(label = str_c(controller, "\n",
                         scales::dollar(price, suffix = "€", prefix = ""), "\n",
                         name))

x11()
ggplot() +
    geom_point(data = data_graph, aes(axis_x, axis_y, size = price), colour = "#39b185", alpha = 0.7) +
    scale_size(range = c(2, 80)) +
    geom_point(data = data_graph, aes(axis_x, axis_y), colour = "#353e4a", size = 1.5) +
    geom_text(data = data_graph %>% filter(price > 1000000),
              aes(axis_x, axis_y, label = label),
              size = 4, fontface = "bold",
              family = "Roboto Condensed", color = "grey90") +
    xlim(-0.02, 1) +
    labs(title = "The Universe of GDPR Violations",
         subtitle = "Top Companies",
         caption = "Source: Privacy Affairs | Graphic: @ysamano28 ") +
    theme_void(base_family = "Roboto Condensed") +
    theme(legend.position = "none",
          plot.background = element_rect(fill = "#252a32", color = "#252a32"),
          plot.title = element_text(face = "bold", size = 30,
                                    color = "white", hjust = 0.5,
                                    margin = margin(t = 15, b = 10)),
          plot.subtitle = element_text(size = 18, color = "white",
                                       hjust = 0.5,
                                       margin = margin(t = 0, b = 0)),
          plot.caption = element_text(size = 10, color = "white",
                                      margin = margin(b = 10)))

ggsave("2020/week_17/gdpr_violations.png", height = 12, width = 8, units = "in", dpi = 300)

