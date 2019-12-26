library(tidyverse)
library(countrycode)

life_exp <- read_csv("week_14/week14_global_life_expectancy.csv")

life_exp2 <- life_exp %>% 
    mutate(continent = countrycode(sourcevar = country, 
                                   origin = 'country.name', 
                                   destination = "continent")) %>% 
    filter(!is.na(continent))


life_plot <- 
    life_exp2 %>% 
    filter(year >= 1950) %>%
    ggplot( aes(x = code, y = year, fill = life_expectancy)) +
    geom_tile() +
    facet_wrap(~ continent, ncol = 2, scales = "free") +
    scale_x_discrete(expand = c(0, 0))+
    scale_y_continuous(breaks = seq(1950, 2015, 10), expand = c(0, 0))+
    scale_fill_distiller(palette = "Spectral", direction = 1)+
    labs(title = "Life Expectancy at Birth, 1950-2015",
         caption = "Source: ourworldindata.org | Graphic: @ysamano28") +
    guides(fill = guide_colourbar(title = "Years", title.position = "top")) +
    theme_minimal(base_family = "sans") +
    theme(axis.text.x = element_text(angle = 90, vjust = .5, size = 6),
          axis.text.y = element_text(size = 7),
          axis.title = element_blank(),
          axis.ticks = element_line(colour = "grey60"),
          plot.title = element_text(face = "bold", size = 22, margin = margin(b = 18)),
          plot.margin = margin(22, 10, 10, 10),
          strip.text.x = element_text(size = 12, hjust = 0, face = "bold"),
          legend.position = c(0.84, 1.065), 
          legend.direction = "horizontal",
          legend.key.width = unit(2.6, "line"),
          legend.key.height = unit(1, "line"))

ggsave("week_14/life_plot.png", life_plot, height = 11, width = 9, units = "in", dpi = 500)

