library(tidyverse)
library(stringr)

alcohol_world <- read_csv("week_13/week13_alcohol_global.csv")

alcohol_tidy <- alcohol_world %>% 
    gather(alcohol, value, -country) %>% 
    group_by(alcohol) %>% 
    top_n(100, value) %>% 
    mutate(axis_x = rep(1:10, length = n()),
           axis_y = rep(20:1, each = 10, length = n()),
           label = str_c(country, value, sep = "\n"))

# Theme of plot
theme_samano <- function() {
    theme_minimal(base_family = "Open Sans") +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              legend.position = "none",
              panel.grid = element_blank(),
              plot.title = element_text(face = "bold", size = 25, hjust = 0.5),
              plot.subtitle = element_text(size = 13, hjust = 0.5),
              plot.caption = element_text(size = 8),
              plot.margin = margin(20, 10, 10, 10))
}


beer_plot <- 
    alcohol_tidy %>% 
    filter(alcohol == "beer_servings") %>% 
    ggplot(aes(axis_x, axis_y)) +
    geom_point(aes(size = value)) +
    scale_size(range = c(0, 17)) +
    geom_text(aes(label = label), size = 2.5, nudge_y = -0.4) +
    labs(title = "Where Do People Drink The Most Beer?",
         subtitle = "The 100 countries with the most servings consumed per person, 2010",
         caption = "Source: FiveThirtyEight | Graphic: @ysamano28") +
    theme_minimal(base_family = "Open Sans") +
    theme_samano()

ggsave("week_13/beer_plot.png", beer_plot, height = 12, width = 8, units = "in", dpi = 300)



wine_plot <- 
    alcohol_tidy %>% 
    filter(alcohol == "wine_servings") %>% 
    ggplot(aes(axis_x, axis_y)) +
    geom_point(aes(size = value)) +
    scale_size(range = c(0, 17)) +
    geom_text(aes(label = label), size = 2.5, nudge_y = -0.4) +
    labs(title = "Where Do People Drink The Most Wine?",
         subtitle = "The 100 countries with the most servings consumed per person, 2010",
         caption = "Source: FiveThirtyEight | Graphic: @ysamano28") +
    theme_samano()

ggsave("week_13/wine_plot.png", wine_plot, height = 12, width = 8, units = "in", dpi = 300)



spirit_plot <- 
    alcohol_tidy %>% 
    filter(alcohol == "spirit_servings") %>% 
    ggplot(aes(axis_x, axis_y)) +
    geom_point(aes(size = value)) +
    scale_size(range = c(0, 17)) +
    geom_text(aes(label = label), size = 2.5, nudge_y = -0.4) +
    labs(title = "Where Do People Drink The Most Spirit?",
         subtitle = "The 100 countries with the most servings consumed per person, 2010",
         caption = "Source: FiveThirtyEight | Graphic: @ysamano28") +
    theme_samano()

ggsave("week_13/spirit_plot.png", spirit_plot, height = 12, width = 8, units = "in", dpi = 300)

