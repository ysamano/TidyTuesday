library(tidyverse)

eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')

volcano <- read_csv("2020/week_20/GVP_Volcano_List.csv", skip = 1) %>% 
    janitor::clean_names()

volcano_elevation <- volcano %>% 
    select(volcano_number, volcano_name, elevation) %>% 
    mutate(volcano_name = case_when(volcano_name == "Fournaise, Piton de la" ~ "Fournaise",
                                   TRUE ~ volcano_name))

total_eruption <- eruptions %>% 
    filter(start_year >= 1800,
           evidence_method_dating == "Historical Observations") %>% 
    select(volcano_number) %>% 
    left_join(volcano_elevation, by = c("volcano_number")) %>%
    group_by(volcano_name) %>% 
    mutate(n = n(),
           name_tot = str_c(volcano_name, "\n", "(", n, ")")) %>%
    filter(!is.na(elevation),
           elevation > 0,
           n >= 10) %>% 
    arrange(elevation)


g_ang <- pi * (3 - sqrt(5))

data_graph <- total_eruption %>% 
    mutate(t = row_number(volcano_name),
           x = sqrt(t)*cos(t * g_ang),
           y = sqrt(t)*sin(t * g_ang)) %>% 
    ungroup()


p1 <- ggplot(data_graph, aes(x = x, y = y)) + 
    geom_point(size = 0.9,
               alpha = 0.7, 
               colour = "#ff0055") + 
    facet_wrap(~ name_tot, ncol = 13) + 
    coord_fixed() +
    labs(title = "The most active volcanoes since 1800",
         subtitle = "Volcanoes with ten or more eruptions",
         caption = "Souce: The Smithsonian Institution | Graphic: @ysamano28") +
    theme_ybn(plot_margin = margin(20, 10, 20, 10),
              title_face = "bold",
              title_hjust = 0.5,
              caption_hjust = 0.5,
              strip_size = 6.5,
              strip_face = "bold",
              strip_hjust = 0.5,
              axis_grid = FALSE,
              axis_text = FALSE,
              axis_title = FALSE) +
    theme(strip.text = element_text(family = "Roboto Condensed Light"))

ggsave("2020/week_20/volcano.png", p1, height = 11.2, width = 8, units = "in", dpi = 300)
