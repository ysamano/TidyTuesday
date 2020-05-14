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
    geom_point(alpha = 0.8, colour = "#ff0055") + 
    facet_wrap(~ name_tot, ncol = 12) + 
    coord_fixed() +
    labs(title = "The most active volcanoes since 1800",
         subtitle = "Volcanoes with 10 or more eruptions",
         caption = "Souce: The Smithsonian Institution | Graphic: @ysamano28") +
    theme_void(base_family = "Roboto Condensed Light", base_size = 9) +
    theme(plot.background = element_rect(fill = "#1b1f2b", color = "#1b1f2b"),
          plot.title = element_text(family = "Roboto Condensed", colour = "white", face = "bold",
                                    size = 30, hjust = 0.5, margin = margin(t = 15, b = 5)),
          plot.subtitle = element_text(colour = "white", face = "bold", size = 15, hjust = 0.5,
                                       margin = margin(b = 15)),
          plot.caption = element_text(colour = "white", size = 10, hjust = 0.98,
                                      margin = margin(t = 5, b = 10)),
          strip.text = element_text(colour = "white", face = "bold"))


ggsave("2020/week_20/volcano3.png", p1, height = 12.5, width = 8.5, units = "in", dpi = 300)


