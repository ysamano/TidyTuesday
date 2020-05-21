
library(tidyverse)
library(readxl)
library(ggbump)
extrafont::loadfonts(device = "win")

global_mort <- read_excel("2018/week_03/global_mortality.xlsx")

global_mort_tidy <- global_mort %>% 
    pivot_longer(cols = 4:35, names_to = "cause", values_to = "percent") %>% 
    filter(country == "World") %>% 
    group_by(year) %>% 
    mutate(rank = min_rank(desc(percent))) %>% 
    ungroup() %>% 
    mutate(cause = str_remove(cause, "[[:space:]]\\(%\\)"),
           color_f = case_when(cause == "HIV/AIDS" ~ "HIV/AIDS",
                               cause == "Conflict" ~ "Conflict",
                               cause == "Natural disasters" ~ "Natural disasters",
                               cause == "Diabetes" ~ "Diabetes",
                               cause == "Dementia" ~ "Dementia",
                               TRUE ~ "other"))


group_1 <- global_mort_tidy %>% 
    filter(color_f == "other")

group_2 <- global_mort_tidy %>% 
    filter(color_f != "other")


p1 <- ggplot(group_1, aes(year, rank, group = cause)) +
    geom_bump(size = 1.1, alpha = 0.7, color = "grey") +
    geom_point(size = 2, color = "grey") +
    geom_text(data = group_1 %>% filter(year == 1990),
              aes(x = 1989, y = rank, label = rank), size = 4, hjust = 1, family = "Roboto Condensed") +
    geom_text(data = group_1 %>% filter(year == 2016),
              aes(x = 2016.5, y = rank, label = cause), size = 4, hjust = 0, family = "Roboto Condensed") +
    
    
    geom_bump(data = group_2, aes(year, rank, group = cause, colour = color_f), size = 1.3) +
    geom_point(data = group_2, aes(year, rank, group = cause, colour = color_f), size = 3) +
    geom_text(data = group_2 %>% filter(year == 1990),
              aes(x = 1989, y = rank, label = rank), size = 5.5, hjust = 1,
              family = "Roboto Condensed", fontface = "bold") +
    geom_text(data = group_2 %>% filter(year == 2016),
              aes(x = 2016.5, y = rank, label = cause), size = 5.5, hjust = 0,
              family = "Roboto Condensed", fontface = "bold") +
    
    scale_color_manual(values = c("#F70020","#191A1A","#FB9701","#1A7D00", "#0b53c1")) +
    
    
    scale_y_reverse(breaks = 1:32, expand = c(0.01, 0.01)) +
    scale_x_continuous(breaks = seq(1990, 2016, by = 2), limits = c(1989, 2026)) +
    labs(title = "Causas de muerte en el mundo",
         subtitle = "Principales variaciones",
         caption = "Fuente: Our World in Data") +
    theme_minimal(base_size = 9) +
    theme(panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none",
          plot.title = element_text(size = 22, face = "bold", family = "Roboto Condensed"),
          plot.subtitle = element_text(size = 14))

ggsave("2018/week_03/global_mort.png", p1, height = 12, width = 8, units = "in", dpi = 300)

