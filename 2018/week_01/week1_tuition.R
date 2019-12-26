
library(tidyverse)
library(readxl)
library(ggrepel)

avg_tuition <- read_excel("2018/week_01/us_avg_tuition.xlsx")

avg_tuition_tidy <- avg_tuition %>% 
    pivot_longer(cols = 2:13, names_to = "period", values_to = "tuition")

top_tuit <- avg_tuition_tidy %>% 
    filter(period == c("2004-05", "2015-16")) %>% 
    group_by(State) %>%
    mutate(tuition_lag = lag(tuition),
           growth = (tuition / tuition_lag - 1) * 100) %>% 
    ungroup() %>% 
    top_n(7, growth) %>% 
    select(State)

top_state <- avg_tuition_tidy %>% 
    filter(State %in% top_tuit$State)


x11()

ggplot() +
    geom_line(data = avg_tuition_tidy,
              aes(period, tuition, group = State),
              colour = "#B1D4E0",
              size = 0.5) +
    geom_line(data = top_state,
              aes(period, tuition, group = State),
              colour = "#000C66",
              size = 1) +
    geom_text_repel(data = filter(top_state, period == "2015-16"),
                    aes(x = period, tuition, label = State),
                    nudge_x = 0.5,
                    size = 3) +
    labs(title = "Average US State Tuition Costs 2004-2015",
         subtitle = "States With The Highest Growth",
         caption = "Data Source: Online MBA Page") +
    scale_x_discrete(expand = expand_scale(add = c(0, 1.5))) +
    scale_y_continuous(limits = c(2500, 16000), breaks = c(3000, 6000, 9000, 12000, 15000), labels = scales::dollar) +
    theme_minimal(base_family = "Roboto Condensed") +
    theme(panel.grid.major.x = element_line(linetype = 8),
          panel.grid.major.y = element_line(linetype = 8),
          panel.grid.minor.y = element_blank(),
          legend.position = "none", axis.title = element_blank(),
          plot.title = element_text(face = "bold", size = 25, colour = "#151B25"),
          plot.subtitle = element_text(size = 15, margin = margin(t = 5, b = 10, unit = "pt")),
          plot.caption = element_text(margin = margin(t = 10, unit = "pt")),
          plot.margin = margin(10, 10, 10, 10)) 

ggsave("2018/week_01/avg_tuition2.png", width = 20, height = 35, units = "cm", dpi = 400)



