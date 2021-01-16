library(tidyverse)
library(geofacet)

avg_tuition <- readxl::read_excel("2018/week_01/us_avg_tuition.xlsx")

avg_tuition_tidy <- avg_tuition %>% 
    pivot_longer(cols = where(is.numeric), names_to = "period", values_to = "tuition") %>% 
    mutate(period_short = str_sub(period, 3, 8))

p1 <- ggplot(avg_tuition_tidy,
             aes(period_short,
                 tuition,
                 group = State)) +
    geom_area(fill = "#2a819a") +
    facet_geo( ~ State,
               grid = "us_state_grid2",
               label = "code") +
    scale_x_discrete("",
                     breaks = c("04-05","15-16"),
                     labels = c("'04","'16")) +
    scale_y_continuous("Tuition Cost",
                       labels = scales::label_number_si(prefix = "$")) +
    labs(title = "Average Tuition in the United States, 2004-2016", 
         caption = "Source: onlinembapage.com | Graphic: Yobanny Samano") +
    theme_ybn(base_size = 9,
              plot_margin = margin(20, 30, 20, 30),
              title_margin_b = 30,
              axis_text_size = 6,
              strip_size = 7.5,
              strip_face = "bold",
              axis_grid = FALSE)

ggsave("2018/week_01/avg_tuition.png", p1, width = 9, height = 7, units = "in", type = "cairo")
