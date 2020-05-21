
library(readxl)
library(tidyverse)
library(ggbeeswarm)

nfl_salary <- 
    read_excel("2018/week_02/nfl_salary.xlsx")

salary <- nfl_salary %>% 
    gather(-year, key = "position", value = "valores") %>% 
    filter(!is.na(valores))


x11()
ggplot(salary, aes(year, valores / 1000000, group = year)) +
    geom_quasirandom(size = 0.8, alpha = .3, colour = "#54278f") +
    facet_wrap(~ position, ncol = 5) +
    scale_y_continuous(labels = scales::dollar_format(suffix = "m")) +
    labs(x = "Year", y = "Salary",
         title = "NFL Positional Salaries",
         caption = "Graphic: @ysamano28 | Source: spotrac.com") +
    theme_minimal(base_size = 8) + 
    theme(plot.title = element_text(face = "bold", size = 13, colour = "#151B25",
                                    margin = margin(10, 0, 10, 0)),
          strip.text.x = element_text(size = 8, face = "bold", margin = margin(5, 0, 5, 0)), 
          axis.title.x = element_text(hjust = 1, margin = margin(5, 0, 0, 0)),
          axis.title.y = element_text(hjust = 1, margin = margin(0, 5, 0, 0)))
          

ggsave("2018/week_02/nfl_salaries.png", height = 6, width = 10, units = "in", dpi = 400)


