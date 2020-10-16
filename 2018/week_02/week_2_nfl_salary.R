library(tidyverse)
library(ggbeeswarm)

nfl_salary <- readxl::read_excel("2018/week_02/nfl_salary.xlsx")

salary <- nfl_salary %>% 
    pivot_longer(cols = -year,
                 names_to = "position",
                 values_to = "salary_position") %>% 
    filter(!is.na(salary_position))

p1 <- ggplot(salary, aes(year, salary_position / 1000000, group = year)) +
    geom_quasirandom(size = 0.7,
                     alpha = .3,
                     colour = "#54278f") +
    facet_wrap( ~ position, ncol = 5) +
    scale_y_continuous(labels = scales::dollar_format(suffix = "m")) +
    labs(x = "Year", y = "Salary",
         title = "NFL Positional Salaries",
         caption = "Source: spotrac.com | Graphic: @ysamano28") +
    theme_ybn_w(base_size = 7)

ggsave("2018/week_02/nfl_salaries.png", p1, height = 6.8, width = 8.8, units = "in", type = "cairo")
