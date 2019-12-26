
library(tidyverse)
library(readxl)

global_mort <- read_excel("2018/week_03/global_mortality.xlsx")

global_mort_tidy <- global_mort %>% 
    pivot_longer(cols = 4:35, names_to = "cause", values_to = "percent") %>% 
    filter(country == "World",
           year %in% c(1990, 2016))

x11()

ggplot(global_mort_tidy, aes(year, percent, fill = cause)) + 
    geom_col() +
    theme_minimal() +
    theme(legend.position = "none")

