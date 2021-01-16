library(ggplot2)
library(waffle)

starbucks <- readxl::read_excel("week_6/week6_coffee_chains.xlsx", sheet = "starbucks")
timhorton <- readxl::read_excel("week_6/week6_coffee_chains.xlsx", sheet = "timhorton")
dunkin <- readxl::read_excel("week_6/week6_coffee_chains.xlsx", sheet = "dunkin")

num_places <- c(Starbucks = nrow(starbucks),
                `Tim Hortons` = nrow(timhorton),
                `Dunkin Donuts\n(only USA)` = nrow(dunkin))

coffe_plot <- waffle(num_places / 30,
                     rows = 25,
                     colors = c("#238b45", "#cb181d", "#dd3497"),
                     legend_pos = "bottom",
                     title = "Number of Cofee Stores in the World", 
                     xlab = "1 square is 30 places")

ggsave("week_6/coffe_plot.png", coffe_plot, height = 5, width = 8, units = "in", dpi = 500)
