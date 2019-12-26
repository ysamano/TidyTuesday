
library(tidyverse)

salary <- read_csv("week_4/week4_australian_salary.csv")

top_male <- salary %>% 
    filter(gender == "Male") %>% 
    top_n(30, average_taxable_income) %>% 
    select(occupation)

gap <- salary %>% 
    filter(occupation %in% top_male$occupation)

salary_plot <- ggplot(gap, aes(average_taxable_income, occupation)) +
    geom_line(aes(group = occupation)) +
    geom_point(aes(color = gender), size = 2) +
    scale_color_manual(values = c("#dd3497", "#253494")) +
    scale_x_continuous(labels = scales::dollar, limits = c(4000, 650000)) +
    labs(title = "Austraalia´s 30 Highest Paying Jobs",
         subtitle = "Average Taxable Income by Males Versus Females") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.position = "top",
          axis.title = element_blank())


ggsave("week_4/salary_plot.png", salary_plot, height = 7, width = 6, units = "in", dpi = 400)
