
library(tidyverse)

comic_data <- read_csv("week_9/week9_comic_characters.csv")

glimpse(comic_data)

funModeling::df_status(comic_data)

clear_data <- comic_data %>% 
    select(publisher, sex, year) %>% 
    filter(!is.na(sex), !is.na(year),
           sex %in% c("Male Characters", "Female Characters")) %>% 
    group_by(year, publisher, sex) %>% 
    summarise(conteo =n())

clear_data <- clear_data %>% 
    mutate(publ_sex = case_when(
        publisher == "DC" & sex == "Male Characters" ~ "DC/Male", 
        publisher == "DC" & sex == "Female Characters" ~ "DC/Female",
        publisher == "Marvel" & sex == "Male Characters" ~ "Marvel/Male",
        publisher == "Marvel" & sex == "Female Characters" ~ "Marvel/Female"))

funModeling::df_status(clear_data)

comic_plot <- ggplot(clear_data, aes(conteo, year, size = conteo, colour = publ_sex)) + 
    geom_point(alpha = .7) +
    scale_size_area(max_size = 8)+
    scale_y_continuous(breaks=seq(1935, 2013, 2), limits = c(1935, 2013)) +
    scale_x_continuous(expand = c(0, 0), limits = c(-5, 430)) +
    scale_color_manual(values = c("#1F8FFF", "#004A94", "#BC0A7B", "#CD1351")) +
    guides(colour = guide_legend(override.aes = list(size = 5))) +
    labs(title = "Number of New Comic Book Character by Year and Sex",
         caption = "Source: FiveThirtyEight") +
    ggthemes::theme_fivethirtyeight() +
    theme(legend.title = element_blank(),
          plot.caption = element_text(size = 7),
          axis.text = element_text(size = 7))

ggsave("week_9/comic_plot.png", comic_plot, height = 6, width = 10, units = "in", dpi = 500)





