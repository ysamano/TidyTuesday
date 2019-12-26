
library(tidyverse)
library(lubridate)


hurricane <- read_csv("week_12/week12_mediacloud_hurricanes.csv")
state_hurricane <- read_csv("week_12/week12_mediacloud_states.csv")

hurricane <- hurricane %>% 
    gather(mention, sentences, -Date) %>% 
    filter(sentences != 0) %>% 
    mutate(Date = mdy(Date))

state_hurricane <- state_hurricane %>%
    rename(`Puerto Rico` = `"Puerto Rico"`) %>% 
    gather(mention, sentences, -Date) %>% 
    mutate(Date = mdy(Date))


data_mention <- bind_rows(hurricane, state_hurricane) %>% 
    mutate(mention = factor(mention, 
                            levels = c("Jose", "Maria", "Irma", "Harvey",
                                       "Puerto Rico", "Florida", "Texas")),
        ty_ment = if_else(mention %in% c("Harvey", "Irma", "Maria", "Jose"), 
                             "By Hurricane", "By State"))

hurricane_plot <- 
    ggplot(data_mention, aes(Date, mention, colour = mention)) +
    geom_point(aes(size = sentences), alpha = .7) +
    scale_size_area(max_size = 10) +
    guides(color = F,
           size = guide_legend(title = "Sentences \nper day")) +
    scale_color_manual(values = c("#006837", "#b30000", "#004A94", "#1F8FFF",
                                  "#b30000", "#004A94", "#1F8FFF")) +
    scale_x_date(date_breaks = "3 days", date_labels = "%m/%d") +
    labs(title = "Number of sentences mentioning each hurricane and place it made landfall",
         caption = "Source: FiveThirtyEight") +
    facet_wrap(~ ty_ment, ncol = 1, scales = "free_y") +
    ggthemes::theme_fivethirtyeight(base_size = 10) + 
    theme(strip.text.x = element_text(hjust = 0, face = "bold"),
          panel.grid.minor = element_blank(), 
          axis.title = element_blank(), 
          panel.grid.major.y = element_line(colour = "gray60"),
          legend.title = element_text(size = 8), 
          plot.caption = element_text(size = 8))

ggsave("week_12/hurricane_plot.png", hurricane_plot, height = 5, width = 9, units = "in", dpi = 500)

