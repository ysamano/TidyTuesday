library(tidyverse)
library(ggforce)

covers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/covers.csv')

crear_radios <- function(total_var, radio) {
  
  tibble(variable = 1:total_var,
         radio = seq(radio, radio * total_var, length.out = total_var))
  
}

data_covers <- covers %>%
  separate(characters_visualized, letters, sep = "\r\n") %>% 
  pivot_longer(cols = letters, names_to = "letters", values_to = "character", values_drop_na = T) %>%
  filter(character != "") %>% 
  group_by(issue) %>% 
  summarise(total = n()) %>% 
  mutate(axis_x = rep(seq(2, 24, by = 2), length = n()),
         axis_y = rep(seq(42, 2, by = -2), each = 12, length = n()),
         issue = if_else(issue == 97, "Issue 97", as.character(issue)))


data_graph <- data_covers %>% 
  group_by(issue) %>% 
  mutate(list(crear_radios(total, 0.1))) %>% 
  unnest()

p1 <- ggplot(data_graph) +
  geom_circle(aes(x0 = axis_x,
                  y0 = axis_y,
                  r = radio),
              color = "#00BFC4",
              n = 700,
              size = 0.3) +
  geom_text(data = data_covers,
            aes(axis_x, axis_y - 0.8, label = issue),
            size = 2.5, 
            colour = "white", 
            family = "Source Sans Pro") +
  labs(title = "Uncanny X-Men Covers",
       subtitle = "Characters depicted on each cover in Chris Claremont era",
       caption = "Source: The Claremont Run | Graphic: @ysamno28") +
  coord_fixed() +
  theme_ybn_b(colour_background = "#070C29",
              title_size = 25,
              title_hjust = 0.5,
              subtitle_hjust = 0.5,
              axis_grid = F,
              axis_text = F, 
              axis_title = F)

ggsave("2020/week_27/comic.png", p1, height = 12.35, width = 8.5, units = "in", dpi = 300)

