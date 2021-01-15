library(tidyverse)

firsts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')

firsts <- firsts %>% 
  mutate(decade = year - year %% 10) %>% 
  group_by(decade) %>% 
  arrange(category) %>% 
  mutate(index = row_number()) %>% 
  ungroup()


p1 <- 
  ggplot(firsts, aes(decade, 1, group = index, fill = category)) +
  geom_bar(stat = 'identity',
           width = 8,
           color = "#1b1f2b",
           size = 1.3) +
  annotate("text",
           x = 1820,
           y = 63, 
           label = "The First Achievements by African Americans", 
           size = 9,
           color = "white",
           fontface = "bold",
           family = "Roboto Condensed") +
  annotate("text",
           x = 1755,
           y = 17, 
           label = "1738 - First free African-American\ncommunity: Gracia Real de Santa Teresa de Mose\n(later named Fort Mose) in Florida", 
           hjust = 0,
           size = 3,
           color = "gray80",
           family = "Roboto Condensed") +
  annotate(geom ="curve",
           x = 1728,
           y = 1.5, 
           xend = 1752, 
           yend = 17, 
           curvature = -0.35,
           color = "gray80", 
           arrow = arrow(length = unit(2, "mm"), type = "closed")) +
  labs(caption = 'Source: Wikipedia, "List of African-American firsts" | Graphic: @ysamano28') +
  scale_x_continuous(name = "Decade",
                     breaks = seq(1730, 2010, by = 10),
                     labels = paste0(seq(1730, 2010, by = 10), "s"),
                     expand = c(0, 1)) +
  scale_y_continuous(expand = c(0, 1)) +
  scale_fill_brewer(palette = "Dark2",
                    guide = guide_legend(direction = "horizontal", nrow = 1)) +
  theme_ybn(base_size = 8,
            base_colour = "white",
            colour_background = "#1b1f2b",
            plot_margin = margin(20, 20, 20, 20),
            axis_grid = FALSE) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.key.size = unit(.2, "cm"),
        legend.key.width = unit(0.7,"cm"),
        legend.position = c(0.34, 0.90),
        legend.text = element_text(size = 7),
        legend.title = element_blank())

ggsave("2020/week_24/African-American_Achievements.png", p1, height = 7, width = 11.5, units = "in", dpi = 300)
