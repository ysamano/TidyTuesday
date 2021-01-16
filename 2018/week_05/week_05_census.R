library(tidyverse)
library(ggridges)

data <- read_csv("2018/week_05/week5_acs2015_county_data.csv")

p1 <- ggplot(data, aes(IncomePerCap,State)) + 
  geom_density_ridges(scale = 2,
                      fill = "#004F8B",
                      color = "#004F8B",
                      alpha = 0.9) +
  scale_x_continuous(labels = scales::dollar,
                     n.breaks = 7) +
  labs(title = "US Census Demographic 2015",
       subtitle = "Income Per Capita by State",
       caption = "US Census Demographic Data 2015 | Graphic: @ysamano28") +
  theme_ybn(title_hjust = 0.5,
            caption_hjust = 0.5,
            axis_text = 8.5) +
  theme(axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

ggsave("2018/week_05/census.png", p1, height = 11, width = 8, units = "in", type = "cairo")
