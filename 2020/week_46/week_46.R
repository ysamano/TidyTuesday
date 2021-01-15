library(tidyverse)
library(countrycode)
library(geofacet)

mobile <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv')

data_graph <- mobile %>% 
  filter(continent == "Africa",
         year >= 2000) %>% 
  mutate(iso2 = countrycode(entity,
                            origin = "country.name",
                            destination = "iso2c"),
         iso2 = if_else(iso2 == "NA", "NAM", iso2)) %>% 
  filter(!is.na(iso2))

africa_grid <- africa_countries_grid1 %>% 
  mutate(name = case_when(name == "Central African Republic" ~ "CAF",
                          name == "Republic of the Congo" ~ "COG",
                          name == "Democratic Republic of the Congo" ~ "COD",
                          name == "Equatorial Guinea" ~ "GNQ",
                          name == "São Tomé and Principe" ~ "STP",
                          TRUE ~ name))

p1 <- ggplot(data_graph) +
  geom_rect(aes(xmin = 2000, ymin = 0,
                xmax = 2017, ymax = 200),
            fill = "#26262E") +
  geom_area(aes(year, mobile_subs),
            fill = "#008080") +
  facet_geo( ~ iso2,
             grid = africa_grid,
             label = "name") +
  scale_x_continuous(breaks = seq(2000, 2015, by = 5),
                     labels = c("'00", "'05", "'10", "'15")) +
  labs(title = "African Mobile Phone Subscriptions, 2000-2017",
       subtitle = "Mobile subscriptions per 100 people",
       caption = "Source data: ourworldindata.org | Graphic: Yobanny Samano") +
  theme_ybn(base_colour = "#E8EADC",
            colour_background = "#1E1D23",
            axis_text_size = 7,
            title_face = "bold",
            subtitle_margin_b = 45,
            title_hjust = 0.5,
            caption_hjust = 0.5,
            plot_margin = margin(30, 30, 30, 30),
            strip_family = "Roboto Condensed Light",
            strip_colour = "#E8EADC",
            axis_grid = F,
            axis_title = FALSE)

ggsave(filename = "2020/week_46/Mobile_Phone_Subscriptions.png", plot = p1, height = 11, width = 8.5, units = "in",type = "cairo")
