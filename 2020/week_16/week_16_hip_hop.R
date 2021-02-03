library(tidyverse)
library(packcircles)
library(patchwork)
library(ggtext)

rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

rankings2 <- rankings %>%
    select(ID, title, artist, year, points, gender) %>% 
    mutate(colour = case_when(gender == "male" ~ "#0b53c1",
                              gender == "female" ~ "#ff0055", 
                              TRUE ~ "#ffec1b"))

packing <- rankings2 %>% 
    arrange(year) %>% 
    circleProgressiveLayout(sizecol = "points", sizetype = 'area') %>% 
    mutate(radius = radius * 0.9)

dat.gg <- circleLayoutVertices(packing, npoints = 200)

labels_cir <- rankings2 %>% 
    arrange(year) %>% 
    select(ID, points) %>% 
    bind_cols(packing)

songs <- rankings2 %>% 
    mutate(nom_ra = str_c(ID, ". ", title, " - ", artist),
           axis_x = rep(1:4, each = 79, length = n()),
           axis_y = rep(79:1, length = n()))

p1 <- 
    ggplot() + 
    geom_polygon(data = dat.gg,
                 aes(x, y, group = id, fill = factor(id)),
                 colour = NA) +
    geom_text(data = labels_cir,
              aes(x, y, size = points, label = ID),
              color = "#1E1D23",
              #fontface = "bold",
              family = "Roboto Condensed Light") +
    scale_size_continuous(range = c(1, 3)) +
    scale_fill_manual(values = rankings2 %>%
                          arrange(year) %>% pull(colour)) +
    coord_equal() + 
    theme_void() +
    theme(legend.position="none")


p2 <- 
    ggplot(songs, aes(axis_x, axis_y)) + 
    geom_text(aes(label = nom_ra),
              size = 1.5, 
              hjust = 0,
              color = "#C0C0C4",
              family = "Roboto Condensed Light") +
    scale_x_continuous(limits = c(0.9, 5), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0.5, 79.5), expand = c(0, 0)) +
    theme_void()

patchwork <- p1 / p2 + plot_layout(heights = c(1, 0.9)) + 
    plot_annotation( title = "The best hip-hop songs of all time",
                     subtitle = "269 songs by <span style='color:#0b53c1'>**man rappers**</span><br>
                     23 songs by <span style='color:#ff0055'>**women rappers**</span><br>
                     19 songs by <span style='color:#ffec1b'>**mixed bands or collaborations rappers**</span>",
                     caption = "The size of the bubble represents the number of points gained from all songs on the polling list of the BBC\nSource: BBC Music | Graphic: @ysamano28") &
    theme(plot.background = element_rect(fill = "#1E1D23",
                                         color = "#1E1D23"),
          plot.title = element_text(family = "Roboto Condensed",
                                    face = "bold",
                                    size = 30, 
                                    color = "white",
                                    hjust = 0.5, 
                                    margin = margin(t = 15, b = 10)),
          plot.subtitle = element_markdown(family = "Roboto Condensed Light",
                                           face = "bold",
                                           size = 15, 
                                           color = "white",
                                           hjust = 0.5,
                                           margin = margin(b = 0)),
          plot.caption = element_text(family = "Roboto Condensed Light",
                                      size = 8,
                                      color = "white",
                                      hjust = 0,
                                      margin = margin(t = 10, b = 5, l = 15)))

ggsave("2020/week_16/hip_hop.png", plot = patchwork, height = 12, width = 8.5, units = "in", dpi = 300)
