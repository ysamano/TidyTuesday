library(tidyverse)
library(ggimage)
library(ggtext)
extrafont::loadfonts(device = "win")

villagers <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')

axis_ <- tibble(axis_x = c(rep(seq(5.5, 40, length.out = 24), 3),
                           rep(seq(1, 43, length.out = 29), 10),
                           rep(seq(1, 8.5, length.out = 6), 2),
                           rep(seq(38.5, 43, length.out = 4), 2),
                           rep(seq(1, 2.5, length.out = 2), 2),
                           rep(seq(41.5, 43, length.out = 2), 2),
                           1),
                axis_y = c(rep(seq(1, 4, length.out = 3), each = 24),
                           rep(seq(7, 20.5, length.out = 10), each = 29),
                           rep(seq(22, 23.5, length.out = 2), each = 6),
                           rep(seq(22, 23.5, length.out = 2), each = 4),
                           rep(seq(25, 26.5, length.out = 2), each = 2),
                           rep(seq(25, 26.5, length.out = 2), each = 2),
                           28))


crear_coordenadas <- function(axis_x, axis_y) {
  tibble(xmin = axis_x, 
         ymin = axis_y,
         xmax = axis_x + 1.5,
         ymax = axis_y + 1.5)
}

datos_tabla <- villagers %>% 
  select(name:personality, url) %>% 
  arrange(desc(personality)) %>% 
  bind_cols(axis_) %>% 
  group_by(name) %>% 
  mutate(list(crear_coordenadas(axis_x, axis_y))) %>% 
  unnest()

# TIBBLES PARA ENCABEZADO

logo = "2020/week_19/logo.png"
col_elem = "#252a32"

enca_info <- tibble(x = 14.5,
                    y = 31.8,
                    label = "<span style = 'font-size:7pt; color:#252a32; font-family:Roboto Condensed Light;'>
                      Animal Crossing: New Horizons is a 2020 life simulation video game developed and 
                      published by Nintendo for the Nintendo Switch.New Horizons sees the player assuming the role of 
                      a customizable character who moves to a deserted island after purchasing a package from Tom Nook. 
                      Taking place in real-time, the player can explore the island in a nonlinear fashion, gathering and 
                      crafting items, catching insects and fish, and developing the island into a community of 
                      anthropomorphic animals.</span>")

enca_legen <- datos_tabla %>% 
  filter(name == "Agnes") %>% 
  mutate(xmin = 33, 
         ymin = 29.5,
         xmax = 37.5,
         ymax = 34)

enca_etiq <- tibble(x = 39,
                    y = c(33.7, 32.7, 30.8, 29.8),
                    label = c("Gender", "Name", "Specie", "Birthday (month-day)"))

enca_legen_eti <- tribble(
  ~x,    ~y,  ~gender,
  41, 33.65, "female",
  43, 33.65,   "male")


enca_legen_line <- tribble(
  ~grupo,   ~x,   ~y,
  "gender", 37.5, 33.7,
  "gender", 38.8, 33.7,
  "name", 35.3, 33.2,
  "name", 35.3, 32.7,
  "name", 38.8, 32.7,
  "specie", 35.3, 30.3,
  "specie", 35.3, 30.8,
  "specie", 38.8, 30.8,
  "birthday", 37.5, 29.8,
  "birthday", 38.8, 29.8)


legen_color <- tibble::tribble(
  ~personality, ~axis_x, ~axis_y, ~xmin, ~ymin, ~xmax, ~ymax,
  "cranky",    19.7,    27.2,   19L,   27L,  19.4,  27.4,
  "jock",    19.7,    26.2,   19L,   26L,  19.4,  26.4,
  "lazy",    19.7,    25.2,   19L,   25L,  19.4,  25.4,
  "normal",    19.7,    24.2,   19L,   24L,  19.4,  24.4,
  "peppy",    25.7,    27.2,   25L,   27L,  25.4,  27.4,
  "smug",    25.7,    26.2,   25L,   26L,  25.4,  26.4,
  "snooty",    25.7,    25.2,   25L,   25L,  25.4,  25.4,
  "uchi",    25.7,    24.2,   25L,   24L,  25.4,  24.4
)


colores <- c('#8dd3c7','#ffed6f','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5')


# TABLA PERIODICA
p1 <- 
  ggplot(datos_tabla) +
  # ENCABEZADO
  geom_image(aes(x = 6,
                 y = 32.5,
                 image = logo),
             size = 0.22) +
  geom_textbox(data = enca_info,
               aes(x = x,
                   y = y,
                   label = label),
               hjust = 0,
               color = NA,
               fill = NA,
               width = unit(0.35, "npc")) +
  geom_rect(data = enca_legen,
            aes(xmin = xmin,
                ymin = ymin,
                xmax = xmax,
                ymax = ymax),
            color = col_elem,
            fill = "#FCFCFA",
            size = 0.8) +
  geom_text(data = enca_legen,
            aes(label = name,
                x = xmin + 2.3,
                y = ymax - 0.3),
            size = 3,
            color = col_elem,
            family = "Roboto Condensed") +
  geom_text(data = enca_legen,
            aes(x = xmin + 2.3,
                y = ymin + 0.4,
                label = species),
            size = 3,
            color = col_elem,
            family = "Roboto Condensed Light") +
  geom_text(data = enca_legen,
            aes(x = xmax - 0.38,
                y = ymin + 0.35,
                label = birthday),
            size = 2.5,
            color = col_elem,
            family = "Roboto Condensed Light") +
  geom_point(data = enca_legen,
             aes(x = xmax - 0.35,
                 y = ymax - 0.35,
                 shape = factor(gender)),
             size = 2,
             color = col_elem) +
  geom_text(data = enca_etiq,
            aes(x = x,
                y = y,
                label = label),
            size = 3,
            hjust = 0,
            color = col_elem,
            family = "Roboto Condensed") +
  geom_point(data = enca_legen_eti,
             aes(x = x, 
                 y = y,
                 shape = factor(gender)),
             size = 2) +
  geom_text(data = enca_legen_eti,
            aes(x = x + 0.3,
                y = y,
                label = gender),
            size = 3,
            hjust = 0,
            color = col_elem,
            family = "Roboto Condensed") +
  
  geom_path(data = enca_legen_line,
            aes(x = x,
                y = y,
                group = grupo),
            size = 0.5,
            linetype = 2) +
  
  #TABLA PERIODICA
  
  geom_rect(aes(xmin = xmin,
                ymin = ymin,
                xmax = xmax,
                ymax = ymax,
                fill = personality),
            color = "#f8f8ef") +
  geom_text(aes(x = xmin + 0.75,
                y = ymax - 0.15,
                label = name),
            size = 1.7,
            color = col_elem,
            family = "Roboto Condensed") +
  geom_text(aes(x = xmin + 0.75,
                y = ymin + 0.2,
                label = species),
            size = 1.4,
            color = col_elem,
            family = "Roboto Condensed Light") +
  geom_text(aes(x = xmax - 0.25,
                y = ymin + 0.2,
                label = birthday),
            size = 1.2,
            color = col_elem,
            family = "Roboto Condensed Light") +
  geom_point(aes(x = xmax - 0.15,
                 y = ymax - 0.18,
                 shape = factor(gender)),
             size = 0.8, color = col_elem) +
  geom_image(aes(x = xmin + 0.75,
                 y = ymin + 0.75,
                 image = url),
             asp = 1.5,
             size = 0.025,
             by = "height") +
  
  geom_rect(data = legen_color,
            aes(xmin = xmin,
                ymin = ymin,
                xmax = xmax,
                ymax = ymax,
                fill = personality),
            color = col_elem) +
  geom_text(data = legen_color,
            aes(x = axis_x,
                y = axis_y,
                label = personality),
            size = 2.5,
            color = col_elem,
            hjust = 0,
            family = "Roboto Condensed") +
  
  labs(caption = "Source: VillagerDB | Graphic: @ysamano28 \n") +
  xlim(1, 45) +
  ylim(1, 34) +
  scale_fill_manual(values = colores) +
  #scale_fill_brewer(palette = "Set3") +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#FCFCFA",
                                       color = "#FCFCFA"),
        plot.caption = element_text(color = col_elem,
                                    family = "Roboto Condensed"))


ggsave("2020/week_19/periodic_table_animal_crossing2.png",
       plot = p1,
       height = 9.3,
       width = 12,
       units = "in",
       dpi = 400)
