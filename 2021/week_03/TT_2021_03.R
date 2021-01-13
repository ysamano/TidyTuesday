library(tidyverse)
library(treemap)
library(ggfittext)
library(scales)
library(ggtext)

artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')
artists <- readr::read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv")

artwork_artist <- artwork %>% 
  left_join(artists,by = c("artistId" = "id")
            ) %>% 
  mutate(gender = case_when(str_detect(artist, "British") ~ "Other",
                            str_detect(artist, "Art & Language") ~ "Male",
                            TRUE ~ gender),
         artist = case_when(str_detect(artist, "British") ~ "British School",
                            TRUE ~ artist)
         ) %>% 
  filter(!is.na(gender)) %>% 
  group_by(artist, gender) %>% 
  summarise(total = n()) %>% 
  #filter(name != "Turner, Joseph Mallord William") %>% 
  ungroup() %>% 
  mutate(id_tree = row_number())


################### Grafica 1 #####################

# From Laura Navarro: https://github.com/lau-cloud/florence/blob/master/florence_nightingale_competition.Rmd
data_tree <- treemap(artwork_artist,
                     index=c("artist"),
                     vSize="total",
                     type="categorical",
                     vColor = "gender",
                     algorithm = "pivotSize",
                     sortID = "id_tree",
                     mirror.y = TRUE,
                     mirror.x = TRUE,
                     border.lwds = 0.7,
                     aspRatio = 5/3)

data_ggplot <- data_tree[["tm"]] %>% 
  as_tibble() %>% 
  arrange(desc(vSize)) %>% 
  mutate(rank = row_number(),
         xmax = x0 + w,
         ymax = y0 + h,
         label_artist = str_glue("{artist}\n({comma(vSize, accuracy = 1)})")
         )

how_to_read <- tibble(label = c("**How to read:**",
                                "Each rectangle represents an artist",
                                "The size represents the number of artworks",
                                "The color represents the gender of the artist, 
                                <span style='color:#C95C35'>**female**</span> or 
                                <span style='color:#0A7575'>**male**</span>"),
                      x = c(0.5, 0.5, 0.5, 0.5),
                      y = c(-0.07, -0.1, -0.13, -0.16))


p1 <- ggplot(data_ggplot) +
  geom_rect(aes(xmin = x0,
                ymin = y0,
                xmax = xmax,
                ymax= ymax,
                fill = vColor),
            size = 0.1,
            colour = "#1E1D23",
            alpha = 0.9) +
  geom_fit_text(data = data_ggplot %>% filter(rank <= 200),
                aes(xmin = x0, 
                    xmax = xmax, 
                    ymin = y0,
                    ymax = ymax,
                    label = label_artist),
                colour = "#E8EADC",
                family = "Lora",
                min.size = 4,
                reflow = TRUE) +
  geom_richtext(data = how_to_read,
                aes(x, y, label = label), 
                size = 3.5,
                color = "#E8EADC",
                fill = NA,
                label.color = NA,
                hjust = 0.5,
                family = "Roboto Condensed Light") +          
  labs(title = "The Tate Collection's Artists",
       caption = "Data Source: Tate Art Museum (github.com/tategallery/collection) | Design: Yobanny Samano") +
  scale_fill_manual(values = c("#C95C35", "#0A7575", "#8f9089")) +
  theme_void() +
  theme(text = element_text(colour ="#E8EADC"),
        legend.position = "none",
        plot.background = element_rect(fill = "#1E1D23",
                                       colour = "#1E1D23"),
        plot.margin = margin(30, 10, 20, 10),
        plot.title = element_text(family = "Playfair Display",
                                  size = 25,
                                  hjust = 0.5),
        plot.caption = element_text(family = "Roboto Condensed Light", 
                                    size = 9,
                                    hjust = 0.5)
        )

ggsave(filename = "2021/week_02/TATE1.png", plot = p1, height = 9, width = 11, units = "in", type = "cairo")


########## Grafica 2 #############

data_tree <- treemap(artwork_artist %>% filter(total != 39389),
                      index=c("artist"),
                      vSize="total",
                      type="categorical",
                      vColor = "gender",
                      algorithm = "pivotSize",
                      sortID = "id_tree",
                      mirror.y = TRUE,
                      mirror.x = TRUE,
                      border.lwds = 0.7,
                      aspRatio = 5/3)

data_ggplot <- data_tree[["tm"]] %>% 
  as_tibble() %>% 
  arrange(desc(vSize)) %>% 
  mutate(rank = row_number(),
         xmax = x0 + w,
         ymax = y0 + h,
         label_artist = str_glue("{artist}\n({comma(vSize, accuracy = 1)})")
         )

p1 <- ggplot(data_ggplot) +
  geom_rect(aes(xmin = x0,
                ymin = y0,
                xmax = xmax,
                ymax= ymax,
                fill = vColor),
            size = 0.1,
            colour = "#1E1D23",
            alpha = 0.9) +
  geom_fit_text(data = data_ggplot %>% filter(rank <= 300),
                aes(xmin = x0, 
                    xmax = xmax, 
                    ymin = y0,
                    ymax = ymax,
                    label = label_artist),
                colour = "#E8EADC",
                family = "Lora",
                min.size = 3.5,
                reflow = TRUE) +
  geom_richtext(data = how_to_read,
                aes(x, y, label = label), 
                size = 3.5,
                color = "#E8EADC",
                fill = NA,
                label.color = NA,
                hjust = 0.5,
                family = "Roboto Condensed Light") +
  labs(title = "The Tate Collection's Artists",
       subtitle = "(excluding William Turner)",
       caption = "Data Source: Tate Art Museum (github.com/tategallery/collection) | Design: Yobanny Samano") +
  scale_fill_manual(values = c("#C95C35", "#0A7575", "#8f9089")) +
  theme_void() +
  theme(text = element_text(colour ="#E8EADC"),
        legend.position = "none",
        plot.background = element_rect(fill = "#1E1D23",
                                       colour = "#1E1D23"),
        plot.margin = margin(30, 10, 20, 10),
        plot.title = element_text(family = "Playfair Display",
                                  size = 25,
                                  hjust = 0.5),
        plot.subtitle = element_text(family = "Playfair Display",
                                     size = 14,
                                     hjust = 0.5),
        plot.caption = element_text(family = "Roboto Condensed Light", 
                                    size = 9,
                                    hjust = 0.5)
        )

ggsave(filename = "2021/week_02/TATE2.png", plot = p1, height = 9, width = 11, units = "in", type = "cairo")
