library(tidyverse)
library(rvest)

url_science <- "https://en.wikipedia.org/wiki/List_of_African-American_inventors_and_scientists"

raw_data <- read_html(url_science) %>% 
  html_table() %>% 
  .[[2]] %>% 
  janitor::clean_names() %>% 
  as_tibble() %>% 
  separate(years, into = c("birth", "death"), sep = "–")


data_graph <- 
  raw_data %>% 
  mutate(birth = case_when(name %in% c("Green, Lisa", "Ross, Archia") ~ "?",
                           name == "Mensah, Thomas" ~ "1950",
                           TRUE ~ birth),
         death = case_when(name %in% c("Olukotun, Kunle", "Tyree, G. Bernadette", "Green, Lisa", "Ross, Archia") ~ "?",
                           name == "Easley, Annie" ~ "2011",
                           TRUE ~ death),
         name = case_when(name == "Henry Brown" ~ "Brown, Henry",
                          TRUE ~ name))

data_graph <- data_graph %>% 
  arrange(birth) %>% 
  separate(name, c("name1", "name2", "name3"), sep = ", ") %>% 
  mutate_all(funs(replace_na(., ""))) %>% 
  mutate(name_i = str_c(str_sub(name2, 1, 1), str_sub(name1, 1, 1), " ", name3),
         name_c = str_c(name2, "\n", name1, " ", name3),
         birth_death = str_c(birth, death, sep = " - ")) %>% 
  bind_cols(expand_grid(y = 13:1, x = 1:10))


p1 <- ggplot(data_graph, aes(x, y)) +
  geom_rect(aes(xmin = x, ymin = y, xmax = x + 1, ymax = y + 1),
            fill = "#27233A",
            colour = "#f0efed") +
  geom_text(aes(x + 0.52, y + 0.85, label = name_i),
            size = 5.5,
            colour = "gray95",
            fontface = "bold",
            family = "Roboto Condensed") +
  geom_text(aes(x + 0.5, y + 0.5, label = name_c),
            size = 3.5,
            colour = "gray80",
            family = "Roboto Condensed") +
  geom_text(aes(x + 0.5, y + 0.15, label = birth_death),
            size = 3,
            colour = "gray80",
            family = "Roboto Condensed") +
  labs(title = "African-American Inventors and Scientists",
       caption = 'Source: Wikipedia, "List of African-American inventors and scientists" | Graphic: @ysamano28') +
  scale_x_continuous(limits = c(1, 11), expand = c(0, 0)) +
  scale_y_continuous(limits = c(1, 14), expand = c(0, 0)) +
  theme_ybn_w(base_size = 13, base_family = "Roboto Condensed") +
  theme(plot.title = element_text(size = 35, 
                                  hjust = 0.5, 
                                  colour = "#221F33", 
                                  margin = margin(t = 10, b = 20)),
        axis.title = element_blank(),
        axis.text = element_blank())

ggsave("2020/week_24/inventors_and_scientists.png", p1, height = 13, width = 11, units = "in", dpi = 300)
