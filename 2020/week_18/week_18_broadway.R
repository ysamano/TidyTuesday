library(tidyverse)
library(lubridate)
library(ggbump)
library(ggtext)

grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv',
                           guess_max = 10000)

cpi <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/cpi.csv')

pre_1985_starts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/pre-1985-starts.csv')

cpi <- cpi %>%
    mutate(jan_2020_dollars = cpi[year_month == "2020-01-01"] / cpi)

datos <- grosses %>% 
    mutate_at(vars(weekly_gross:seats_sold), ~ ifelse(performances + previews == 0 | . == 0, NA, .)) %>% 
    select(week_ending, show, weekly_gross, avg_ticket_price, seats_sold) %>% 
    # Covertimos a dolares de 2020
    mutate(year_month = floor_date(week_ending, unit = "month")) %>%
    left_join(cpi, by = "year_month") %>%
    mutate_at(vars(weekly_gross, avg_ticket_price), ~ . * jan_2020_dollars) %>%
    select(-year_month:-jan_2020_dollars) %>% 
    # Pasos para sacar las ejecuciones
    group_by(show) %>%
    # Se acomoda por fecha desde la primera
    arrange(week_ending) %>%
    #run number es el numero de ejecuciones que ha tenido
    mutate(run_number = cumsum(row_number() == 1 | week_ending - lag(week_ending) > 90)) %>%
    group_by(show, run_number) %>%
    # week_of_run es el conteo de semanas que lleva la presentacion  
    mutate(week_of_run = row_number()) %>%
    ungroup()

calculate_weeks_since_start <- function(x) {
    as.integer(pmax(1, difftime("1985-06-09", x, units = "weeks")))
}


pre_1985 <- datos %>%
    group_by(show, run_number) %>%
    filter(min(week_ending) == "1985-06-09") %>%
    ungroup() %>%
    select(week_ending, show) %>%
    left_join(pre_1985_starts, by = "show") %>%
    group_by(show) %>%
    mutate(week_of_run_originals = calculate_weeks_since_start(start_date) + row_number()) %>%
    ungroup() %>%
    select(week_ending, show, week_of_run_originals)

grosses_clean <- datos %>%
    left_join(pre_1985, by = c("show", "week_ending")) %>%
    # En este paso sustituye los valores de las obras que fueron estrenadas antes de 1985
    mutate(week_of_run = coalesce(week_of_run_originals, week_of_run)) %>%
    select(-week_of_run_originals)

resumen <- grosses_clean %>% 
    group_by(show, run_number) %>% 
    summarise(total_gross = sum(weekly_gross, na.rm = T),
              total_seats = sum(seats_sold, na.rm = T),
              fecha_min = min(week_ending),
              fecha_max = max(week_ending)) %>% 
    ungroup() %>% 
    top_n(30, wt = total_gross) %>% 
    arrange(desc(total_gross)) %>% 
    mutate(total_gross = total_gross/1000000,
           num_label = str_c("[", row_number(), "]"),
           label = str_c(num_label, " ", show),
           axis_x = rep(ymd(c("1985/01/01", "1997/01/01")), each = 15),
           axis_y = c(seq(2000, 1200, length.out = 15), seq(2000, 1200, length.out = 15)))

p1 <- ggplot(resumen) +
    geom_sigmoid(aes(x = fecha_min,
                     y = 0,
                     xend = fecha_max,
                     yend = total_gross,
                     group = factor(show)),
                 direction = "y", 
                 size = 0.3, 
                 smooth = 6.1, 
                 alpha = 0.6, 
                 color = "grey70") +
    geom_point(aes(fecha_max,
                   total_gross,
                   size = total_gross),
               color = "#ff0055",
               alpha = 0.7) +
    scale_size(range = c(3, 40)) +
    geom_point(aes(fecha_max,
                   total_gross),
               size = 1.5,
               shape = 18,
               color = "White") +
    geom_point(aes(fecha_min, 0),
               size = 1.5,
               shape = 17,
               color = "grey70") +
    geom_text(aes(fecha_max + 250,
                  total_gross,
                  label = num_label),
              size = 2.2,
              color = "grey80") +
    geom_text(aes(axis_x,
                  axis_y,
                  label = label),
              size = 3.5,
              #fontface = "bold",
              family = "Roboto Condensed Light",
              hjust = 0,
              color = "grey80") +
    scale_x_date(date_labels = "\'%y",
                 breaks = seq(as.Date("1985/1/1"), by = "2 years", length.out = 19),
                 expand = expand_scale(mult = c(0.05, .13))
    ) +
    scale_y_continuous(labels =  scales::label_dollar(suffix = "M"),
                       n.breaks = 8,
                       position = "right",
                       expand = expand_scale(mult = c(0, .1))
    ) +
    labs(title = "Los 30 espectáculos teatrales más exitosos de",
         subtitle = "Broadway",
         caption = "Source: Playbill | Graphic: @ysamano28",
         y = "Ingreso acumulado bruto desde la primera semana de presentación\n") +
    theme_ybn(colour_background = "#1b1f2b",
              base_colour = "gray95",
              title_face = "bold",
              title_hjust = 0.5,
              subtitle_size = 30,
              subtitle_face = "bold",
              caption_hjust = 0.5,
              axis_text_size = 9) +
    theme(legend.position = "none",
          panel.grid = element_line(colour = "gray20", size = 0.1),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.title.x = element_blank()
          )

ggsave("2020/week_18/broadway.png", p1, height = 11, width = 8, units = "in", dpi = 300)
