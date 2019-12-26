
library(tidyverse)
library(purrr)
library(lubridate)

name_files <- list.files("week_10/PublicTripData", full.names = T)

data_bike <- name_files %>% 
    map_dfr(~ data.table::fread(.x))

write_csv(data_bike, "week_10/data_biketown.csv", na = "")

data_bike <- read_csv("week_10/data_biketown.csv")

funModeling::df_status(data_bike)


clear_data <- data_bike %>%
    select(PaymentPlan, StartDate) %>% 
    filter(!is.na(StartDate)) %>% 
    mutate(startdate = mdy(StartDate),
           year = year(startdate),
           week = week(startdate),
           week_day = wday(startdate, label = T, locale = "C", week_start = 1))


total_trips <- clear_data %>% 
    group_by(year, week, week_day) %>% 
    summarise(trips = n()) %>%
    ungroup() %>% 
    mutate(quantile = cut(trips,
                          breaks = quantile(trips, probs = seq(0, 1, by= 0.2)),
                          include.lowest = T, 
                          ordered_result = T))


subscriber_trips <- clear_data %>% 
    filter(PaymentPlan == "Subscriber") %>% 
    group_by(year, week, week_day) %>% 
    summarise(trips = n()) %>%
    ungroup() %>% 
    mutate(quantile = cut(trips,
                          breaks = quantile(trips, probs = seq(0, 1, by= 0.2)),
                          include.lowest = T, 
                          ordered_result = T))


casual_trips <- clear_data %>% 
    filter(PaymentPlan == "Casual") %>% 
    group_by(year, week, week_day) %>% 
    summarise(trips = n()) %>%
    ungroup() %>% 
    mutate(quantile = cut(trips,
                          breaks = quantile(trips, probs = seq(0, 1, by= 0.2)),
                          include.lowest = T, 
                          ordered_result = T))


# Theme of plot

scale_plot <- scale_x_continuous(expand = c(0, 0),
                       breaks = map_dbl(seq.Date(ymd("2018/1/1"), by = "month", length.out = 12), week),
                       labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

guides_plot <- guides(fill = guide_legend(title = "Number of Trips",
                                          label.position = "bottom",
                                          label.hjust = .5,
                                          title.position = 'top',
                                          keywidth = 4, keyheight = .8))
theme_heatmap <- function(...) {
    theme_minimal() +
        theme(plot.title = element_text(size = 20, face = "bold"),
              plot.margin = margin(10, 10, 10, 10),
              legend.position = "bottom",
              strip.text.x = element_text(size = 13, hjust = 1, face = "plain"),
              axis.title = element_text(size = 12, hjust = 1),
              axis.text = element_text(colour = "Black"))
}



# plot total trips

total_plot <- ggplot(total_trips, aes(week, week_day, fill = quantile)) +
    geom_tile(color = 'gray30') +
    facet_wrap('year', ncol = 1) +
    labs(x = "Week", y = "Day of the Week",
         title = "Number of Trips per Weekday", caption = "Source: BIKETOWNpdx") +
    scale_fill_brewer(palette = "YlOrRd", direction = 1,
                      labels = c("20 - 325", "325 - 515", "515 - 826", 
                                 "826 - 1410", "1410 - 2990")) +
    scale_plot +
    guides_plot +
    theme_heatmap()

ggsave("week_10/total_plot.png", total_plot, height = 7, width = 12, units = "in", dpi = 500)


# Plot Subscriber trips
table(subscriber_trips$quantile)

subscriber_plot <- ggplot(subscriber_trips, aes(week, week_day, fill = quantile)) +
    geom_tile(color = 'gray30') +
    facet_wrap('year', ncol = 1) +
    labs(x = "Week", y = "Day of the Week",
         title = "Number of Trips per Weekday", subtitle = "Payment Plan: Subscriber",
         caption = "Source: BIKETOWNpdx") +
    scale_fill_brewer(palette = "YlOrBr", direction = 1,
                      labels = c("19 - 221", "221 - 350", "350 - 473", 
                                 "473 - 636", "636 - 1020")) +
    scale_plot +
    guides_plot +
    theme_heatmap()

ggsave("week_10/subscriber_plot.png", subscriber_plot, height = 7, width = 12, units = "in", dpi = 500)


# Plot Casual trips
table(casual_trips$quantile)

casual_plot <- ggplot(casual_trips, aes(week, week_day, fill = quantile)) +
    geom_tile(color = 'gray30') +
    facet_wrap('year', ncol = 1) +
    labs(x = "Week", y = "Day of the Week",
         title = "Number of Trips per Weekday", subtitle = "Payment Plan: Casual",
         caption = "Source: BIKETOWNpdx") +
    scale_fill_brewer(palette = "Oranges", direction = 1,
                      labels = c("1 - 72", "72 - 149", "149 - 365", 
                                 "365 - 696", "696 - 2310")) +
    scale_plot +
    guides_plot +
    theme_heatmap()

ggsave("week_10/casual_plot.png", casual_plot, height = 7, width = 12, units = "in", dpi = 500)











