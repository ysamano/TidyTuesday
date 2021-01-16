library(tidyverse) 
library(treemapify)

fifa_audience <- read_csv("2018/week_11/week11_fifa_audience.csv")

data_fifa <- fifa_audience %>% 
    pivot_longer(population_share:gdp_weighted_share, names_to = "var", values_to = "share")

fifa_plot <- ggplot(data_fifa,
                    aes(area = share,
                        subgroup = confederation,
                        fill = confederation,
                        label = country)) +
    geom_treemap(colour = "#FDFDFB") +
    geom_treemap_text(colour = "gray30",
                      fontface = "italic",
                      size = 11,
                      min.size = 2,
                      reflow = T) +
    geom_treemap_subgroup_border(colour = "#FDFDFB",
                                 size = 3) +
    geom_treemap_subgroup_text(alpha = 0.6,
                               colour = "gray50",
                               fontface = "bold") +
    scale_fill_brewer(type = "qual", palette = 7) +
    facet_wrap( ~ var,  labeller = labeller(var = c("gdp_weighted_share" = "GDP Weighted Share",
                                                    "population_share" = "Population Share",
                                                    "tv_audience_share" = "TV Audience Share"))) +
    labs(title = "The 2010 FIFA World Cup",
         caption ="Source: FiveThirtyEight") +
    theme_ybn(title_margin_b = 20,
              strip_size = 12) +
    theme(legend.position = "none")

ggsave("2018/week_11/fifa_plot.png", fifa_plot, height = 8, width = 11, units = "in", type = "cairo")
