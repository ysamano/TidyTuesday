
library(tidyverse) 
library(treemapify)

fifa_audience <- read_csv("week_11/week11_fifa_audience.csv")

fifa_plot <- ggplot(fifa_audience,
                    aes(area = tv_audience_share,
                        subgroup = confederation,
                        fill = gdp_weighted_share,
                        label = country)) +
    geom_treemap(colour = "gray10") +
    geom_treemap_text(colour = "gray20", fontface = "italic",
                      min.size = 3, reflow = T) +
    geom_treemap_subgroup_border(colour = "white", size = 2) +
    geom_treemap_subgroup_text(alpha = 0.6, colour = "white") +
    viridis::scale_fill_viridis(direction = -1) +
    labs(title = "Country's share of global world cup TV Audience in 2010",
         caption ="Source: How To Break FIFA | FiveThirtyEight",
         fill = "GDP-weighted \naudience share \n(%)") +
    theme(plot.title = element_text(size = 20, face = "bold"))


ggsave("week_11/fifa_plot.png", fifa_plot, height = 7, width = 12, units = "in", dpi = 500)

