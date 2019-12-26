
library(tidyverse)

honey_data <- read_csv("week_8/week_8.csv")

head(honey_data)

growth_prod <- honey_data %>% 
    select(state, totalprod, year) %>% 
    group_by(state) %>% 
    mutate(totalprod_lag = lag(totalprod),
           growth = (totalprod / totalprod_lag - 1) * 100) %>% 
    filter(!is.na(growth))

growth_prod$prodFactor <- cut(growth_prod$growth,
                              breaks = quantile(growth_prod$growth,
                                                probs = seq(0, 1, by= 0.2), na.rm = T),
                              include.lowest = T, 
                              ordered_result = T)

# HONEY PLOT 1
    
honey_plot <-
ggplot(growth_prod, aes(state, year, fill = prodFactor))+
    geom_tile(color = "white", size = 0.25) +
    labs(title = "U.S. Honey Production",
         subtitle = "Annual Growth Rate By State",
         caption = "Data: USDA-NASS") +
    scale_y_continuous(breaks = seq(1998, 2012, 1), expand = c(0, 0))+
    scale_fill_brewer(palette = "YlOrRd", direction = -1,
                      labels = c("-21.1%", "8.7", "3.2", "19.3", "134"))+
    guides(fill = guide_legend(title = "Growth Rate",
                               label.position = "bottom",
                               label.hjust = 1,
                               keywidth = 4, 
                               keyheight = .8))+
    ggthemes::theme_fivethirtyeight()+
    theme(panel.grid.major = element_blank(),
          axis.title = element_blank())

ggsave("honey_plot.png", honey_plot, height = 7, width = 13, units = "in", dpi = 500)


# HONEY PLOT 2

honey_plot_2 <- growth_prod %>% 
    select(state, year, growth) %>% 
    spread(state, growth) %>% 
    rename(Alabama = AL,
           Arizona = AZ,
           Arkanzas = AR,
           California = CA,
           Colorado = CO,
           Florida = FL,
           Georgia = GA,
           Hawaii = HI,
           Idaho = ID,
           Illinois = IL,
           Indiana = IN,
           Iowa = IA,
           Kansas = KS,
           Kentucky = KY,
           Louisiana = LA,
           Maine = ME,
           Maryland = MD,
           Michigan = MI,
           Minnesota = MN,
           Mississippi = MS,
           Missouri = MO,
           Montana = MT,
           Nebraska = NE,
           Nevada = NV,
           `New Jersey` = NJ,
           `New Mexico` = NM,
           `New York` = NY,
           `North Carolina` = NC,
           `North Dakota` = ND,
           Ohio = OH,
           Oklahoma = OK,
           Oregon = OR,
           Pennsylvania = PA,
           `South Carolina` = SC,
           `South Dakota` = SD,
           Tennessee = TN,
           Texas = TX,
           Utah = UT,
           Vermont  = VT,
           Virginia = VA,
           Washington = WA,
           `West Virginia` = WV,
           Wisconsin = WI,
           Wyoming = WY) %>% 
    gather(state, growth, -year) %>% 
    ggplot(aes(year, growth)) +
    geom_line(colour = "#FFD300", size = 1) +
    facet_wrap(~state) +
    labs(title = "U.S. Honey Production",
         subtitle = "Annual Growth Rate By State",
         caption = "Data: USDA-NASS")+
    ggthemes::theme_fivethirtyeight()

ggsave("honey_plot_2.png", honey_plot_2, height = 7, width = 13, units = "in", dpi = 500)




