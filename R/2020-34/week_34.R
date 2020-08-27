library(tidyverse)
library(ggchicklet)
library(pals)
library(patchwork)


#### Loading the data
threats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/threats.csv')



##### Colors
n_threat_types <- length(threats$threat_type %>% unique())
pal <- ocean.curl(n_threat_types)
bgcolor <- "#fcf6e9"
  

##### Chicklet Plot
p1 <- threats %>% 
  filter(threatened == 1 ) %>%
  mutate(threat_type = fct_infreq(threat_type) %>% 
           fct_rev(),
         continent = fct_infreq(continent) %>% 
           fct_rev()) %>% 
  group_by(continent) %>%
  mutate(n_cont = n()) %>%
  count(threat_type, n_cont) %>%
  mutate(perc = n / n_cont) %>%
  ggplot(aes(x=continent, y=perc, fill=threat_type)) +
  geom_chicklet(width=0.8) + 
  scale_y_continuous(breaks = NULL, expand = c(0,0)) +
  coord_flip() +
  labs(x="",
       y="",
       fill="",
       title="Threat Distribution per Continent") +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = bgcolor),
        legend.key = element_rect(fill = bgcolor, size=1),
        legend.key.size = unit(0.5, "cm"),
        legend.key.width = unit(0.7, "cm"),
        legend.margin = margin(10, 10, 10, 10),
        legend.text = element_text(size = 10),
        panel.background = element_rect(fill=bgcolor),
        plot.background = element_rect(fill=bgcolor),
        panel.grid = element_blank(),
        plot.margin = margin(10, 40, 10, 10),
        plot.title = element_text(family = "Montserrat", size = 15, color="grey30"),
        plot.caption = element_text(size=8, color="grey50", margin = margin(15, 0, 0, 0) ),
        text = element_text(family = "Montserrat", color = "grey30", size=10),
        axis.text = element_text(family="Montserrat", color="grey30", size=12),
        axis.ticks = element_blank()) +
  guides(fill = guide_legend(nrow=4)) +
  scale_fill_manual(values = pal) +
  ggsave(here::here("plots/2020-34/continents.png") , width = 8, height = 6  ) 



####### Data Munging for Time Line Plot
df_t <- threats %>% 
  filter(threatened == 1 & !is.na(year_last_seen )) %>% 
  mutate(threat_type = fct_infreq(threat_type) %>% 
           fct_rev(),
         year_last_seen = factor(year_last_seen, 
                                 levels = c("Before 1900", "1900-1919", "1920-1939", "1940-1959", "1960-1979", "1980-1999", "2000-2020"), 
                                 labels = c("Before 1900", "1900", "1920", "1940", "1960", "1980", "After 2000") )) %>% 
  group_by(year_last_seen) %>%  
  mutate(n_time = n()) %>% 
  count(threat_type, n_time)  %>%
  mutate(perc = n / n_time)

threat_grid <- expand_grid(threat_type = unique(df_t$threat_type),
                           year_last_seen = unique(df_t$year_last_seen))

###### Time Line Plot
p2 <- df_t %>%
  full_join(threat_grid, by=c("year_last_seen", "threat_type")) %>%
  replace_na(list(n=0, perc=0)) %>% 
  ggplot(aes(x=as.numeric(year_last_seen), y=perc, fill=threat_type)) +
  geom_area(show.legend=F) +
  scale_x_continuous(name = "", breaks = 1:7, expand = c(0,0),
                     labels = str_wrap(c("Before 1900", "1900", "1920", 
                                         "1940", "1960", "1980", "After 2000"), width = 8)) +
  scale_fill_manual(values=pal) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(y="Threat Type in Percentage",
       title = "Threats over Time",
       fill = "") +
  theme(legend.position = "right",
        legend.background = element_rect(fill = bgcolor),
        legend.key = element_rect(fill = bgcolor, size=1),
        legend.key.size = unit(0.5, "cm"),
        legend.key.width = unit(0.7, "cm"),
        legend.margin = margin(10, 15, 10, 15),
        legend.text = element_text(size = 9),
        panel.background = element_rect(fill=bgcolor),
        plot.background = element_rect(fill=bgcolor),
        panel.grid = element_blank(),
        plot.margin = margin(10, 40, 10, 0),
        plot.title = element_text(family = "Montserrat", size = 15, color="grey30"),
        plot.caption = element_text(size=8, color="grey50", margin = margin(15, 0, 0, 0) ),
        text = element_text(family = "Montserrat", color = "grey30", size=10),
        axis.text = element_text(family="Montserrat", color="grey30", size=12),
        axis.title = element_text(face = "bold", vjust = -0.8),
        axis.ticks = element_blank()) +
  guides(fill = guide_legend(ncol=4)) +
  ggsave(here::here("plots/2020-34/threats_over_time.png") , width = 8, height = 6  ) 




######## Combine
(p2 + p1 + plot_layout(nrow=2, 
                       heights = c(2, 1.5),
                       guides = "keep") +
  plot_annotation(title=str_wrap("Plants in Danger", width = 10),
                  subtitle=str_wrap("Many species from isolated areas are facing extinction due to human activities. 
                                    These visualizations explore which activities are the most threatening ones and how 
                                    the different threats evolved over time and how they're distributed over the continents."),
                  caption = "Data: IUCN Red List\nVisualization: Corrie Bartelheimer @corrieaar",
                  theme = theme(plot.title = element_text(family = "Merriweather", size = 30, color="grey10"),)) &
  theme(plot.background = element_rect(fill=bgcolor, color=bgcolor),
        panel.grid = element_blank(),
        axis.title.y = element_text(face = "bold", vjust = -18),
        plot.margin = margin(30, 30, 10, 20),
        plot.subtitle = element_text(size = 13),
        plot.caption = element_text(size=10, color="grey50", margin = margin(15, 0, 0, 0) ),
        text = element_text(family = "Montserrat", color = "grey30", size=10)) ) +
  ggsave(here::here("plots/2020-34/composition.png" ), width = 10, height = 11  ) 


