library(tidyverse)
library(lubridate)
library(ggtext)
#library(tidytext)
#library(ggraph)
#library(tidygraph)

tuesdata <- tidytuesdayR::tt_load('2020-08-25')

chopped <- tuesdata$chopped %>%
  mutate(air_date = lubridate::parse_date_time(air_date, "%B %d, %Y", locale="en_US.utf8")) %>%
  arrange(series_episode) 

bgcolor <- "#E8E5DA"

chopped %>%
  filter(!is.na(episode_rating ) ) %>%
  # seasons after 40 are not complete
  filter(season <= 40) %>%
  group_by(season) %>%
  mutate(    season_mean = mean(episode_rating, na.rm=T),
             even = season %% 2 == 1 ) %>%
  ggplot() +
  geom_segment(aes(x=series_episode,
                   xend=series_episode,
                   y=season_mean,
                   yend = episode_rating),
               col="grey20",
               alpha=0.3, size=0.3, show.legend = F) +
  geom_point(aes(x=series_episode, y=episode_rating, color=even),
             show.legend = F,  size=1.6) +
  geom_line(aes(x=series_episode, y=season_mean), col="grey50", size=0.4) +
  geom_line(aes(x = series_episode, y=season_mean, color=even, group=season), 
            size = 1.8, show.legend = F) +
  scale_color_manual(values = c("#456990", "#F96F5D")) +
  scale_x_continuous(name = "", breaks = NULL, expand = expansion(add = 10) ) +
  labs(title = "Ratings for <span style=color:#C5584A;'>*CHOPPED*</span> by Season",
       subtitle = "How did the ratings for the series change over the seasons?",
       y="Rating",
       caption = "Data: Kaggle & IMDb \nVisualization: @corrieaar") +
  theme(plot.background = element_rect(color=bgcolor, fill=bgcolor ),
        panel.background = element_rect(color=bgcolor, fill=bgcolor ),
        plot.margin = margin(50, 50, 20, 50),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#CBC5AA"),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(margin = margin(r=15)),
        text = element_text(color = "#666254", family = "Lato", size = 18),
        plot.title = element_markdown(color = "#444135", size=32, family = "Share"),
        plot.caption = element_text( size=14),
        axis.title.y = element_text(color = "#444135", size=20, margin=margin(r=25))) +
  ggsave(here::here("plots/2020-35/ratings_over_time.svg"), 
         device = "svg", scale = 1, dpi=300,
         width = 15, height = 10)


