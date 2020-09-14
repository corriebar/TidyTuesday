library(tidyverse)
library(gender)        # estimate gender for speakers
library(ggtext)        # use markdown in title etc
library(waffle)        # make waffle plot
library(patchwork)     # combine multiple plots
library(ggnewscale)    # to add a geom_text layer to a waffle icon plot

tuesdata <- tidytuesdayR::tt_load('2020-09-08')

friends <- tuesdata$friends

# define colors
scale_fill_discrete <- function(...) {
  scale_fill_manual(..., values = c("female" = "#b75d69",
                                    "male" = "#774c60"), name =NULL )
}

scale_color_discrete <- function(...) {
  scale_color_manual(..., values = c("female" = "#b75d69",
                                    "male" = "#774c60"), name =NULL )
}

bgcolor <- "#F5EAE5"

# data processing
# computing the gender 
gen <- friends %>% 
  filter(!(speaker %in% c("Scene Directions", "#ALL#"))) %>%
  separate(speaker, into="first_name", sep=" |-", extra="drop") %>%
  distinct(first_name) %>%
  pull(first_name) %>%
  gender(method="ssa") 


# doing some further manual gendering
friends_g <- friends %>% 
  filter(!(speaker %in% c("Scene Directions", "#ALL#"))) %>%
  separate(speaker, into="first_name", sep=" |-,", remove=F, extra="drop") %>%
  left_join(gen, by=c("first_name"="name")) %>%
  mutate(gender1 = case_when(
           speaker %in% c("Rachel Green","Monica Geller", "Phoebe Buffay") ~ "female",
           speaker %in% c("Ross Geller", "Chandler Bing", "Joey Tribbiani") ~ "male",
           str_detect(tolower(speaker), "woman|female|ms\\.|mrs\\.|girlfriend|sister|aunt|grandma|grandmother|ess(?=\\b)|girl|mom|mother") ~ "female",
           str_detect(tolower(speaker), "male|(?<!wo)man(?=\\W)|mr\\.|boyfriend|waiter|brother|guy|boy|dad|father") ~ "male"),
         gender = if_else(is.na(gender), gender1, gender),
         words = str_count(text, "\\w"),
         nchar = nchar(text))  %>% 
  filter(!is.na(gender)) 

bar_plot <- 
  friends_g %>% 
  group_by(gender) %>% 
  summarise(nlines = n(),
            nwords = sum(words),
            nchars = sum(nchar)) %>% 
  pivot_longer(cols=c("nlines", "nwords", "nchars"), names_to="type", values_to="num") %>%
  filter(type == "nlines") %>%
  ggplot(aes(fill = gender, y=num, x=gender)) + 
  geom_bar(stat="identity", width = 0.7) +
    #facet_wrap(vars(type), nrow=3, scales="free") +
  geom_text(aes(label=paste(num, "Lines")), 
            vjust = -1, family = "Lato", size=6.5) +
  scale_x_discrete(label = c("female"="Spoken\nby Women", "male"="Spoken\nby Men")) +
  scale_y_continuous(limits = c(0, 31500), name = "", breaks =NULL) +
  labs(
    title = "Who's speaking?",
    subtitle="Even though the TV show *Friends* features in the main roles three men and three women, men still have more lines. <br>
    Men have about 1700 more lines than women which is equivalent to about **6 episodes** of lines.",
    x = ""
  ) +
  theme_minimal() +
  theme(plot.background = element_rect(color=bgcolor, fill=bgcolor),
        plot.margin = margin(l=10, r=10, b=10),
        panel.background = element_rect(color=bgcolor, fill=bgcolor),
        plot.subtitle = element_textbox(margin=margin(b=10), width=unit(15, "cm")),
        plot.caption = element_text(color="grey40", margin = margin(t=15)),
        plot.title = element_markdown(family = "Merriweather", size=36, margin = margin(b=10,t=15)),
        text = element_text(family = "Lato", size=18, color="#2C2126"),
        legend.position = "none") 

bar_plot +
  labs(caption = "Data: {friends} R package\nGraph: Corrie Bartelheimer @corrieaar") +
  ggsave(here::here("plots/2020-37/friends_plot.svg" ),
          width = 10, height = 13, dpi = 300)
 
   
# is the difference significant?
total_lines <- nrow(friends)
lines_wo_directions <- nrow(friends %>% filter(!(speaker %in% c("Scene Directions", "#ALL#"))))
total_gendered_lines <- nrow(friends_g)
ungendered_lines <- lines_wo_directions - total_gendered_lines
random_lines <- rbinom(1000000, size=lines_wo_directions, prob=0.5)
mean(random_lines >= nrow(friends_g %>% filter(gender == "male")))
mean(random_lines <= nrow(friends_g %>% filter(gender == "female")) )
mean(random_lines <= (nrow(friends_g %>% filter(gender == "female")) + ungendered_lines )  )

# computing the difference for multiple metrics
diff <- friends_g %>%
  group_by(gender) %>% 
  summarise(nlines = n(),
            nwords = sum(words),
            nchars = sum(nchar)) %>% 
  pivot_longer(cols=c("nlines", "nwords", "nchars"), names_to="type", values_to="num") %>%
  pivot_wider(names_from=gender, values_from=num) %>%
  mutate(diff = male - female)

# average num of lines per episode
avg <- friends %>%
  mutate(words = str_count(text, "\\w"),
         nchar = nchar(text)) %>%
  group_by(season, episode) %>%
  summarise(nlines = n(),
            nwords = sum(words),
            nchars = sum(nchar)) %>%
  pivot_longer(cols=c("nlines", "nwords", "nchars"), names_to="type", values_to="num") %>%
  group_by(type) %>%
  summarise(avg = mean(num)) %>%
  bind_rows( friends %>%
               group_by(season, episode) %>%
               summarise(nscenes = max(scene), .groups ="drop" ) %>%
               summarise(type = "nscenes", avg = mean(nscenes) ) )



# computing if a scene is mixed, women-only or men-only
scenes <- friends_g %>%
  group_by(season, episode, scene) %>%
  summarise(perc_female = mean(gender == "female")) %>%
  mutate(gender = case_when(
    perc_female == 1 ~ "female",
    perc_female == 0 ~ "male",
    TRUE ~ "mixed"
  )) %>% 
  ungroup() %>%
  count(gender, name="nscenes") %>%
  pivot_wider(names_from="gender", values_from="nscenes") %>%
  mutate(diff = male - female,
         type = "nscenes")


# waffle plot
ann_text <- tibble(facets = "female", x=75, y=2.9, label = "There are 143 more male-only scenes than female-only scenes.<br>
                                                                    This is equivalent to about **11 whole episodes**.")
ann_line <- tibble(facets="male", x=c(60, 60, 87, 87), y=c(5.9, 6.2, 6.2, 5.9))

waffle_plot <- diff %>%
  bind_rows(scenes) %>%
  left_join(avg) %>%
  mutate(num_episodes = diff / avg) %>%
  pivot_longer(cols=c("female", "male", "mixed")) %>%
  filter(type == "nscenes" & name != "mixed" ) %>%
  mutate(facets = name) %>%
  ggplot() +
  geom_step(data = ann_line, aes(x=x, y=y), size=0.8) +
  geom_textbox(data = ann_text, aes(x=x, y=y, label=label),
                fill=bgcolor,
               box.size=0,
               halign=0.5,
            size = 6.5,
            width = unit(8, "cm"),
            family="Lato",
            color="#2C2126")  +
  new_scale("label") +
  geom_pictogram(aes(label = name, values=value, fill=name, color=name), 
                 size=5, n_rows = 5) +
  scale_x_continuous(expand = c(0,0), name = NULL) +
  scale_y_continuous(name = NULL ) +
  scale_label_pictogram(  name = NULL,
    values = c( female = "female" , male = "male" )) +
  scale_color_manual(values = c("female" = "#b75d69",
                                "male" = "#70485a"), name =NULL ) +
  labs(subtitle="The majority of scenes in Friends has both men and women in it. 
       Excluding any scenes with both, we can compare the number of scenes that are only 
       <span style=font-family:'FontAwesome';color:#b75d69>\Uf182</span> or only 
       <span style=font-family:'FontAwesome';color:#70485a>\Uf183</span>.") +
  facet_grid(facets~.) +
  theme_enhance_waffle() +
  theme_minimal() +
  theme(plot.background = element_rect(color=bgcolor, fill=bgcolor),
        plot.margin = margin(l=20, r=20, b=15,t=15),
        panel.background = element_rect(color=bgcolor, fill=bgcolor),
        plot.subtitle = element_textbox(margin=margin(b=10, t=10), size=18, width=unit(18, "cm") ),
        plot.caption = element_text(color="grey40", margin = margin(t=15)),
        plot.title = element_text(family = "Merriweather", size=36, margin = margin(b=10,t=15)),
        text = element_text(family = "Lato", size=16, color="#2C2126"),
        strip.text = element_blank(),
        legend.position = "none",
        axis.text = element_blank()) 

waffle_plot +
  labs(caption = "Data: {friends} R package\nGraph: Corrie Bartelheimer @corrieaar",
       title="Who's in the Scene?") +
  ggsave(here::here("plots/2020-37/waffle.svg"), width=14, height = 7, dpi=300)
  

# composition plot
(bar_plot + waffle_plot +
  plot_layout(nrow=2, 
              heights = c(3, 1.2),
              guides = "keep") +
  plot_annotation(caption = "Data: {friends} R package\nGraph: Corrie Bartelheimer @corrieaar") &
    theme(plot.background = element_rect(fill=bgcolor, color=bgcolor),
          plot.margin = margin(l=15, r=15, b=15, t=15),
          panel.background = element_rect(color=bgcolor, fill=bgcolor),
          plot.subtitle = element_textbox(margin=margin(b=10), width=unit(15, "cm"), size=20),
          plot.caption = element_text(color="grey40", size=18,margin = margin(t=15)),
          plot.title = element_markdown(family = "Merriweather", size=40),
          text = element_text(family = "Lato", size=22, color="#2C2126"),
          legend.position = "none"))  +
  ggsave(here::here("plots/2020-37/composition.svg" ), width = 14, height = 18, dpi=300  ) 
  


