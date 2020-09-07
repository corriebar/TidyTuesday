library(tidyverse)
library(janitor)
library(waffle)
library(ggtext)

tuesdata <- tidytuesdayR::tt_load(2020,36)

crop_yields <- tuesdata$key_crop_yields
bgcolor <- "#EFEFE1" 
  
# color palette from package nord
algoma_forest = c("#4B4B4B","#967D4B", "#AFAF7D", "#C89632", "#647D64","#96AFAF","#7D96AF")

scale_fill_discrete <- function(...) {
  scale_fill_manual(..., values = c("Cereals" = "#AFAF7D",
                                      "Roots & Tubers" = "#967D4B",
                                      "Legumes" = "#647D64",
                                      "Bananas" = "#C89632",
                                      "Cocoa beans" = "#4B4B4B"), name =NULL )
}


crops <- crop_yields %>%
  pivot_longer(cols = 4:last_col(),
               names_to = "crop",
               values_to = "crop_production") %>%
  mutate(crop = str_remove_all(crop, " \\(tonnes per hectare\\)")) %>%
  set_names(nm = names(.) %>% tolower()) %>%
  filter(entity %in% c("Africa", "Americas", "Asia", "Europe", "Oceania"))



crops %>%
  filter(year %in% c(1961, 2018 ) ) %>%
  mutate(crop_cat = fct_collapse(crop,
                                 Cereals = c("Wheat", "Rice", "Maize", "Barley"),
                                 `Roots & Tubers` = c("Potatoes", "Cassava"),
                                 Legumes = c("Soybeans", "Beans", "Peas")) ) %>%
  group_by(crop_cat, year) %>%
  summarise(tonnes = ceiling(mean(crop_production, na.rm=T) )  ) %>% 
  ggplot(aes(fill=crop_cat,  values = tonnes)) +
    geom_waffle(color=bgcolor, size = 1.2,  n_rows = 5) +
  labs( title = "The Green Revolution",
  subtitle = str_wrap("Worldwide, on average the crop productivity has   
  nearly doubled. Planting the same area of crop today
    gives almost twice as many tonnes per hectare
    than it would have in the 60ies.  
    The increase in yield varies by crop category.         
    ◼ ≃  t ha⁻¹", width=56) ,
  caption = "Data: Our World in Data | Graphic: @corrieaar") +
  facet_grid( rows = vars(year), switch = "y") +
  guides( fill = guide_legend(override.aes = list(size = 5, shape = 2)) ) +
  theme_enhance_waffle() +
  theme_void() +
  theme(plot.background = element_rect(color=bgcolor, fill=bgcolor),
        panel.background = element_rect(color=bgcolor, fill=bgcolor),
        panel.border = element_blank(),
        plot.margin = margin(t=10, l=30,r=30,b=10),
        panel.grid = element_blank(),
        panel.spacing = unit(1, "cm"),
        plot.title = element_text(size = 34, 
                                  family = "Playfair Display",
                                  margin = margin(t=10)),
        plot.subtitle = element_text(margin = margin(b=20, t=15)),
        legend.position = "bottom",
        legend.text = element_text(size = 24),
        strip.text.y.left = element_text(angle = 0, face = "bold"),
        plot.caption = element_text(margin = margin(t=20)),
        text = element_text(family = "Lato Light", size=20, color="#243324")) +
  ggsave(here::here("plots/2020-36/waffle_plot.svg" ),
         width = 15, height = 15, dpi = 300)

