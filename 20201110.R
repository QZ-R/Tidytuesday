# Get the Data

# Read in with tidytuesdayR package
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Or read in the data manually

mobile <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv')
landline <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/landline.csv')

library(tidytuesdayR)
library(tidyverse)
library(viridis)
library(rnaturalearth)

tt <- tt_load("2020-11-10")
tt
mobile <- tt$mobile
landline <- tt$landline

colnames(mobile) # to show what kind of variables in the
hist(mobile$mobile_subs)

ggplot(mobile) +
  geom_point(aes(x=gdp_per_cap, y = mobile_subs, color = year)) +
  scale_x_log10() +
  scale_color_viridis_c()

colnames(landline)

ggplot(landline) +
  geom_point(aes(x=gdp_per_cap, y = landline_subs, color = year)) +
  scale_x_log10() +
  scale_color_viridis_c()


## i realize here that landline data looks kind of boring (very stable trend over time) let's stick with mobile data
# Which countries in the past years have really amped up in mobile phone subscriptions?


#plot trends across time for continent
ggplot(mobile) +
  geom_point(aes(x=year, y = mobile_subs, color = continent)) +
  scale_y_log10() +
  scale_color_viridis_d() +
  theme(legend.position = "none")


mobile2 <- na.omit(mobile) #remove NAs for models

nested0 <- mobile2 %>%
  group_by(code) %>%
  nest()

country_model <- function(df) {
  lm(log(mobile_subs+1) ~ year, data = df)
  }

#of course I'm assuming models are fine but residuals should be evaluated

nested <- nested0 %>%
  mutate(model = map(data, country_model))

nested %>%
  pluck("model", 1) %>%
  summary()

worldmap_df <- nested %>%
  mutate(glance = map(model, broom::tidy)) %>%
  unnest(glance) %>%
  filter(term == "year")

#let's get global maps so we can plot this data
#world map
world <- ne_countries(returnclass = "sf") # get sf-object of world map using the rnaturalearth package
map_data <- left_join(world, worldmap_df, by = c("adm0_a3" = "code"))

ggplot() +
  geom_sf(data = map_data, aes(fill = estimate, color = estimate)) +
  scale_fill_viridis_c(option = "D") +
  scale_color_viridis_c(option = "D")+
  labs(color = "Increase in \nmobile phone \nsubscriptions") +
  labs(color = "Increase in \nmobile phone \nsubscriptions") +
  guides(fill = FALSE, size = FALSE) +
  theme_light()
  # theme_classic() +
  # theme(
  #   panel.grid.major = element_blank(),
  #   panel.grid.minor = element_blank(),
  #   panel.background = element_rect(fill = "transparent",color = NA),
  #   plot.background = element_rect(fill = "black",color = NA),
  #   rect = element_rect(fill = "transparent"), # all rectangles
  #   legend.title = element_text(color="white", size = 12),
  #   legend.text = element_text(color="white", size = 8)
  # )
