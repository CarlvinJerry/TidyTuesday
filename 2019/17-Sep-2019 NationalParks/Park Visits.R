setwd("J:/Personalprojects/TidyTuesday/17-Sep-2019 NationalParks")

#load common libraries----
library(tidyverse)
library(magrittr)
library(ggplot2)
library(extrafont)
library(dplyr)
library(ggraph)
library(igraph)
library(RColorBrewer)
library(hrbrthemes)
library(viridis)

#data import----
park_visits <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")
state_pop <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/state_pop.csv")
gas_price <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/gas_price.csv")



# Clean data -----------------------------------------------------------------------------------
data_year <- park_visits %>%
  # Clean column names, if necessary
  janitor::clean_names() %>%
  # Remove observations which have "Total" as the year value
  # These can be calculated later rather, if needed
  filter(year != "Total") %>%
  # Convert year to numerical value and add in date type
  mutate(year = as.numeric(year),
         date = lubridate::ymd(paste(year, 1, 1, sep = "-"))) %>%
  select(year, date, gnis_id:visitors)

#get lustrums-----------------------------------------------------------------
lustrums <- data_year %>% mutate(lustrum = (year - year %% 10))

#count visitors every year
df <- lustrums %>%
  group_by(year) %>%
  filter(unit_type == "National Park") %>%
  summarise(visitors = sum(visitors)/10^6)




unit_groupings <- lustrums %>% mutate( unit_type = replace(unit_type, unit_type %in% c("National Park",
                                                                                     "National Park",
                                                                                     "National Recreation Area",
                                                                                     "National Preserve","Parkway",
                                                                                     "Park","National Parkway","National Reserve"), "No water body")) %>%
  mutate(unit_type = replace(unit_type, unit_type %in% c("National Seashore","National Scenic River","National Recreation River","Wild and Scenic River",
                                      "National Lakeshore","National River and Recreation Area","National River","National Scenic Riverway",
                                      "Scenic and Recreational River"), "With water body")) %>%
  mutate(unit_type = replace(unit_type, unit_type %in% c("National Monument","National Memorial"), "Monument & Memorial")) %>%
  mutate(unit_type = replace(unit_type, unit_type %in% c("National Battlefield","National Battlefield Park","National Battlefield Site",
                                                         "War in the Pacific","National Military Park","Other Designation"), "Military & War")) %>%
  mutate(unit_type = replace(unit_type, unit_type %in% c("Ecological and Historic Preserve","National Historical Park and Preserve",
                                                         "National Historical Park","National Historic Site",
                                                         "International Historic Site"), "Historical")) %>%
  group_by(year,unit_type)  %>%  summarise(visitors = sum(visitors)/10^6)

#
# #vv <- data_year %>% group_by(year) %>% mutate(visitors = summarise(sum(visitors)))
# # Plot
# p <- unit_groupings %>% #filter(unit_type %in% c( "Dry Park") ) %>%
#   ggplot( aes(x=year, y=visitors, fill=unit_type, text= visitors)) +
#   geom_area( ) +
#   scale_fill_viridis(discrete = TRUE) +
#   theme(legend.position="none") +
#   ggtitle("U.S park visits grouped by custom type") +
#   theme_ipsum() +
#   theme(legend.position="bottom")
#
# # Turn it interactive
# p <- ggplotly(p, tooltip="text")
# p
#
#
#
# # Plot
# n <-unit_groupings %>%
#   ggplot( aes(x=year, y=visitors, group=unit_type, color=unit_type)) +
#   geom_line() +
#   scale_color_viridis(discrete = TRUE) +
#
#   theme_ft_rc() +
#   theme(
#     legend.position="bottom",
#     plot.title = element_text(size=14)
#   ) +
#   ggtitle("A spaghetti chart of baby names popularity") +
#   theme_ipsum()
#
#
# ggplotly(n)



#Facet visits by type of park -----
tmp <- unit_groupings %>%
  mutate(unit_type2=unit_type)

p1 <- tmp %>%
  ggplot( aes(x=year, y=visitors)) +
  geom_line( data=tmp %>% dplyr::select(-unit_type), aes(group=unit_type2), color="grey", size=.5, alpha=0.25) +
  geom_line( aes(color=unit_type), color ="#ee3823", size=.65 )+ #ee3823 #69b3a2
  scale_color_viridis(discrete = TRUE) +
  theme_ft_rc() +
  #theme_modern_rc() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 12),
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 7),
    plot.subtitle = element_text(size = 10)
  ) +
  ggtitle("The growth of park visits in the U.S",
          subtitle = "Number of yearly park visits in Millions") +
  labs(caption = "Data Source: Wikipedia | Plot by @CarlvinJerry") +
  facet_wrap(~unit_type)

p1

