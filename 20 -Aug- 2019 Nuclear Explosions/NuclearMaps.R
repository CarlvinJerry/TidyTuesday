
setwd("J:\\Personalprojects\\TidyTuesday\\Nuclear Explosions")

library(readxl)
library(ggplot2)
library(maps)
library(rgeos)
library(maptools)
library(ggmap)
library(geosphere)
library(dplyr)
library(gganimate)
#Get the data----
Nuclear_Data <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")

#Static map----
worldmap <- map_data("world")
wrld<-c(geom_polygon(aes(long,lat,group=group),
                     size = 0.1,
                     colour= "#363636", #070708",#4f4f4f"
                     fill="#363636", alpha=.9, data=worldmap)) #090D2A

global <- ggplot() +
  wrld +
  theme(panel.background =
          element_rect(fill='#0b0b0d',colour='#0b0b0d'), #00001C
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
 # coord_cartesian(xlim = c(-25,45), ylim = c(32,70))+ #magnifies map
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +



#Sample data----
#nuke_name <- unique(Nuclear_Data$name)
#namez <- names(Nuclear_Data)
dataSample <- Nuclear_Data %>% filter(!is.na(latitude),
                                      !is.na(longitude),
                                      !is.na(name),
                                      name %in% nuke_name) %>%
  select(name, longitude,latitude, region, Killotons = yield_upper, year) %>%
  top_n(30,Killotons)



map <- global +
  geom_point(aes(x = longitude, y = latitude, size = Killotons ),
             data = dataSample,
             colour = "red", alpha = .5) +
  geom_text(data= dataSample,aes(x=longitude+12, y=latitude, label=region),
            color = "orange", fontface = "italic", check_overlap = TRUE, size = 1.9) +
  annotate(geom = "text", x = 162., y = 9.3,
           label = "*Mike",
           fontface = "italic",
           color = "lightgreen",
           size = 3.5) +
  annotate(geom = "text", x = -116., y = 35.3,
           label = "*Handley",
           fontface = "italic",
           color = "bisque1",
           size = 3.5) +
  annotate(geom = "text", x = 165., y = 5.4,
           label = "*Romeo",
           fontface = "italic",
           color = "gold2",
           size = 3.5) +
  annotate(geom = "text", x = 172., y = 5.4,
           label = "Yankee",
           fontface = "italic",
           color = "peru",
           size = 3.5) +
  annotate(geom = "text", x = 175., y = 5.4,
           label = "Bravo",
           fontface = "italic",
           color = "lightblue2",
           size = 3.5) + transition_states(dataSample$year, transition_length = 7, state_length = 7, wrap = TRUE)
  # scale_colour_manual(values=name)+
  scale_size_continuous(range = c(1, 8),
                        breaks = c(100,1000,2000,3000,4000,5000, 6000,7000,8000,9000,10000)) +
  labs(size = 'Killotons')
animation::ani.options(interval = 3)
gganimate::animate(map)
#Save----
ggsave(plot = map, "map.jpg")

transition_states()
#Animating the map----
library(tibble)
library(lubridate)

ghost_points_ini <- tibble(
  created_at = as.Date('2011-09-01'),
  followers = 0, lon = 0, lat = 0)

ghost_points_fin <- tibble(
  created_at = seq(as.Date('2017-05-16'),
                   as.Date('2017-05-30'),
                   by = 'days'),
  followers = 0, lon = 0, lat = 0)

seq(Nuclear_Data$year)
