setwd("J:\\Personalprojects\\TidyTuesday\\27-Jan-2020 Trees Of San Francisco")

#devtools::install_github("thebioengineer/tidytuesdayR")
library(dplyr)
library(reactable)
library(htmltools)
library(leafpop)
library(leaflet)
library(mapview)
library(sf)
library(ggmap)

# Get the Data
sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')
#
# tuesdata <- tidytuesdayR::tt_load('2020-01-28')
# tuesdata <- tidytuesdayR::tt_load(2020, week = 5)
#
#
# sf_trees <- tuesdata$sf_trees



library(tidyverse)
library(osmdata)
library(here)
library(jkmisc)
library(nord)
library(colorspace)
library(glue)
library(tmaptools)

# NOT RUN {
SanFbb <- getbb("san francisco")

roads <- SanFbb %>%
  opq() %>%
  add_osm_feature("highway") %>%
  osmdata_sf()

water <- SanFbb %>%
  opq() %>%
  add_osm_feature("water") %>%
  osmdata_sf()


#our background map
mad_map <- get_map(getbb("san francisco"),maptype = "toner-background")

#final map
ggmap(mad_map)

+
  geom_sf(data=cinema$osm_points,
          inherit.aes =FALSE,
          colour="#238443",
          fill="#004529",
          alpha=.5,
          size=4,
          shape=21)+
  labs(x="",y="")



q <- getbb("Madrid")%>%
  opq()%>%
  add_osm_feature("amenity", "cinema")



osmdata(q)







# pnt <- data.frame(x = 10.88922, y = 49.71979)
# pnt <- st_as_sf(pnt, coords = c("x", "y"))
#
# # img <- "https://upload.wikimedia.org/wikipedia/commons/thumb/6/62/Mount_Eden.jpg/640px-Mount_Eden.jpg"
# #
# # mapview(pnt, map.types = "Esri.WorldImagery",
#         popup = popupImage(img, src = "remote"))
# , crs = 4326
#
# mapview(x, popup = leafpop:::popupIframe("https://www.youtube.com/embed/iApz08Bh53w?autoplay=1", width = 300, height = 225))
#
# mapview(breweries[])
#
# sfc = st_sfc(st_point(c(37.78286,-122.4092)))
# library(dplyr)
# x = sfc %>% st_set_crs(4326) %>% st_transform(3857)
#
#
# ggmap(SanFranCisco)
#
# ggmap(paris, extent = "normal")
# geocode("the white house")
#
#


library(tidyverse)
library(ggmap)

#Get the data
sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')

#Get the map background from OpenStreetMap
osm_sf <- get_map(location = c(left = -122.5501, bottom = 37.0000, right = -122.3367, top = 37.8116),
                  zoom = 14 , color = "bw")

#Plot the map
trees <- ggmap(osm_sf) +
  geom_point(data = sf_trees, aes( x = longitude, y = latitude),
             position=position_jitter(w=0.00025,h=0.00025),
             colour = "darkgreen",
             size = 0.01,
             alpha = 0.3) +
  stat_density2d(data = sf_trees, aes(x = longitude, y = latitude, fill = ..level..),
                 alpha = 0.15,
                 geom = "polygon",
                 n = 100) +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(fill = "Density") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = c(0.95, 0.60),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(1, 2, 10, 2))

#save plot
ggsave("trees.png", trees, height = 18, width = 32, units = "cm", dpi = "retina")

##################################################################


#
# SanfFranc <- get_map(location = c(left = -122.5501, bottom = 37.0000, right = -122.3367, top = 37.8116), zoom = 6, maptype = 'terrain')
# ggmap(SanfFranc) +
#   annotate('rect', xmin=-97.11, ymin=31.54, xmax=-97.12, ymax=31.55, col="red“, fill=“white”)+
#  annotate('text', x=-97.12, y=31.54, label = 'Statistical Sciences', colour = I(‘red'), size = 8)+
#  annotate('segment', x=-97.12, xend=-97.12, y=31.55, yend=31.55,
#  colour=I('red'), arrow = arrow(length=unit(0.3,"cm")), size = 1.5) +
#  labs(x = 'Longitude', y = 'Latitude') + ggtitle('Baylor University')
#
#

 ###########################_-----------------------


b <- leaflet() %>%
  setView(lng = -87.618994, lat = 41.875619, zoom = 12)
b %>%
  addTiles() %>%
  widgetframe::frameWidget()

  m <-leaflet() %>%
    setView(lng = -122.431297, lat = 37.773972, zoom = 14)
  m %>%
    addTiles() %>%
    widgetframe::frameWidget()

  content <- paste(sep = "<br/>",
                   "<b><a href='http://www.samurainoodle.com'>Samurai Noodle</a></b>",
                   "606 5th Ave. S",
                   "Seattle, WA 98138"
  )

  topTenTrees =data.frame( treeName = c("Sweet Bay", "Brazilian pepper tree", "Northern Rata", "6 Bluegum Eucalyptus Trees",
                                        "Moreton Bay Fig", "Centennial tree-bluegum", "Bunya Bunya",
                                        "New Zealand Christmas Tree", "Monterey Cypress" ),#"California Buckeye",
                           image =c(9, 10, 8, 7, 6, 5, 4, 3, 1), #2,
                           latitude =c(37.796640, 37.727700, 37.796570, 37.787100, 37.748060, 37.802000, 37.739810, 37.762850, 37.786282 ),#37.779890,
                           longitude = c(-122.400900, -122.393160, -122.426450, -122.426690, -122.420290, -122.460140, -122.468490, -122.452220, -222.489487 )) #-122.421910,



  leaflet(data = head(topTenTrees,8)) %>%
    addTiles() %>%
    addProviderTiles("NASAGIBS.ViirsEarthAtNight2012") %>%  #"NASAGIBS.ViirsEarthAtNight2012" Esri.WorldImagery Esri.WorldTopoMap NASAGIBS.ViirsEarthAtNight2012 Stamen.Toner
    addMarkers(label = ~treeName) %>% #,clusterOptions = markerClusterOptions()
    addPopups(-122.400900,37.796640,content, options = popupOptions(closeButton = FALSE)) %>%
    widgetframe::frameWidget()
