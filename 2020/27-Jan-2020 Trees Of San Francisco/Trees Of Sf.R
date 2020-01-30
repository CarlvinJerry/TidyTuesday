setwd("J:\\Personalprojects\\TidyTuesday\\27-Jan-2020 Trees Of San Francisco")

library(dplyr)
library(htmltools)
library(leafpop)
library(leaflet)
library(mapview)
library(sf)
library(ggmap)
library(htmlwidgets)
library(htmltools)

# Get the Data
sf_trees <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv")

# Df of top ten trees  save for two pop ups----
topTenTrees <- data.frame(
  treeName = c(
    "Sweet Bay", "Northern Rata", "6 Bluegum Eucalyptus Trees",
    "Moreton Bay Fig", "Centennial tree-bluegum", "Bunya Bunya",
    "New Zealand Christmas Tree"
  ), # "California Buckeye", "Brazilian pepper tree", "Monterey Cypress"
  image = c(9, 8, 7, 6, 5, 3), # 2,10, , 1 4,
  latitude = c(37.796640, 37.796570, 37.787100, 37.748060, 37.802000,  37.762850), # 37.779890, 37.727700,, 37.786282 37.739810,
  longitude = c(-122.400900, -122.426450, -122.426690, -122.420290, -122.460140, -122.452220) #-122.468490,
) #-122.421910, -122.393160,, -222.489487


# Create pop up content for labels----
popup10 <- paste0("<b><a href='http://www.sftrees.com/new-page-1'>#10  Brazilian Pepper Tree</a></b><br/>3rd Street & Yosemite, the Bay View")
popup1 <- paste0("<b><a href='http://www.sftrees.com/new-page-1'>#1  Monterey Cypress</a></b><br/>McLaren Lodge")
popup3 <- paste0(c(
  "#9  Sweet Bay:555 Battery Street",
  "#8 Northern Rata: Vallejo Street",
  "#7 Bluegum Eucalyptus Trees: 1661 Octavia Street",
  "#6 Moreton Bay Fig: Valencia Street",
  "#5 Centennial tree-bluegum: Presidio Main Post",
  #"#4 Bunya Bunya: 201 Vicente, West Portal",
  "#3 New Zealand Christmas Tree: Stanyan/17Th"
))
popup4 <- paste0("<b><a href='http://www.sftrees.com/new-page-1'>#4 Bunya Bunya Tree</a></b><br/>201 Vicente, West portal")

# Leaflet----
tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title {
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px;
    padding-right: 10px;
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 14px;
  }
"))

title <- tags$div(
  tag.map.title, HTML("SAN FRANCISCO’S TOP 10 “LANDMARK TREES”")
)



map_leaflet <- leaflet(data = topTenTrees) %>%
  addTiles() %>%
  addProviderTiles("Esri.WorldImagery") %>% #Esri.WorldImagery Esri.WorldTopoMap NASAGIBS.ViirsEarthAtNight2012 Stamen.Toner
  addMarkers(label = htmlEscape(popup3), labelOptions = labelOptions(noHide = T, direction = "right")) %>% # ,clusterOptions = markerClusterOptions()
  addPopups(-122.454360, 37.772890, popup1, options = popupOptions(closeButton = FALSE)) %>%
  addPopups(-122.393160, 37.727700, popup10, options = popupOptions(closeButton = FALSE, maxWidth = 100)) %>%
  addPopups(-122.468490, 37.739810, popup4, options = popupOptions(closeButton = FALSE, maxWidth = 100)) %>%
  #addPopups(-122.468490, 37.739810,popupImage(img, src = "remote")) %>%
  addControl(title, position = "topleft", className = "map-title")
# %>%
#   widgetframe::frameWidget()

mapshot(map_leaflet, file = "SanfranciscoTrees.svg")
mapshot(map_leaflet, url = paste0(getwd(), "/map.html"))
