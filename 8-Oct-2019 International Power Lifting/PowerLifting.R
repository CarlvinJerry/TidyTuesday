#Set wd-----
setwd("J:/Personalprojects/TidyTuesday/8-Oct-2019 International Power Lifting")

#Cleaning script------
library(tidyverse)

df <- read_csv(here::here("openpowerlifting-2019-09-20", "openpowerlifting-2019-09-20.csv"))

df_clean <- df %>%
  janitor::clean_names()

df_clean %>%
  group_by(federation) %>%
  count(sort = TRUE)

size_df <- df_clean %>%
  select(name:weight_class_kg, starts_with("best"), place, date, federation, meet_name)  %>%
  filter(!is.na(date)) %>%
  filter(federation == "IPF") %>%
  object.size()

ipf_data <- df_clean %>%
  select(name:weight_class_kg, starts_with("best"), place, date, federation, meet_name)  %>%
  filter(!is.na(date)) %>%
  filter(federation == "IPF")

print(size_df, units = "MB")

ipf_data %>%
  write_csv(here::here("2019", "2019-10-08","ipf_lifts.csv"))


#Fetch data----
ipf_lifts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-08/ipf_lifts.csv")
View(ipf_lifts)
#Packages----
library(tidyverse)
library(dplyr)
library(ggplot2)

#Filter data, remove NAs, create new columns(year)----
lifts <- ipf_lifts %>% select(sex, event, equipment,age,bodyweight_kg,best3squat_kg,best3bench_kg,best3deadlift_kg,
                              place,date) %>% na.omit() #%>% mutate(year = as.numeric(substring(as.character(date),1,4)))

plot(lifts %>% group_by(year) %>% summarise(max(best3squat_kg))) #Try-------

# best3squat_kg, best3bench_kg, best3deadlift_kg,


# Libraries
library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(networkD3)


# Long format counts
data_long <- lifts %>% select(age, sex, event,
                              equipment, bodyweight_kg ) %>% mutate(age_group = replace(age, age <= 30, "30 Yrs & Below" )) %>%
                                                                      mutate(age_group = replace(age_group, age > 30 & age <= 60, "30 to 60 Yrs" )) %>%
  mutate(age_group = replace(age_group, age > 60 & age <= 90, "60 to 90 Yrs" )) %>%
  mutate(weight_group = replace(bodyweight_kg, bodyweight_kg <= mean(bodyweight_kg), "Below 80 Kgs" )) %>%
  mutate(weight_group = replace(weight_group, bodyweight_kg > mean(bodyweight_kg), "Above  80 Kgs" )) %>%
  group_by(sex) %>% count(age_group, weight_group, equipment, event)




# A connection data frame is a list of flows with intensity for each flow
links <- data.frame(
  source = c(data_long$sex,data_long$age_group,data_long$weight_group,data_long$equipment),
  target = c(data_long$age_group,data_long$weight_group,data_long$equipment,data_long$event),
  value = data_long$n
)


# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name = c(as.character(links$source), as.character(links$target)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource = match(links$source, nodes$name)-1
links$IDtarget = match(links$target, nodes$name)-1

# prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Add a 'group' column to each connection:
#links$group <- as.factor(c(1:112))

# Add a 'group' column to each node. Here I decide to put all of them in the same group to make them grey
nodes$group <- as.factor(c(1:11))

# Give a color for each group:
my_color <- 'd3.scaleOrdinal() .domain(["type_a", "type_b", "my_unique_group"]) .range(["#69b3a2", "steelblue", "grey"])'

ColourScal2 ='d3.scaleOrdinal() .range([ "#2e2829","#993c40","#094781","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'
#colourScale=ColourScal,
color = c( '#2e2829','#993c40','#438D80','#307D7E','#3EA99F', '#5e5a80', '#4b0082',  '#e7a4b6', '#c16587','#3d3b72', '#0e2973'),

# Make the Network
sankey <- sankeyNetwork(Links = links, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name",
              sinksRight=TRUE, colourScale = ColourScal2, NodeGroup="group",  nodeWidth=10, fontSize=15, nodePadding=90) #LinkGroup="group",   NodeGroup="group",
# library(RColorBrewer)
# NodeGroup="group",
# LinkGroup="group",

#PLOTLY::::---------
#Create canvas----
library(plotly)

p <- plot_ly(
  type = "sankey",
  domain = list(
    x =  c(0,1),
    y =  c(0,1)
  ),
  orientation = "h",
  valueformat = ".0f",
  valuesuffix = "TWh",

  #Add nodes----
  node = list(
    label = nodes$name,
    color = c( '#2e2829','#993c40','#438D80','#307D7E','#3EA99F', '#5e5a80', '#4b0082',  '#e7a4b6', '#c16587','#3d3b72', '#0e2973'),

    pad = 15,
    thickness = 15,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  link = list(
    source = links$IDsource,
    target = links$IDtarget,
    value =  links$value

  )
)%>%
  layout(
    title = "Power Lifters' distribution across Gender,Age, Body Weight and Equipment Used for SBD",
    font = list(
      size = 11,
      colo = "Yellow"
    ),
    xaxis = list(showgrid = F, zeroline = F),
    yaxis = list(showgrid = F, zeroline = F),

    plot_bgcolor = 'black'
    #paper_bgcolor = 'black'
  )
p

#Add nodes-----
