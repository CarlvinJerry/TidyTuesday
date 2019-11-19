setwd("J:\\Personalprojects\\TidyTuesday\\19-Nov-2019 BOTY")

library(dplyr)
library(reactable)
library(htmltools)


#Get data----
nz_bird <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-19/nz_bird.csv")

# Or read in with tidytuesdayR package (https://github.com/thebioengineer/tidytuesdayR)
# Either ISO-8601 date or year/week works!
# Install via devtools::install_github("thebioengineer/tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load("2019-11-19")
tuesdata <- tidytuesdayR::tt_load(2019, week = 47)

nz_bird <- tuesdata$nz_bird
#write.csv(nz_bird, "Birds.csv")
View(nz_bird)

#Create star rankings----
stars <- nz_bird %>% na.omit() %>% select(vote_rank, bird_breed) %>% mutate(vote_rank = replace(vote_rank, vote_rank %in% c("vote_1"),5)) %>%
  mutate(vote_rank = replace(vote_rank, vote_rank %in% c("vote_2"),4)) %>%
  mutate(vote_rank = replace(vote_rank, vote_rank %in% c("vote_3"),3)) %>%
  mutate(vote_rank = replace(vote_rank, vote_rank %in% c("vote_4"),2)) %>%
  mutate(vote_rank = replace(vote_rank, vote_rank %in% c("vote_5"),1))


avg_ratings <- stars %>% group_by(bird_breed) %>%  summarise( avg_rating = round(mean(as.numeric(vote_rank)),1)) %>%
  na.omit()

#Counts----
counts <- stars %>% group_by(bird_breed)  %>% tally(sort = TRUE)

#Top n----
top_ten <- merge(avg_ratings,counts, by = "bird_breed") %>% arrange(desc(n)) %>% top_n(15,n)
names(top_ten) <- c("Bird Breed", "Rating", "Votes" )

#reactable----
reactable(top_ten)

#Rating stars----
rating_stars <- function(rating, max_rating = 5) {
  star_icon <- function(empty = FALSE) {
    tagAppendAttributes(shiny::icon("star"),
                        style = paste("color:", if (empty) "#edf0f2" else "orange"),
                        "aria-hidden" = "true"
    )
  }
  rounded_rating <- floor(rating + 0.5)  # always round up
  stars <- lapply(seq_len(max_rating), function(i) {
    if (i <= rounded_rating) star_icon() else star_icon(empty = TRUE)
  })
  label <- sprintf("%s out of %s", rating, max_rating)
  div(title = label, "aria-label" = label, role = "img", stars)
}

ratings <- data.frame(
  Movie = c("Silent Serpent", "Nowhere to Hyde", "The Ape-Man Goes to Mars", "A Menace in Venice"),
  Rating = c(3.65, 2.35, 4.5, 1.4),
  Votes = c(115, 37, 60, 99)
)

#table----
reactable(top_ten, columns = list(
  Rating = colDef(cell = function(value) rating_stars(value))
))


#bar charts
# Render a bar chart with a label on the left
bar_chart <- function(label, width = "5%", height = "10px", fill = "#00bfc4", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "8px", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}


reactable(top_ten, columns = list(
  Votes = colDef(name = "Votes", align = "left", cell = function(value) {
    width <- paste0(value / max(top_ten$Votes) * 100, "%")
    bar_chart(value, width = width, background = "#e1e1e1")
  }),
  Rating = colDef(name = "Rating", align = "left",cell = function(value) rating_stars(value))
), borderless = TRUE , striped = TRUE, highlight = TRUE)



data <- MASS::Cars93[20:49, c("Make", "MPG.city", "MPG.highway")]

# Render a bar chart with a label on the left
bar_chart <- function(label, width = "10%", height = "16px", fill = "#00bfc4", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "8px", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}

reactable(data, columns = list(
  MPG.city = colDef(name = "MPG (city)", align = "left", cell = function(value) {
    width <- paste0(value / max(data$MPG.city) * 100, "%")
    bar_chart(value, width = width)
  }),
  MPG.highway = colDef(name = "MPG (highway)", align = "right", cell = function(value) {
    width <- paste0(value / max(data$MPG.highway) * 100, "%")
    bar_chart(value, width = width, fill = "#fc5185", background = "#e1e1e1")
  })
))



data <- data.frame(
  Animal = c("beaver", "cow", "wolf", "goat"),
  Body = c(1.35, 465, 36.33, 27.66),
  Brain = c(8.1, 423, 119.5, 115)
)

reactable(data, columns = list(
  Animal = colDef(cell = function(value) {
    image <- img(src = sprintf("images/%s.png", value), height = "10px")
    tagList(
      div(style = list(display = "inline-block", width = "45px"), image),
      value
    )
  }),
  Body = colDef(name = "Body (kg)"),
  Brain = colDef(name = "Brain (g)")
))

png::readPNG("images/cow.png")
sprintf("images/cow.png")

