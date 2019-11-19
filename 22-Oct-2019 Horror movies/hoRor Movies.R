# Set wd-----
setwd("J:/Personalprojects/TidyTuesday/22-Oct-2019 Horror movies")

# Get the data----
horror_movies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")
write.csv(horror_movies, "horror_movies.csv")

View(horror_movies)

# Load packages----
library(ggplot2)
library(lubridate)
library(tidyverse)
library(dplyr)
library(ggridges)
library(ggsci)
library(ggrepel)
library(extrafont)
# load fonts----
loadfonts(device = "win")

# Some cleaning----
clean_Horror <- horror_movies %>% select(everything()) %>% mutate(
  release_date = dmy(release_date),
  year = year(release_date),
  quarter = factor(quarter(release_date)),
  month = factor(month(release_date, label = TRUE), levels = month.abb),
  day = day(release_date)
) %>% drop_na()

Movie_counts <- clean_Horror %>% count(month)
Average_Ratings <- clean_Horror %>% summarise(AVG_Rating = mean(review_rating)) %>% pull(AVG_Rating)

# creating a subset data frame with only top rated for each group----
top_1 <-
  clean_Horror %>%
  group_by(quarter) %>%
  top_n(n = 1, wt = review_rating) %>%
  mutate(title = str_replace(title, " \\(.*\\)", ""))

# creating a subset data frame with only least  rated for each group----
bottom_1 <-
  clean_Horror %>%
  group_by(quarter) %>%
  top_n(n = -1, wt = review_rating) %>%
  mutate(title = str_replace(title, " \\(.*\\)", ""))


# Initiate plot-----
g <- ggplot(clean_Horror, aes(x = quarter, y = review_rating, color = quarter)) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 10), expand = c(0.005, 0.005)) +
  scale_color_uchicago()


## set seed to fix position of jittered points----
set.seed(120)

# Final plot----
p <- g + # annotation_custom(f, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + #Alternative background imge
  geom_hline(aes(yintercept = Average_Ratings), color = "forestgreen", size = 0.6, alpha = .7) +
  geom_jitter(aes(color = quarter),
    alpha = 0.35, size = 3,
    position = position_jitter(width = 0.3)
  ) +
  stat_summary(fun.y = mean, geom = "point", size = 5) +
  theme(legend.position = "none") +
  theme(
    plot.background = element_rect(fill = "black"),
    axis.text = element_text(family = "mono", colour = "gray50", size = 10),
    axis.title = element_text(family = "mono", colour = "gray50", size = 13),
    panel.grid.major = element_blank()
  ) +
  theme(
    line = element_blank(),
    plot.caption = element_text(family = "mono", size = 9, color = "gray50"),
    plot.title = element_text(family = "mono", size = 13, colour = "gray50"),
    plot.subtitle = element_text(family = "mono", size = 10, colour = "gray50"),
    panel.background = element_rect(fill = "transparent"),
    strip.background = element_rect(color = "gray20")
  ) +
  labs(
    x = "QUARTER", y = "REVIEW RATING", title = "FIRST, BE SCARED OF YOUR MOVIE BUDGET",
    subtitle = "Movies produced in the last quarter of the a year have a slight advantage in rating;Comparing Top and Worst rated movies - higher
               budget doesn't always pay off; US dominates the Horror Movie industry",
    caption = "Data: Georgios Karamanis|IMDB ... Plot: @PipeFunction| BeyondRawData Blog"
  ) + #
  annotate("text",
    x = 2.5, y = 3.9, family = "mono",
    size = 3, color = "forestgreen",
    label = glue::glue(" Overal average movie rating:", size = 3)
  ) +
  annotate("text",
    x = 2.4, y = 4.8, family = "mono",
    size = 3, color = "forestgreen",
    label = glue::glue(paste0(round(Average_Ratings, 2)))
  ) +

  geom_text_repel(top_1,
    mapping = aes(label = paste0(title, "\n(Rating: ", review_rating, ")", "\n(Country: ", release_country, year, ")", "\n(Budget: ", budget, ")")),
    family = "mono", color = "cyan4", size = 2.9,
    segment.size = 0.3,
    arrow = arrow(length = unit(0.01, "npc")),
    point.padding = NA, # unit(0.9, 'lines')
    box.padding = unit(0.7, "lines")
  ) +
  geom_text_repel(bottom_1,
    mapping = aes(label = paste0(title, "\n(Rating: ", review_rating, ")", "\n(Country: ", release_country, year, ")", "\n(Budget: ", budget, ")")),
    family = "mono", color = "firebrick2", size = 2.9,
    segment.size = 0.3,
    arrow = arrow(length = unit(0.01, "npc")),
    point.padding = NA, # unit(0.9, 'lines')
    box.padding = unit(0.8, "lines")
  )

# Add background image----
# img = jpeg::readJPEG("halloween-movie.jpg", native = TRUE)
# img = "https://github.com/CarlvinJerry/TidyTuesday/blob/master/22-Oct-2019%20Horror%20movies/halloween-movie.jpg?raw=true"
ggimage::ggbackground(p, img, alpha = 0.1)

# Save high quality----
ggsave("horrific4.png", dpi = 400)
ggsave("h0rror.svg")
##################################
