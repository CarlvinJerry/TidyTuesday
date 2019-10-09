#Set wd-----
setwd("J:/Personalprojects/TidyTuesday/22-Oct-2019 Horror movies")

#Get the data
horror_movies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")
write.csv(horror_movies,"horror_movies.csv")

View(horror_movies)

#Load packages
library(ggplot2)
library(lubridate)
library(tidyverse)
library(dplyr)
library(ggridges)
library(ggsci)

#Some cleaning
clean_Horror <- horror_movies %>% select(everything()) %>% mutate(release_date = dmy(release_date),
                            year = year(release_date),
                            quarter = factor(quarter(release_date)),
                            month = factor(month(release_date, label = TRUE), levels = month.abb),
                            day = day(release_date)) %>% drop_na()

Movie_counts <- clean_Horror %>% count(month)
Average_Ratings <- clean_Horror %>%summarise( AVG_Rating = mean(review_rating)) %>% pull(AVG_Rating)



#
# ggplot(clean_Horror, aes(x = quarter, y = review_rating)) +
#   geom_point(color = "firebrick", alpha = 0.5)

g <- ggplot(clean_Horror, aes(x = quarter, y = review_rating, color = quarter)) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 10), expand = c(0.005, 0.005)) +
  scale_color_uchicago()



g  +
  geom_hline(aes(yintercept = Average_Ratings), color = "brown", size = 0.6) +
  geom_jitter(aes(color = quarter), alpha = 0.25,size = 3,
                position = position_jitter(width = 0.3)) +
  stat_summary(fun.y = mean, geom = "point", size = 5)+
  theme(legend.position = "none")+
  theme(plot.background = element_rect(fill = "black"),
        axis.text  = element_text(colour = "gray"),
        axis.title = element_text(colour = "gray"),
       # axis.line.y = element_blank(),
        #axis.line.x = element_blank(),
        panel.grid.major = element_blank())
  ggimage::ggbackground(g,img2)

  img2 = "https://assets.bakker.com/ProductPics/560x676/10028-00-BAKI_20170109094316.jpg"
  ggbackground(p, img)


  +
    annotation_custom(rasterGrob(imgage,
                                 width = unit(1,"npc"),
                                 height = unit(1,"npc")),
                      -Inf, Inf, -Inf, Inf) +
  1
  img <-png::readPNG("thumb-1920-939120.png")
  f <- grid::rasterGrob(img, interpolate=TRUE,
                        width = unit(1,"npc"),
                        height = unit(1,"npc"))
  # +
  #   annotate("text", x = 2, y = 8.6, family = "Poppins",
  #            size = 2.7, color = "gray20",
  #            label = glue::glue(" students per teacher")) +
  #   annotate("text", x = 3.5, y = 10, family = "Poppins",
  #            size = 2.7, color = "gray20",
  #            label = "Continental average") +
  #   annotate("text", x = 1.7, y = 11, family = "Poppins",
  #            size = 2.7, color = "gray20",
  #            label = "Countries per continent") +
  #   annotate("text", x = 1.9, y = 64, family = "Poppins",
  #            size = 2.7, color = "gray20",
  #            label = "The Central African Republic has by far\nthe most students per teacher") +
  #   geom_curve(data = arrows, aes(x = x1, xend = x2,
  #                                 y = y1, yend = y2),
  #              arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
  #              color = "gray20", curvature = -0.3) +
  #   annotation_custom(ggplotGrob(map_regions),
  #                     xmin = 2.5, xmax = 7.5, ymin = 55, ymax = 85) +
  #   scale_y_continuous(limits = c(0, 90), expand = c(0.005, 0.005),
  #                      breaks = c(1, seq(20, 80, by = 20))) +
  #   scale_color_uchicago() +
  #   labs(x = NULL, y = "Student to teacher ratio",
  #        caption = 'Data: UNESCO Institute for Statistics') +
  #   theme_light(base_size = 15, base_family = "Poppins") +
  #   theme(legend.position = "none",
  #         axis.title = element_text(size = 12),
  #         axis.text.x = element_text(family = "Roboto Mono", size = 10),
  #         plot.caption = element_text(size = 9, color = "gray50"),
  #         panel.grid = element_blank())
  #
  #
  #
  # (g_text <- g +
  #     geom_segment(aes(x = quarter, xend = quarter,
  #                      y = Average_Ratings, yend = review_rating),
  #                  size = 0.8) +
  #     geom_hline(aes(yintercept = Average_Ratings), color = "gray70", size = 0.6) +
  #     stat_summary(fun.y = mean, geom = "point", size = 5) +
  #     geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
  #     annotate("text", x = 6.3, y = 35, family = "Poppins", size = 2.7, color = "gray20",
  #              label = glue::glue("Worldwide average:\n{round(Average_Ratings, 1)} students per teacher")) +
  #     annotate("text", x = 3.5, y = 10, family = "Poppins", size = 2.7, color = "gray20",
  #              label = "Continental average") +
  #     annotate("text", x = 1.7, y = 11, family = "Poppins", size = 2.7, color = "gray20",
  #              label = "Countries per continent") +
  #     annotate("text", x = 1.9, y = 64, family = "Poppins", size = 2.7, color = "gray20",
  #              label = "The Central African Republic has by far\nthe most students per teacher"))
  #