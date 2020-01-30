setwd("J:\\Personalprojects\\TidyTuesday\\02-Sep-2019 Moores Law")

#load common libraries----
library(tidyverse)
library(magrittr)
library(ggplot2)
library(extrafont)
library(dplyr)
library(ggraph)
library(igraph)
library(RColorBrewer)
library(scico)
# install.packages("devtools")
#devtools::install_github("business-science/tibbletime")



# #Cleaning script*----
# library(tidyverse)
# library(rvest)
#
# url <- "https://en.wikipedia.org/wiki/Transistor_count"
#
# tables <- url %>%
#   read_html() %>%
#   html_table(fill = TRUE)
#
# df1 <- tables %>% chuck(1) %>%
#   janitor::clean_names() %>%
#   as_tibble()
#
# df1_clean <- df1 %>%
#   mutate(
#     # transistor_count = gsub("\\[[^\\]]*\\]", "", transistor_count, perl=TRUE),
#     transistor_count = str_remove(transistor_count, "\\[[^\\]]*\\]"),
#     transistor_count = str_remove(transistor_count, "[:punct:]+"),
#     transistor_count = str_remove(transistor_count, "\\[[^\\]]*\\]"),
#     transistor_count = str_remove(transistor_count, "[:punct:]+"),
#     transistor_count = str_remove(transistor_count, "[:punct:]+"),
#     transistor_count = str_extract(transistor_count, "[:digit:]+"),
#     date_of_introduction = str_sub(date_of_introduction, 1, 4),
#     process = str_remove(process, ","),
#     process = str_extract(process, "[:digit:]+"),
#     area = str_extract(area, "[:digit:]+")
#   ) %>%
#   mutate_at(.vars = vars(transistor_count:date_of_introduction, process:area), as.double)
#
#
# df1_clean %>%
#   mutate()
# df2 <- tables %>% chuck(2) %>%
#   janitor::clean_names() %>%
#   as_tibble()
#
# df2_clean <- df2 %>%
#   mutate(
#     # transistor_count = gsub("\\[[^\\]]*\\]", "", transistor_count, perl=TRUE),
#     transistor_count = str_remove(transistor_count, "\\[[^\\]]*\\]"),
#     transistor_count = str_remove(transistor_count, "[:punct:]+"),
#     transistor_count = str_remove(transistor_count, "\\[[^\\]]*\\]"),
#     transistor_count = str_remove(transistor_count, "[:punct:]+"),
#     transistor_count = str_remove(transistor_count, "[:punct:]+"),
#     transistor_count = str_extract(transistor_count, "[:digit:]+"),
#     process = str_remove(process, ","),
#     process = str_extract(process, "[:digit:]+"),
#     area = str_extract(area, "[:digit:]+")
#   ) %>%
#   mutate_at(.vars = vars(transistor_count:date_of_introduction, process:area), as.double)
#
# df3 <- tables %>% chuck(4) %>%
#   janitor::clean_names() %>%
#   as_tibble()
#
# df3
#
# df3_clean <- df3 %>%
#   mutate(
#     # transistor_count = gsub("\\[[^\\]]*\\]", "", transistor_count, perl=TRUE),
#     transistor_count = str_remove(transistor_count, "\\[[^\\]]*\\]"),
#     transistor_count = str_remove(transistor_count, "[:punct:]+"),
#     transistor_count = str_remove(transistor_count, "\\[[^\\]]*\\]"),
#     transistor_count = str_remove(transistor_count, "[:punct:]+"),
#     transistor_count = str_remove(transistor_count, "[:punct:]+"),
#     transistor_count = str_extract(transistor_count, "[:digit:]+"),
#     date_of_introduction = if_else(
#       str_length(date_of_introduction) >= 5,
#       str_sub(date_of_introduction, -4),
#       str_sub(date_of_introduction, 1, 4)),
#     process = str_remove(process, ","),
#     process = str_extract(process, "[:digit:]+"),
#     area = str_extract(area, "[:digit:]+"),
#     bit_units = case_when(
#       str_detect(capacity_bits, "bit") ~ "Bits",
#       str_detect(capacity_bits, "kb") ~ "kb",
#       str_detect(capacity_bits, "Mb") ~ "Mb",
#       str_detect(capacity_bits, "Gb") ~ "Gb",
#       TRUE ~ ""
#     )
#   ) %>%
#   mutate_at(.vars = vars(transistor_count:date_of_introduction, process:area), as.double) %>%
#   select(chip_name, capacity_bits, bit_units, everything()) %>%
#   mutate(capacity_bits = str_extract(capacity_bits, "[:digit:]+"))
#
# df3_clean
#
# write_csv(df1_clean, here::here("2019", "2019-09-03", "cpu.csv"))
# write_csv(df2_clean, here::here("2019", "2019-09-03", "gpu.csv"))
# write_csv(df3_clean, here::here("2019", "2019-09-03", "ram.csv"))


#Get the data-----
cpu <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/cpu.csv")

gpu <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/gpu.csv")

ram <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/ram.csv")


#Combine the data----
moore_df <-
  ram %>%
  dplyr::select(
    name = chip_name,
    transistors = transistor_count,
    date = date_of_introduction,
    manufacturer = manufacturer_s,
    process,
    area
  ) %>%
  mutate(type = "RAM") %>%
  bind_rows(cpu %>%
              rename(date = date_of_introduction,
                     transistors = transistor_count,
                     manufacturer = designer,
                     name = processor) %>%
              mutate(type = "CPU")) %>%
  bind_rows(gpu %>%
              dplyr::select(
                name = processor,
                transistors = transistor_count,
                date = date_of_introduction,
                manufacturer = manufacturer_s,
                process,
                area
              ) %>%
              mutate(type = "GPU")
  )

skimr::skim(moore_df)
names(moore_df)




#Pick Intel's data only----
intel_df <- moore_df %>% filter(type =="CPU",
                         manufacturer =="Intel") %>%
  mutate(log10_transistors = log10(transistors)) %>%
#   distinct(date, .keep_all = TRUE)
  group_by(date) %>%
  summarize(log10_transistors = max(log10_transistors, na.rm = TRUE))
  #

# Percent ---------------------------------------------------------
# roll over tibbles----
roll_percent <- tibbletime::rollify(.f = function(n) (n[2] - n[1])*100/n[1], 2)


#Calculate percentage change----
Intel_Intel <-
  intel_df %>%
  select(date, log10_transistors) %>% #name,
  #mutate(name = gsub("\\s*\\([^\\)]+\\)","",as.character(name))) %>%
  mutate(percent = roll_percent(log10_transistors)) %>%
  filter(complete.cases(.)) #,grepl("Intel",name)



#plot --------------------------------------------------------------------
# needed to center divergent palette
  lim = Intel_Intel$percent %>%
  range() %>%
  abs() %>%
  max()

#options(scipen=10000)

labels <- tibble(
  x1 = c(1978),
  x2 = c(1980),
  y1 = c(4.46 + 17/10),
  y2 = c(4.46 + 3 ),
  #type = rep("CPU", 3),
  text = c("**Intel 8086 (16-bit, 40-pin)**")
)

p <-
  Intel_Intel %>%
  mutate(yend = log10_transistors + (percent/10)) %>%
  ggplot(aes(x = date,
             y = log10_transistors)) +
  geom_segment(aes(yend = yend,
                   xend = ..x..,
                   colour = percent),
               size = 1,
               arrow = arrow(length = unit(1.2, "mm"),#1.2
                             type = "closed")) +

  geom_point(colour = "grey40", size = 2.3) +
  geom_text(aes(y = case_when(percent > 0 ~ yend + .1 ,#.12,
                              TRUE ~ yend - .1),#.12
                label = percent %>%
                  round() %>% paste0("%"),
                colour = percent),
            size = 3) +
  # geom_curve(data = labels,
  #            aes(x = x1, y = y1, xend = x2, yend = y2),
  #            size = 0.3,
  #            color = c("grey80"),
  #            linetype = "dotted",
  #            curvature = -0.3) +
  ggtext::geom_richtext(data = labels,
                aes(x = x1, y = y1, label = text),
                color = c("grey40"),
                family = "Montserrat",
                size = 3,
                fill = NA,
                label.color = NA,
                hjust = c(1)) +
  scale_colour_scico(palette = "lapaz",
                     direction = 1,
                     limits = c(-lim, lim),
                     guide = FALSE) +#+
  guides(colour = element_blank()) +
  labs(title = "Moore's Law:The exponential growth of microprocessors and it's impending end",
       subtitle = str_wrap("The near death of Moore's Law can be seen as the percentage change in number of transistors reduces over time"),
       y = "Log10_Transistors",
       x = "Year",
       caption = "Percentage change in number of transistors | Data Source: Wikipedia | Plot by @CarlvinJerry")





p <- p + #annotation_custom(rast, ymin = 9, ymax = 10, xmin = 1970) +
  theme(text = element_text(family = "Arial Narrow",
                            colour = "white",#grey40
                            size = 11),
        axis.title = element_text(size = 10),
        plot.title = element_text(colour = "white",
                                  face = "bold",
                                  size = 14),
        plot.subtitle = element_text(face = "italic",
                                     size = 10)) + #aspect.ratio = .4
  theme(panel.background =  element_rect(fill = "gray20", color = NA), #,size = 2, linetype = "solid"
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "gray20", color = NA),
      panel.grid.major.x = (element_line(color = "grey40",
                                                     size = 0.25,
                                                     linetype = "dotted")),
      panel.grid.major.y = element_line(color = "grey40",
                                        size = 0.1,
                                        linetype = "dotted"),
      # Change axis line
      axis.line = element_line(colour = "white"),
      axis.text = element_text(colour = "white"))
p
# img <- png::readPNG("./intel3.png")
# rast <- grid::rasterGrob(img, interpolate = T)
# annotation_custom(rast, ymin = 24000, ymax = 27000, xmin = 2)
 #Save plot
p

ggsave("02-Sep-2019_Dying Moore's Law.pdf ",
       width = 14, height = 5.8, device = cairo_pdf)
