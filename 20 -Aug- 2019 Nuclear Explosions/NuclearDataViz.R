library(tidyverse)
library(magrittr)
library(ggplot2)
library(extrafont)

loadfonts(device = "win", quiet = TREU)

# Getting the data----
Nuclear_Data <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")

skimr::skim(Nuclear_Data)



# Nukes in decades----
# create decades...
# Can be chnaged directly by:

# byYr$year[byYr$year%in% c(1955:1964)] <- "1940-1949"
# byYr$year[byYr$year%in% c(1965:1974)] <- "1950-1959"

byDecade <- bind_rows(
  Nuclear_Data %>% filter(year %in% c(1945:1954)) %>% mutate(decade = "'45-'54") %>% group_by(decade) %>% count(decade, country),
  Nuclear_Data %>% filter(year %in% c(1955:1964)) %>% mutate(decade = "'55-'64") %>% group_by(decade) %>% count(decade, country),
  Nuclear_Data %>% filter(year %in% c(1965:1974)) %>% mutate(decade = "'65-'74") %>% group_by(decade) %>% count(decade, country),
  Nuclear_Data %>% filter(year %in% c(1975:1984)) %>% mutate(decade = "'75-'84") %>% group_by(decade) %>% count(decade, country),
  Nuclear_Data %>% filter(year %in% c(1985:1994)) %>% mutate(decade = "'85-'94") %>% group_by(decade) %>% count(decade, country),
  Nuclear_Data %>% filter(year %in% c(1995:2004)) %>% mutate(decade = "'95-'04") %>% group_by(decade) %>% count(decade, country)
)

sum(byDecade$n)
col_pallete <- c(
  "#a6cee3",
  "#1f78b4",
  "#b2df8a",
  "#33a02c",
  "#fb9a99",
  "#e31a1c",
  "#fdbf6f"
)

p1 <- byDecade %>%
  ggplot() +
  geom_linerange(aes(x = decade, ymin = 0, ymax = n, colour = country),
    position = position_dodge(width = 1)
  ) +
  geom_point(aes(x = decade, y = n, colour = country),
    position = position_dodge(width = 1)
  ) +
  ggtitle("Number of nukes per country grouped by decades") +
  # annotate("text", x = byDecade$decade[which(byDecade$decade == "'45-'54")], y = byDecade$n[which(byDecade$decade == "'45-'54")]*1,
  #          label = paste(byDecade$n[which(byDecade$decade == "'45-'54")] ,sep = "" ),
  #          color = "firebrick4",size = 3.3, angle = 0, fontface = "italic", hjust = -0.05)
  facet_grid(decade ~ ., scales = "free", space = "free") +
  coord_flip() +
  scale_color_manual(values = col_pallete) +
  theme(strip.text.y = element_blank()) +
  theme(plot.title = element_text(
    colour = "grey",
    family = "lato",
    size = 11,
    hjust = 0.5
  )) +
  theme(
    axis.text.x = element_text(angle = -90),
    axis.text = element_text(
      color = "white",
      family = "Lato"
    ),
    axis.title = element_text(
      color = "white",
      family = "Montserrat SemiBold"
    ),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_line(color = "gray")
  ) + # element_blank()) +
  theme(
    strip.background = element_rect(colour = NA, fill = NA),
    strip.text = element_text(color = "white", family = "Montserrat"),
    strip.placement = "outside"
  ) +
  theme(
    legend.background = element_rect(fill = NA),
    legend.box.background = element_rect(fill = NA, linetype = "blank"),
    legend.box.spacing = unit(1.5, "lines"),
    legend.key = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(color = "white", family = "Montserrat"),
    legend.text = element_text(color = "white", family = "Lato")
  ) +
  theme(
    panel.border = element_rect(
      linetype = "blank",
      fill = NA,
      color = "white"
    ),
    panel.background = element_blank(),
    panel.spacing = unit(1.5, "lines"),
    panel.grid = element_blank()
  ) +
  theme(plot.background = element_rect(fill = "gray20", color = NA))
#Save plot
ggsave("decadeNukes.png",plot = p1)







# Trends by nuclear types----
library(hrbrthemes)
library(gganimate)

Ntype <- Nuclear_Data %>%
  filter(type %in% c("ATMOSPH", "SHAFT", "TUNNEL", "UW", "AIRDROP", "TOWER", "BALLOON")) %>%
  group_by(type) %>%
  count(date_long)

# Anim plot
Ntype %>%
  ggplot(aes(x = year, y = n, group = type, color = type)) +
  geom_point(aes(group = seq_along(year))) +
  geom_line() +
  annotate(
    geom = "text", x = 1970, y = 1,
    label = "5 Aug '63 PTBT", color = "#69b3a2"
  ) +
  # geom_vline(xintercept = 1963, color = "red", linetype = "dashed")
  annotate(geom = "point", x = 1963, y = 1, size = 5, shape = 21, fill = "#69b3a2") +
  annotate(
    geom = "text", x = 1996, y = 15,
    label = "10 Sep '96 CTBT", color = "#69b3a2", angle = 90
  ) +
  # geom_vline(xintercept = 1963, color = "red", linetype = "dashed")
  annotate(geom = "point", x = 1996, y = 5, size = 5, shape = 21, fill = "#69b3a2") +
  # viridis::scale_color_viridis(discrete = TRUE) +
  ggtitle("Nuclear type trends since 1945") +
  # theme(strip.text.y = element_blank()) +
  scale_color_manual(values = col_pallete) +
  theme(plot.title = element_text(
    colour = "grey",
    family = "lato",
    size = 11,
    hjust = 0.5
  )) +
  theme(
    axis.text.x = element_text(angle = 0),
    axis.text = element_text(
      color = "white",
      family = "Lato"
    ),
    axis.title = element_text(
      color = "white",
      family = "Montserrat SemiBold"
    ),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_line(color = "gray") # element_blank()
  ) + # element_line(color = "gray") ) +#
  theme(
    strip.background = element_rect(colour = NA, fill = NA),
    strip.text = element_text(color = "white", family = "Montserrat"),
    strip.placement = "outside"
  ) +
  theme(
    legend.background = element_rect(fill = NA),
    legend.box.background = element_rect(fill = NA, linetype = "blank"),
    legend.box.spacing = unit(1.5, "lines"),
    legend.key = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(color = "white", family = "Montserrat"),
    legend.text = element_text(color = "white", family = "Lato")
  ) +
  theme(
    panel.border = element_rect(
      linetype = "blank",
      fill = NA,
      color = "white"
    ),
    panel.background = element_blank(),
    panel.spacing = unit(1.5, "lines"),
    panel.grid = element_blank()
  ) +
  theme(plot.background = element_rect(fill = "gray20", color = NA)) +
  # hrbrthemes::theme_ipsum_ps() +
  ylab("Number of nukes") +
  transition_reveal(year)


# Save gif:
anim_save("NuclearTrends.gif")

