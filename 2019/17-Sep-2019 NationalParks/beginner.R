# DATA ----------------------------------------------------------------------------------------
data_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")

# Have a quick look at the data
summary(data_raw)
head(data_raw)


# TIDY DATA -----------------------------------------------------------------------------------
data_year <- data_raw %>%
  # Clean column names, if necessary
  janitor::clean_names() %>%
  # Remove observations which have "Total" as the year value
  # These can be calculated later rather, if needed
  filter(year != "Total") %>%
  # Convert year to numerical value and add in date type
  mutate(year = as.numeric(year),
         date = lubridate::ymd(paste(year, 1, 1, sep = "-"))) %>%
  select(year, date, gnis_id:visitors)

# Create a df of the "Totals" for each park
data_total <- data_raw %>%
  janitor::clean_names() %>%
  filter(year == "Total")

summary(data_year)
str(data_year)

# Note: It seems that "unit_name" is more reliable than "park_name" as there are less NAs in "unit_name"


# VISUALISE -----------------------------------------------------------------------------------
# Let's recreate some of the visualisations in the article:
# https://fivethirtyeight.com/features/the-national-parks-have-never-been-more-popular/

# "Annual recreational visits to national parks since 1904" ----------------------------------
data_summary <- data_year %>%
  filter(unit_type == "National Park") %>%
  group_by(year) %>%
  summarise(total_visitors_mil = sum(visitors)/10^6)

# 1. Using base R ----------------------------------------------------------------------------
plot(data_summary$year, data_summary$total_visitors_mil, type = "l")

# 2. Using ggplot ----------------------------------------------------------------------------
# Base plot
g <- ggplot(data_summary, aes(x = year, y = total_visitors_mil)) +
  geom_line(colour = "#396D39")
g

# Styling and aesthetics
g <- g +
  geom_area(stat = "identity", fill = "#396D39", alpha = 0.4) +
  scale_x_continuous(breaks = seq(1910, 2010, 10)) +
  scale_y_continuous(breaks = seq(0, 80, 20),
                     labels = scales::unit_format(unit = "M")) +
  labs(title = "U.S. national parks have never been so popular",
       subtitle = "Annual recreational visits to national parks since 1904",
       x = "", y = "") +
  theme(plot.title = element_text(size = 16),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#D4D4D4"),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#F0F0F0"))
g

# 3. Using plotly -----------------------------------------------------------------------------
# Install "plotly" package if required - interactive, web-based graphs
if (!require(plotly)) {
  install.packages("plotly")
}
library(plotly)

# Interactive plot with hover text
p <- plot_ly(data_summary,
             x = ~year,
             y = ~total_visitors_mil,
             type = "scatter",
             mode = "lines",
             fill = "tozeroy",
             line = list(color = "#396D39"),
             fillcolor = list(color = "#396D39", alpha = 0.5),
             text = ~paste(year, "<br>", round(total_visitors_mil), "million visitors"),
             hoverinfo = "text") %>%
  layout(title = "U.S. national parks have never been so popular<br>Annual recreational visits to national parks since 1904",
         xaxis = list(title = ""),
         yaxis = list(title = "Millions of visitors"))

p

# "The most popular national parks over the past 20 years" -----------------------------------
# There are several great packages for visualising tabular data. My favorite is {DT}.
# Here I've explored {formattable} - a package I recently found out about
if (!require(formattable)) {
  install.packages("formattable")
}
library(formattable)

data_popular <- data_year %>%
  filter(unit_type == "National Park") %>%
  filter(year > 1996) %>%
  group_by(unit_name, state) %>%
  summarise(`Total visitors` = sum(visitors),
            `Average annual visitors` = round(mean(visitors))) %>%
  dplyr::rename(Park = unit_name,
                Location = state) %>%
  arrange(desc(`Average annual visitors`))

formattable(data_popular,
            align = c("l", "l", "l", "r"),
            list(`Average annual visitors` = color_bar(color = "#f2a080")))


# NEXT STEPS ----------------------------------------------------------------------------------

# Try replicate another one of the visualizations.
# Ask yourself a question and then design and implement a visualization to answer it, such as:
#   - How has the number of National Parks changed over the years?
#   - What is the trend in park visits for the last 10 years for the top 20 parks as of 2016?
#   - How does the ranking of most popular national parks per year change over time?