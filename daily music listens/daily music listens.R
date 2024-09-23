# -----------------
# Resources
# -----------------

# https://lastfm.ghan.nl/export/

# -----------------
# Packages
# -----------------

library(tidyverse)
library(data.table)
library(kableExtra)
library(anytime)
library(lubridate)
library(zoo)
library(plotly)
library(showtext)

# -----------------
# Data
# -----------------

last.fm.df <- read.csv("last-fm-extract.csv",
                         header = TRUE,
                         stringsAsFactors=FALSE)

# -----------------
# Duplicate Removal  
# -----------------

# remove duplicate tracks where scrobble has doubled (i.e. a play at th exact same time)
last.fm.df <- last.fm.df[!duplicated(last.fm.df[c("utc_time")]),]
# combine artist and song into single column 
last.fm.df <- last.fm.df %>% 
  mutate(artist.track = str_c(artist," - ",track))
# this will delete first instance of any two tracks occurring in succession
last.fm.df <- as.data.table(last.fm.df)[, .SD[1], by = rleid(artist.track)]

# -----------------
# Date Tidy 
# -----------------

# change to time format (will default to AEST)
last.fm.df$date.time <- as.POSIXct(last.fm.df$uts, 
                                     format = "%Y-%m-%d %H:%M")
# Separate out date & time
last.fm.df <- separate(last.fm.df, col = date.time, 
                       into = c("date", "time"), 
                       sep = " ")

# -----------------
# Date Metrics 
# -----------------

# Create week column 
last.fm.df$week <- isoweek(ymd(last.fm.df$date))
# Extract month from date
last.fm.df$month <- format(anydate(last.fm.df$date), "%m")
# Quarter from date
last.fm.df$quarter <- as.yearqtr(last.fm.df$date, format = "%Y-%m-%d")
# Year from date
last.fm.df$year <- year(last.fm.df$date)
# Merge "Year" Column with "Week" Column 
last.fm.df$yr_week <- paste(last.fm.df$year, last.fm.df$week, sep="-")
# Merge "Year" Column with "Month" Column 
last.fm.df$yr_month <- paste(last.fm.df$year, last.fm.df$month, sep="-")
# Duplicate time of song scrobble columsn to create 'hour' column 
last.fm.df$hour = last.fm.df$time
# Convert to time format
last.fm.df$hour <- as.POSIXct(last.fm.df$hour, format = "%H:%M:%S")
# Round to nearest hour
last.fm.df$hour = format(round(last.fm.df$hour, units = "hours"), format = "%H:%M")
# Day of the week
last.fm.df$dayofweek <- weekdays(as.Date(last.fm.df$date))
# Weekend/Weekday
last.fm.df$daytype[
  last.fm.df$dayofweek == "Saturday" |
    last.fm.df$dayofweek == "Sunday"] <-
  "Weekend"
last.fm.df$DayType[is.na(last.fm.df$dayofweek)] <- "Weekday"

# -----------------
# Prepare for visualisation 
# -----------------

# extract out the calendar 'day' from date 
last.fm.df$day <- substr(x = last.fm.df$date, 
                          start = 9, 
                          stop = 10)
# Aggregate total listens by date 
last.fm.calendar <- last.fm.df %>% 
  group_by(date) %>% 
  summarise(daily_listens = n())
# check df
str(last.fm.calendar)
# ensure in df format 
last.fm.calendar <- as.data.frame(last.fm.calendar)
# no missing data 
last.fm.calendar <- last.fm.calendar %>% drop_na(date)
# Crucial step 
  # on dates where I have not listened to music, they are absent from the df
  # as such we need to create "empty records" for these dates, so that they are
  # reflected in the visualisation 
last.fm.calendar <- 
  last.fm.calendar %>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(min(date), max(date), by="day"))
# Replace NA values with a 0, reflecting no listens on that date 
last.fm.calendar[is.na(last.fm.calendar)] <- 0
# Extract out day from date for visualisation 
last.fm.calendar$day <- substr(x = last.fm.calendar$date, 
                               start = 9, 
                               stop = 10)
# Create "Year-Month" column - y axis 
setDT(last.fm.calendar)[, yr_month := format(as.Date(date), "%Y-%m") ]
# Create year column - y axis labels 
last.fm.calendar$year <- year(last.fm.calendar$date)

# -----------------
# Visualisation  
# -----------------

# text colours 
tile_c_palette <- c(light_text = "#5d6b75", dark_text =  "#1C2226")
# fonts 
font_add_google("Patua One")
font_add_google("Inter")
# add to library
showtext_auto()

p <- 
  last.fm.calendar %>% 
  filter(year >= 2017) %>% 
  ggplot(aes(x = day,
             forcats::fct_rev(yr_month))) + # reverse order of y-axis
  geom_tile(aes(fill = daily_listens), 
            colour = "#f0eadf", 
            linewidth = 0.2, 
            na.rm = FALSE) + 
  scale_fill_stepsn(colors = c("#e8d9be", "#4ea599","#eac06c", "#e4905c", "#c9534e", "#912e2a"),
                    limits = c(0, 75), 
                    breaks = c(0, 1, 10, 25, 35, 45, 55, 65, 90)) +
  coord_equal(ratio = 1) +  
  labs(title = "Daily music listening history") +
  scale_y_discrete(
    breaks = c("2024-01", "2023-01", "2022-01", "2021-01", "2020-01", "2019-01", "2018-01", "2017-01", "2016-01"), # pick only first-month
    labels = c("2024", "2023", "2022", "2021", "2020", "2019", "2018", "2017", "2016"),
    expand = c(0,0.5)
    ) + 
  scale_x_discrete(
    breaks = c("01", "05", "10", "15", "20", "25", "31"), 
    labels = c("1", "5", "10", "15", "20", "25", "31")
    ) +
  theme(panel.background = element_rect(fill = "#f0eadf"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16, vjust = 0.2, colour = tile_c_palette["light_text"], face = "bold", family = "Inter"),
        axis.text.x = element_text(size = 16, vjust = 0.2, colour = tile_c_palette["light_text"], family = "Inter", face = "bold"),
        plot.title = element_text(colour = "#3b474f", hjust = 0.5, size = 20, face = "bold", family = "Inter"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "top", # position to right of plot
        legend.direction = "horizontal", # vertical orientation
        legend.margin = margin(0,0,0,0,"cm"), # move a little away from plot, to the right
        legend.text = element_text(colour = tile_c_palette["light_text"], size = 16, family = "Inter"),
        legend.key.height = grid::unit(0.6,"cm"),
        legend.key.width = grid::unit(2.0,"cm"),
        legend.box.just = "centre",
        legend.title = element_blank(),
        plot.background = element_rect(fill = "#f0eadf"),
        legend.background = element_rect(fill = "#f0eadf"),
        panel.border = element_blank(),
        plot.margin = margin(0.7,0.7,0.7,0.7,"cm")
        )
p

# -----------------
# Save & View   
# -----------------

fname <- "/data-visualisation/daily music listens/plot/lastfm.png"
ggsave(fname,  p, width = 10, height = 20, bg = "#f0eadf", units = "in", dpi = 96)
viewer <- getOption("viewer")
viewer(fname)