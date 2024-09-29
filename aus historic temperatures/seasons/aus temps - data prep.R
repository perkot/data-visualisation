
# ----------------------------------
# DEPENDENCIES 
# ----------------------------------

library(tidyverse)
library(forcats)

# -----------------
# Data
# -----------------

# http://www.bom.gov.au/climate/data/acorn-sat/
tbl <- read.csv("ACORN-SAT-Clean.csv")

# -----------------
# Tidy up station names 
# -----------------

# lower case column names 
colnames(tbl) <- tolower(colnames(tbl))

# Parse out first two-words of string
tbl$station <- gsub("(\\w+\\s+\\w+).*", "\\1", 
                    tbl$site.name)

# Get unique station names 
unique_station <- unique(tbl$station)
unique_station

# Remove these specific words 
tbl <- tbl %>% mutate_all(~gsub("AIRPORT", "", .))
tbl <- tbl %>% mutate_all(~gsub("AERO", "", .))
tbl <- tbl %>% mutate_all(~gsub("RAAF", "", .))

# Change these station names 
tbl$station[tbl$station == "BATHURST AGRICULTURAL"] <- "BATHURST"
tbl$station[tbl$station == "LARAPUNA (EDDYSTONE POINT"] <- "LARAPUNA"
tbl$station[tbl$station == "INVERELL (RAGLAN ST"] <- "INVERELL"
tbl$station[tbl$station == "KALGOORLIE-BOULDER "] <- "KALGOORLIE"
tbl$station[tbl$station == "MELBOURNE (OLYMPIC PARK"] <- "MELBOURNE"
tbl$station[tbl$station == "SYDNEY (OBSERVATORY HILL"] <- "SYDNEY"
tbl$station[tbl$station == "RUTHERGLEN RESEARCH"] <- "RUTHERGLEN"
tbl$station[tbl$station == "HOBART (ELLERSLIE ROAD"] <- "HOBART"
tbl$station[tbl$station == "CAMOOWEAL TOWNSHIP"] <- "CAMOOWEAL"
tbl$station[tbl$station == "CUNDERDIN AIRFIELD"] <- "CUNDERDIN"
tbl$station[tbl$station == "GROVE (RESEARCH station"] <- "GROVE"
tbl$station[tbl$station == "GILES METEOROLOGICAL"] <- "GILES"

# -----------------
# yearly aggregation
# -----------------

# convert to numeric 
tbl$maximum.temperature..degC. <- as.numeric(tbl$maximum.temperature..degc.)
tbl$minimum.temperature..degC. <- as.numeric(tbl$minimum.temperature..degc.)

# Aggregate 
ya <- tbl %>% 
  group_by(year, station) %>% 
  summarise(avg_max = round(mean(maximum.temperature..degC., na.rm=T,),2),
            avg_min = round(mean(minimum.temperature..degC., na.rm=T),2)) 

# Ensure dataframe
ya <- as.data.frame(ya)

# Convert 'year' to factor
ya$year <- as.factor(ya$year)

# Convert average temperatures to numeric 
ya$avg_max <- as.numeric(ya$avg_max)
ya$avg_min <- as.numeric(ya$avg_min)

# -----------------
# Create a grid 
# -----------------

# Where stations do not have temperature data, create empty records 
# https://stackoverflow.com/questions/9996452/r-find-and-add-missing-non-existing-rows-in-time-related-data-frame

grid <- expand.grid(year = unique(ya$year),
                    station = unique(ya$station))
ya <- 
  merge(grid,
        ya,
        all = TRUE)

# -------------------
# Order avg temps 
# -------------------

# Maximum Average
max <- ya %>% 
  group_by(station) %>% 
  summarise(max_average = round(mean(avg_max, na.rm = TRUE),2)) 
ya <- left_join(ya, max, by = "station") 

# Minimum Average
min <- ya %>% 
  group_by(station) %>% 
  summarise(min_average = round(mean(avg_min, na.rm = TRUE),2)) 
ya <- left_join(ya, min, by = "station") 

rm(max, min)