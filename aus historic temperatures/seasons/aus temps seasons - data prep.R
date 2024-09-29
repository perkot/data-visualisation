# -----------------
# Dependencies  
# -----------------

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
tbl$station[tbl$station == "GROVE (RESEARCH STATION"] <- "GROVE"
tbl$station[tbl$station == "GILES METEOROLOGICAL"] <- "GILES"

# -------------------
# prepare visualisation table 
# -------------------

# check data
str(tbl)

# as numeric
tbl$maximum.temperature..degc. <- as.numeric(tbl$maximum.temperature..degc.)
tbl$minimum.temperature..degc. <- as.numeric(tbl$minimum.temperature..degc.)

# Aggregate 
as <- tbl %>% 
  group_by(year, station, season) %>% 
  summarise(avg_max = round(mean(maximum.temperature..degc., na.rm=T,),2),
            avg_min = round(mean(minimum.temperature..degc., na.rm=T,),2))

# ensure df
as <- as.data.frame(as)

# max bins
as <- as %>%
  # convert state to factor and reverse order of levels
  mutate(station=factor(station,levels=rev(sort(unique(station))))) %>%
  # create a new variable from count
  mutate(maxbin=cut(avg_max,
                    breaks=c(0,5,10,15,20,25,30,35,40,max(avg_max,na.rm = T)),
                    labels=c("0°", 
                             "5°",
                             "10°",
                             "15°",
                             "20°",
                             "25°",
                             "30°",
                             "35°",
                             "40°"))) %>%
  # change level order
  mutate(maxbin=factor(as.character(maxbin),levels=rev(levels(maxbin))))

# min bins
as <- as %>%
  # convert state to factor and reverse order of levels
  mutate(station=factor(station,levels=rev(sort(unique(station))))) %>%
  # create a new variable from count
  mutate(minbin=cut(avg_min,
                    breaks=c(-5,0,5,10,15,20,25,max(avg_min,na.rm = T)),
                    labels=c("-5°", 
                             "0°", 
                             "5°",
                             "10°",
                             "15°",
                             "20°",
                             "25°"))) %>%
  # change level order
  mutate(minbin=factor(as.character(minbin),levels=rev(levels(minbin))))

# grid expand
grid.s <- expand.grid(year = unique(as$year),
                      station = unique(as$station),
                      season = unique(as$season))
as <- 
  merge(grid.s,
        as,
        all = TRUE)

# Maximum Average
max <- as %>% 
  group_by(station, season) %>% 
  summarise(max_average = round(mean(avg_max, na.rm = TRUE),2)) 
as <- left_join(as, max, by = c("station", "season")) 

# Maximum Average
min <- as %>% 
  group_by(station, season) %>% 
  summarise(min_average = round(mean(avg_min, na.rm = TRUE),2)) 
as <- left_join(as, min, by = c("station", "season")) 

# Convert year from factor back to numeric
as$year <- as.character(as$year)
as$year <- as.numeric(as$year)

# In order to filter range of data
as_1960 <- as %>% 
  filter(year >= 1960)

# Convert back to factor 
as_1960$year <- as.factor(as_1960$year)
as_1960$year <- as.factor(as_1960$year)

# Required to custom order facets 
as_1960$season <- factor(as_1960$season, 
                                  levels = c("Winter",
                                             "Autumn",
                                             "Spring",
                                             "Summer"))
