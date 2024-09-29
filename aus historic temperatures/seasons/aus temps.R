
# ----------------------------------
# DEPENDENCIES 
# ----------------------------------

library(tidyverse)
library(forcats)

font_add_google("Inter")
showtext_auto()

# ----------------------------------
# DATA-SOURCE
# ----------------------------------

tbl <- read.csv("/Users/perkot/Dropbox/Analytics/Personal/R/git/ACORN-SAT-Climate-Data-gganimate/ACORN-SAT-Clean.csv")

# ----------------------------------
# TIDY STATION NAMES 
# ----------------------------------

# Sub-out only first word 
# "([A-Za-z]+).*", "\\1" 

# Parse out first two-words of string
tbl$Station <- gsub("(\\w+\\s+\\w+).*", "\\1", 
                    tbl$site.name)

# Get unique station names 
unique_station <- unique(tbl$Station)
unique_station

# Remove these specific words 
tbl <- tbl %>% mutate_all(~gsub("AIRPORT", "", .))
tbl <- tbl %>% mutate_all(~gsub("AERO", "", .))
tbl <- tbl %>% mutate_all(~gsub("RAAF", "", .))

# Change these station names 
tbl$Station[tbl$Station == "BATHURST AGRICULTURAL"] <- "BATHURST"
tbl$Station[tbl$Station == "LARAPUNA (EDDYSTONE POINT"] <- "LARAPUNA"
tbl$Station[tbl$Station == "INVERELL (RAGLAN ST"] <- "INVERELL"
tbl$Station[tbl$Station == "KALGOORLIE-BOULDER "] <- "KALGOORLIE"
tbl$Station[tbl$Station == "MELBOURNE (OLYMPIC PARK"] <- "MELBOURNE"
tbl$Station[tbl$Station == "SYDNEY (OBSERVATORY HILL"] <- "SYDNEY"
tbl$Station[tbl$Station == "RUTHERGLEN RESEARCH"] <- "RUTHERGLEN"
tbl$Station[tbl$Station == "HOBART (ELLERSLIE ROAD"] <- "HOBART"
tbl$Station[tbl$Station == "CAMOOWEAL TOWNSHIP"] <- "CAMOOWEAL"
tbl$Station[tbl$Station == "CUNDERDIN AIRFIELD"] <- "CUNDERDIN"
tbl$Station[tbl$Station == "GROVE (RESEARCH STATION"] <- "GROVE"
tbl$Station[tbl$Station == "GILES METEOROLOGICAL"] <- "GILES"

# ----------------------------------
# CREATE YEARLY AVERAGE AGGREGATION
# ----------------------------------

# convert to numeric 
tbl$maximum.temperature..degC. <- as.numeric(tbl$maximum.temperature..degC.)
tbl$minimum.temperature..degC. <- as.numeric(tbl$minimum.temperature..degC.)

# Aggregate 
Yearly_Average <- tbl %>% 
  group_by(Year, Station) %>% 
  summarise(avg_max = round(mean(maximum.temperature..degC., na.rm=T,),2),
            avg_min = round(mean(minimum.temperature..degC., na.rm=T),2)) 

# Ensure dataframe
Yearly_Average <- as.data.frame(Yearly_Average)

# Convert 'year' to factor
Yearly_Average$Year <- as.factor(Yearly_Average$Year)

# Convert average temperatures to numeric 
Yearly_Average$avg_max <- as.numeric(Yearly_Average$avg_max)
Yearly_Average$avg_min <- as.numeric(Yearly_Average$avg_min)

# ----------------------------------
# PREPARE FOR DATA VISUALISATION  
# ----------------------------------

# https://www.royfrancis.com/a-guide-to-elegant-tiled-heatmaps-in-r-2019/

# -------------------
# BUCKET MAX TEMPS 
# -------------------

# Create buckets for avg_max
Yearly_Average <- Yearly_Average %>%
  # convert state to factor and reverse order of levels
  mutate(Station=factor(Station,levels=rev(sort(unique(Station))))) %>%
  # create a new variable from count
  mutate(maxbin=cut(avg_max,
                    breaks=c(0,5,10,15,20,25,30,35,40,max(avg_max,na.rm = T)),
                    labels=c("0-5°", 
                             "5-10°",
                             "10-15°",
                             "15-20°",
                             "20-25°",
                             "25-30°",
                             "30-35°",
                             "35-40°",
                             "40-45°"))) %>%
  # change level order
  mutate(maxbin=factor(as.character(maxbin),levels=rev(levels(maxbin))))

# -------------------
# BUCKET MIN TEMPS 
# -------------------

# Create buckets for avg_min
Yearly_Average <- Yearly_Average %>%
  # convert state to factor and reverse order of levels
  mutate(Station = factor(Station,
                          levels = rev(sort(unique(Station))))) %>%
  # create a new variable from count
  mutate(minbin=cut(avg_min,
                    breaks=c(0,5,10,15,20,25,30,35,40,max(avg_min,na.rm = T)),
                    labels=c("0-5°", 
                             "5-10°",
                             "10-15°",
                             "15-20°",
                             "20-25°",
                             "25-30°",
                             "30-35°",
                             "35-40°",
                             "40-45°"))) %>%
  # change level order
  mutate(minbin=factor(as.character(minbin),levels=rev(levels(minbin))))

# -------------------
# CREATE "DATA GRID"
# -------------------

# Where stations do not have temperature data, create empty records 
# https://stackoverflow.com/questions/9996452/r-find-and-add-missing-non-existing-rows-in-time-related-data-frame

grid <- expand.grid(Year = unique(Yearly_Average$Year),
                    Station = unique(Yearly_Average$Station))
Yearly_Average <- 
  merge(grid,
        Yearly_Average,
        all = TRUE)

# -------------------
# GET AVERAGE TEMP PER STATION FOR ORDERING 
# -------------------

# Maximum Average
Max <- Yearly_Average %>% 
  group_by(Station) %>% 
  summarise(Max_Average = round(mean(avg_max, na.rm = TRUE),2)) 
Yearly_Average <- left_join(Yearly_Average, Max, by = "Station") 

# Minimum Average
Min <- Yearly_Average %>% 
  group_by(Station) %>% 
  summarise(Min_Average = round(mean(avg_min, na.rm = TRUE),2)) 
Yearly_Average <- left_join(Yearly_Average, Min, by = "Station") 

rm(Max, Min)

# ----------------------------------
# KEEP A .CSV
# ----------------------------------

# Standard .csv export 

# write.csv(Yearly_Average, file = "Yearly_Average.csv",
#           na = "", 
#           row.names = FALSE)

# D3_TEMP <- Yearly_Average %>% 
#   select(Year, Station, avg_max, maxbin, Max_Average)
# 
# write.csv(D3_TEMP, file = "D3_TEMP.csv",
#           na = "",
#           row.names = FALSE)

# ----------------------------------
# CREATE AESTHETICS FOR VISUALISATIONS 
# ----------------------------------

# assign text colour for visualisation 
textcol <- "grey40"
texthot <- "#6b3029"
textcold <- "#3d6c80"

# Warm Colours
WarmPalette <- c("40°" = "#780018", 
                 "35°" = "#96002b", 
                 "30°" = "#d13024", 
                 "25°" = "#e87a13", 
                 "20°" = "#edb41c", 
                 "15°" = "#e8d18e", 
                 "10°" = "#f2eaac", 
                 "5°" = "#fcf8d4",
                 "0°" = "#fff9e6")

WarmPalette2 <- c("40°" = "#780018", 
                 "35°" = "#96002b", 
                 "30°" = "#d13024", 
                 "25°" = "#e87a13", 
                 "20°" = "#edb41c", 
                 "15°" = "#e5e84a", 
                 "10°" = "#dbe899", 
                 "5°" = "#c9f2ed",
                 "0°" = "#85def2")

WarmPalette3 <- c("40°" = "#702b28", 
                  "35°" = "#912e2a", 
                  "30°" = "#c9534e", 
                  "25°" = "#e4905c", 
                  "20°" = "#eac06c", 
                  "15°" = "#e3d58d", 
                  "10°" = "#c0d18e", 
                  "5°" = "#a5bd97",
                  "0°" = "#87a383")



# Cool Colours
CoolPalette <- c("25-30°" = "#c9f2ed", 
                 "20-25°" = "#acebf2", 
                 "15-20°" = "#85def2", 
                 "10-15°" = "#5cc1e6", 
                 "5-10°" = "#30a1c7",
                 "0-5°" = "#167dab",
                 "-5-0°" = "#055a87")

# Stylish Colours
# Warm Colours
StatePalette <- c("40°" = "#993B37", 
                  "35°" = "#cf5f1f", 
                  "30°" = "#C88370", 
                  "25°" = "#e39724", 
                  "20°" = "#edb41c", 
                  "15°" = "#e5e84a", 
                  "10°" = "#dbe899", 
                  "5°" = "#7AA2BE",
                  "0°" = "#9bcbcf")

# ----------------------------------
# VISUALISE MAX & MIN TEMPS  
# ----------------------------------

# Attempt to build a tool-tip
# new column: text for tooltip:
Yearly_Average <- Yearly_Average %>%
  mutate(text = paste0("average max °C: ", avg_max, 
                       "\n", 
                       "Year: ", Year, 
                       "\n", 
                       "Station: ", Station))


# -------------------
# ALL SEASONS 
# -------------------

# Aggregate 
All_Seasons <- tbl %>% 
  group_by(Year, Station, Season) %>% 
  summarise(avg_max = round(mean(maximum.temperature..degC., na.rm=T,),2),
            avg_min = round(mean(minimum.temperature..degC., na.rm=T,),2))

All_Seasons <- as.data.frame(All_Seasons)
All_Seasons$avg_max <- as.numeric(All_Seasons$avg_max)
All_Seasons$avg_min <- as.numeric(All_Seasons$avg_min)

# max bins
All_Seasons <- All_Seasons %>%
  # convert state to factor and reverse order of levels
  mutate(Station=factor(Station,levels=rev(sort(unique(Station))))) %>%
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
All_Seasons <- All_Seasons %>%
  # convert state to factor and reverse order of levels
  mutate(Station=factor(Station,levels=rev(sort(unique(Station))))) %>%
  # create a new variable from count
  mutate(minbin=cut(avg_min,
                    breaks=c(-5,0,5,10,15,20,25,max(avg_min,na.rm = T)),
                    labels=c("-5-0°", 
                             "0-5°", 
                             "5-10°",
                             "10-15°",
                             "15-20°",
                             "20-25°",
                             "25-30°"))) %>%
  # change level order
  mutate(minbin=factor(as.character(minbin),levels=rev(levels(minbin))))

grid.s <- expand.grid(Year = unique(All_Seasons$Year),
                      Station = unique(All_Seasons$Station),
                      Season = unique(All_Seasons$Season))
All_Seasons <- 
  merge(grid.s,
        All_Seasons,
        all = TRUE)

# Maximum Average
Max <- All_Seasons %>% 
  group_by(Station, Season) %>% 
  summarise(Max_Average = round(mean(avg_max, na.rm = TRUE),2)) 
All_Seasons <- left_join(All_Seasons, Max, by = c("Station", "Season")) 

# Maximum Average
Min <- All_Seasons %>% 
  group_by(Station, Season) %>% 
  summarise(Min_Average = round(mean(avg_min, na.rm = TRUE),2)) 
All_Seasons <- left_join(All_Seasons, Min, by = c("Station", "Season")) 

# Convert year from factor back to numeric
All_Seasons$Year <- as.character(All_Seasons$Year)
All_Seasons$Year <- as.numeric(All_Seasons$Year)
# In order to filter range of data
All_Seasons_1960 <- All_Seasons %>% 
  filter(Year >= 1960)
# Convert back to factor 
All_Seasons_1960$Year <- as.factor(All_Seasons_1960$Year)
All_Seasons$Year <- as.factor(All_Seasons$Year)

# Required to custom order facets 
All_Seasons_1960$Season <- factor(All_Seasons_1960$Season, 
                                  levels = c("Winter",
                                             "Autumn",
                                             "Spring",
                                             "Summer"))

# All_Seasons_1960$maxbin <- as.character(All_Seasons_1960$maxbin)
# 
# All_Seasons_1960 <- All_Seasons_1960 %>%
#   mutate(across(everything(), ~ replace(.x, is.na(.x), "")))
# 
# All_Seasons_1960$maxbin <- as.factor(All_Seasons_1960$maxbin)


# Attempt to build a tool-tip
# new column: text for tooltip:

# All_Seasons_1960 <- All_Seasons_1960 %>%
#   mutate(text = paste0(" average max °C: ", avg_max,
#                        "\n",
#                        "Year: ", Year,
#                        "\n",
#                        "Station: ",Station))

TILE.MAX.SEASONS <- All_Seasons_1960 %>% 
  # order by station, & average max temperature 
  mutate(name = fct_reorder(Station, Max_Average)) %>%
  ggplot(aes(x = Year,
             y = name,
             fill = maxbin)) + 
  geom_tile(colour = "white",
            size = 0.2,
            na.rm = FALSE) +
  facet_grid(~ Season) +
  coord_equal() + # perfect square 
  guides(fill = guide_legend(title = "°C")) +
  labs(x = "",
       y = "",
       title = "Australian surface air temperature x season",
       subtitle = "Daily recordings from 112 weather stations between 1960 & 2019",
       caption = "Data Source: The Australian Climate Observations Reference Network – Surface Air Temperature (ACORN-SAT)"
  ) +
  scale_y_discrete(expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0),
                   breaks = c("1960","1970","1980","1990","2000","2010")) +
  scale_fill_manual(values=WarmPalette3,
                    na.value = "#fcf5e1",
                    na.translate = F) + #remove NA as factor level  
  #coord_fixed()+
  theme_grey(base_size = 10)+
  theme(legend.position = "right",
        legend.direction = "vertical",
        # legend.title = element_text(colour = "#4E4F4E",
        #                             size=10),
        legend.title = element_blank(),
        legend.margin = margin(grid::unit(0,"cm")),
        legend.text = element_text(colour = "#4E4F4E",
                                   size = 8,
                                   face = "bold"),
        legend.key.height = grid::unit(0.8,"cm"),
        legend.key.width = grid::unit(0.2,"cm"),
        # axis.text.x = element_blank(),
        # axis.text.y = element_blank(),
        # axis.ticks = element_blank(),
        axis.text.x = element_text(size = 6,
                                   colour = "#4E4F4E",
                                   family = "Inter"),
        axis.text.y = element_text(size = 4,
                                   vjust = 0.2,
                                   colour = "#4E4F4E",
                                   family = "Inter"),
        axis.ticks = element_line(size = 0.2),
        plot.background = element_rect(fill = "#fcf9f0"),
        legend.background = element_rect(fill = "#fcf9f0"),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 9, colour = "#4E4F4E"),
        strip.background = element_rect(fill="#fcf9f0"),
        plot.margin = margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title = element_text(colour = "#4E4F4E",
                                  hjust = 0,
                                  size = 12,
                                  face = "bold",
                                  family = "Inter"),
        plot.subtitle = element_text(colour = "#4E4F4E",
                                     hjust = 0,
                                     size = 10,
                                     family = "Inter"),
        plot.caption = element_text(colour = "#4E4F4E",
                                    hjust = 0,
                                    vjust = 1,
                                    size = 6,
                                    face = "italic",
                                    family = "Inter",
                                    margin = margin(-5,0,0,0))) # adjust position ... top, bottom, left, right
TILE.MAX.SEASONS