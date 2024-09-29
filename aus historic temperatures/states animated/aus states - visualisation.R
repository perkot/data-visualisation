# -----------------
# Dependencies 
# -----------------

require(tidyverse)
require(readr)
require(ggplot2)
require(gganimate)
require(viridis)
require(scales)
require(gifski)
require(png)
require(transformr)
require(kableExtra)

font_add_google("Inter")
showtext_auto()

# -----------------
# Data
# -----------------

tbl <- read.csv("ACORN-SAT-Clean.csv")

# -----------------
# aesthetics
# -----------------

# color themes 
state.colors.gradient.2 <- 
  c("NT" = "#993B37", 
    "TAS" = "#7AA2BE", 
    "VIC" = "#4EA599",  
    "SA" = "#E1A345", 
    "NSW" = "#bda768", 
    "WA" = "#C88370", 
    "QLD" = "#CA6A33") 

# Themes
theme_plot <-
  theme(
    plot.title = element_text(size = 14, hjust = 0, colour = "#4E4F4E", face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0, colour = "#4E4F4E"),
    axis.title = element_text(size = 12, colour = "#4E4F4E"),
    legend.title = element_text(size = 12, colour = "#4E4F4E"),
    axis.text = element_text(size = 12, colour = "#4E4F4E"),
    panel.background = element_rect(fill = "#fcf9f0",
                                    colour = "#fcf9f0"),
    plot.background = element_rect(fill = "#fcf9f0",
                                   colour = "#fcf9f0"))

# -----------------
# Visualise 
# -----------------

# Create table
temp.state <- tbl %>% 
  group_by(state, era) %>% 
  filter(era != 2019) %>% 
  summarise(avgmax = mean(maximum.temperature..degc., na.rm = TRUE))

# as character
temp.state$era <- as.character(temp.state$era)
# remove s
temp.state$era = substr(temp.state$era,1,nchar(temp.state$era)-1)
# to numeric
temp.state$era <- as.numeric(temp.state$era)

# Plot
state.plot.2 <- 
  ggplot(temp.state, aes(era, avgmax, group = state, color = state)) + 
  geom_line(size = 1.6) + 
  geom_segment(aes(xend = 2018, yend = avgmax), linetype = 2, colour = 'grey') + 
  geom_point(size = 8) + 
  geom_text(aes(x = 2018, label = state, size = 16), hjust = 0) + 
  scale_colour_manual(values = state.colors.gradient.2) +
  coord_cartesian(clip = 'off') + 
  labs(title = 'Average temperature in C° over the past 100 years',
       subtitle = 'Separated by Australian state',
       caption  = "Data Source: ACORN-SAT",
       y = 'C°',
       X = "Year") +
  theme_minimal() + 
  theme(legend.position = "none") +
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) + 
  theme_plot +
  transition_reveal(era) 

# -----------------
# save plot
# -----------------

state.plot.2.anim <-
animate(state.plot.2,
        end_pause = 80,
        fps = 30,
        nframe = 240,
        height = 1024,
        width = 768)
state.plot.2.anim

fname <- "/Users/perkot/GIT/data-visualisation/aus historic temperatures/states animated/plot/austempstates.gif"
anim_save(fname, state.plot.2.anim, width = 4000, height = 4000)
viewer <- getOption("viewer")
viewer(fname)