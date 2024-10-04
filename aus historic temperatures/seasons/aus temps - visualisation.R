# -----------------
# Dependencies 
# -----------------

library(tidyverse)
library(forcats)
library(showtext)
library(ggtext)


font_add_google("Inter")
showtext_auto()

# -----------------
# Data
# -----------------

as_1960 <- read.csv("/Users/perkot/GIT/data/as_1960.csv")

# -----------------
# Aesthetics 
# -----------------

WarmPalette3 <- c(
  "40°" = "#702b28", 
  "35°" = "#912e2a",
  "30°" = "#c9534e", 
  "25°" = "#e4905c", 
  "20°" = "#eac06c", 
  "15°" = "#e3d58d", 
  "10°" = "#c0d18e", 
   "5°" = "#a5bd97",
   "0°" = "#87a383",
  "-5°" = "#6fab9d"
  )

# -----------------
# Visualise max temps 
# -----------------

p.max <- as_1960 %>% 
  mutate(name = fct_reorder(station, max_average)) %>%
  ggplot(aes(x = year,
             y = name,
             fill = maxbin)) + 
  geom_tile(colour = "white",
            linewidth  = 0.2,
            na.rm = FALSE) +
  facet_grid(~ season) +
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
  scale_fill_manual(values = WarmPalette3,
                    na.value = "#fcf9f0",
                    breaks = c("40°","35°","30°","25°","20°","15°","10°","5°","0°")
                    ) +
  theme_grey(base_size = 10)+
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.margin = margin(grid::unit(0,"cm")),
        legend.text = element_text(colour = "#4E4F4E",
                                   size = 80,
                                   # face = "bold",
                                   family = "Inter"),
        legend.key.height = grid::unit(0.8,"cm"),
        legend.key.width = grid::unit(0.8,"cm"),
        axis.text.x = element_text(size = 60,
                                   colour = "#4E4F4E",
                                   family = "Inter"),
        axis.text.y = element_text(size = 40,
                                   vjust = 0.2,
                                   colour = "#4E4F4E",
                                   family = "Inter"),
        axis.ticks = element_line(size = 0.2),
        plot.background = element_rect(fill = "#fcf9f0"),
        legend.background = element_rect(fill = "#fcf9f0"),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 80, colour = "#4E4F4E", family = "Inter"),
        strip.background = element_rect(fill="#fcf9f0"),
        plot.margin = margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title = element_text(colour = "#4E4F4E",
                                  hjust = 0,
                                  size = 100,
                                  face = "bold",
                                  family = "Inter"),
        plot.subtitle = element_text(colour = "#4E4F4E",
                                     hjust = 0,
                                     size = 80,
                                     family = "Inter"),
        plot.caption = element_text(colour = "#4E4F4E",
                                    hjust = 0,
                                    vjust = 1,
                                    size = 60,
                                    face = "italic",
                                    family = "Inter",
                                    margin = margin(3,0,0,0))) 

# -----------------
# Visualise min temps 
# -----------------

p.min <- as_1960 %>% 
  mutate(name = fct_reorder(station, min_average)) %>%
  ggplot(aes(x = year,
             y = name,
             fill = minbin)) + 
  geom_tile(colour = "white",
            linewidth  = 0.2,
            na.rm = FALSE) +
  facet_grid(~ season) +
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
  scale_fill_manual(values = WarmPalette3,
                    na.value = "#fcf9f0",
                    breaks = c("40°","35°","30°","25°","20°","15°","10°","5°","0°","-5°")
  ) +
  theme_grey(base_size = 10)+
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.margin = margin(grid::unit(0,"cm")),
        legend.text = element_text(colour = "#4E4F4E",
                                   size = 80,
                                   # face = "bold",
                                   family = "Inter"),
        legend.key.height = grid::unit(0.8,"cm"),
        legend.key.width = grid::unit(0.8,"cm"),
        axis.text.x = element_text(size = 60,
                                   colour = "#4E4F4E",
                                   family = "Inter"),
        axis.text.y = element_text(size = 40,
                                   vjust = 0.2,
                                   colour = "#4E4F4E",
                                   family = "Inter"),
        axis.ticks = element_line(size = 0.2),
        plot.background = element_rect(fill = "#fcf9f0"),
        legend.background = element_rect(fill = "#fcf9f0"),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 80, colour = "#4E4F4E", family = "Inter"),
        strip.background = element_rect(fill="#fcf9f0"),
        plot.margin = margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title = element_text(colour = "#4E4F4E",
                                  hjust = 0,
                                  size = 100,
                                  face = "bold",
                                  family = "Inter"),
        plot.subtitle = element_text(colour = "#4E4F4E",
                                     hjust = 0,
                                     size = 80,
                                     family = "Inter"),
        plot.caption = element_text(colour = "#4E4F4E",
                                    hjust = 0,
                                    vjust = 1,
                                    size = 60,
                                    face = "italic",
                                    family = "Inter",
                                    margin = margin(3,0,0,0))) 

# -----------------
# Save & View   
# -----------------

fname <- "/Users/perkot/GIT/data-visualisation/aus historic temperatures/seasons/plot/austempseason.png"
ggsave(fname,  p.max, width = 24, height = 12, bg = "#fcf9f0", units = "in", dpi = 600)
viewer <- getOption("viewer")
viewer(fname)

fname <- "/Users/perkot/GIT/data-visualisation/aus historic temperatures/seasons/plot/austempseasonmin.png"
ggsave(fname,  p.min, width = 24, height = 12, bg = "#fcf9f0", units = "in", dpi = 600)
viewer <- getOption("viewer")
viewer(fname)
