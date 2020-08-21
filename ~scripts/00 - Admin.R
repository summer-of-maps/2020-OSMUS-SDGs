##########################################################################
# This script:
# 1. Loads packages 
# 2. Defines global variables
#   (a) Cities for SDG analysis
#   (b) Projections for the cities
# 3. 
#   (a) Sets Census API key
#   (b) Caches Census shapefiles
#   (c) Sets Google API key
# 4. Turns off scientific notation
# 5. Sets visualization aesthetics
#
# Exports:
#
# To-do:
# 1. 
##########################################################################

## 1. ----
# Data reading and wrangling
library(tidyverse)
library(measurements) # unit conversion

# spatial data
library(sf)
library(tmap) # thematic mapping
library(tmaptools) # spatial utility functions
library(walkscoreAPI) # for walksheds

# census
library(tidycensus)
library(tigris)

# visualization and geocoding
library(ggmap) # basemaps
library(gridExtra)
library(knitr)
library(kableExtra)
# library(wesanderson) # palettes
library(osmplotr)
library(ggsn)

# OSM and OSHDB tools
library(osmdata)
library(httr)
library(jsonlite)
library(osrm) # isochrones
library(nominatim)

# debugging
library(rbenchmark) # time processing speed

## 2. ----
# (a)
sdg_cities <- data.frame(city = c("Baltimore, Maryland",
                                  "Minneapolis, Minnesota",
                                  "New Orleans, Louisiana",
                                  "Philadelphia, Pennsylvania",
                                  "Houston, Texas",
                                  "San Francisco, California",
                                  "Pennsylvania"),
                         proj = c(2248,
                                  26821,
                                  3452,
                                  2272,
                                  2278,
                                  2227,
                                  2272))

sdg_cities_list <- split(sdg_cities, f = seq(nrow(sdg_cities))) %>% 
  set_names(sdg_cities$city)

## 3. ----
# (a)
census_key <- readRDS("API_keys/census_api_key.rds")
census_api_key(census_key, install = T, overwrite = TRUE)

# (c)
options(tigris_use_cache = TRUE)

## 4. ----
options(scipen = 999)

## 5. ----
plotTheme <- function(){
  theme_bw()
}

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text(color = "black"),
    plot.title = element_text(size = 14, colour = "black"),
    plot.subtitle = element_text(face = "italic"),
    plot.caption = element_text(hjust = 1),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
    # panel.border = element_rect(colour = "black", fill = NA, size = 2)
  )
}
