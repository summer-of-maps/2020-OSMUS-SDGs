##########################################################################
# This script:
# 1. Loads packages 
# 2. Defines global variables
#   (a) Cities for SDG analysis
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
# 1. Consider putting aesthetics in separate script and sourcing here
##########################################################################

## 1. ----
# Data reading and wrangling
library(tidyverse)

# spatial data
library(sf)
library(tmap) # thematic mapping
library(tmaptools) # spatial utility functions

# census
library(tidycensus)
library(tigris)

# visualization and geocoding
library(ggmap) # basemaps
library(gridExtra)
library(knitr)
library(kableExtra)
# library(wesanderson) # palettes

# OSM and OSHDB tools
library(osmdata)
library(httr)
library(jsonlite)

# debugging
library(rbenchmark) # time processing speed

## 2. ----
# (a)
sdg_cities <- c("Baltimore",
                "Minneapolis",
                "New Orleans")

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
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  )
}