##########################################################################
# This script:
# 1. Runs the OSM data admin scripts
# 2. Defines a vector of public transport keys for the Overpass query
#
# Exports: 
# 1. 
# 
# To-do:
#
##########################################################################

## 1. ----
source("~scripts/10 - OSM data admin.R")

## 2. ----
transport_keys <- c("public_transport")

## 3. ----
pubTrans <- map(bboxes,
                ~ .x %>% 
                  add_osm_feature(key = transport_keys) %>% 
                  osmdata_sf())
