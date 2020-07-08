##########################################################################
# This script:
# 1. Collects ggmap basemaps for SDG cities
#
# Exports: 
# 1. sdg_basemaps as 30_sdg_basemaps.rds
# 
# To-do:
#
##########################################################################

## 1. ----
sdg_basemaps <- map(set_names(sdg_cities),
                    ~ get_map(getbb(.x), 
                              maptype = "toner-background"))

## 1. Export as RDS ----
# saveRDS(sdg_basemaps,
#         "~objects/30/30_sdg_basemaps.rds")
