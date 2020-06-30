##########################################################################
# This script includes all mapping tasks for the Transit SDG:
# 1. All transit stop points
#
# Exports: 
# 1. transit_point_maps as 31_transit_point_maps/[City]_transitStops.png
# 
# To-do:
#
##########################################################################

## 1. ----
transit_point_maps <- map2(pubTrans,
                           sdg_basemaps,
                           ~ ggmap(.y) +
                             geom_sf(data = .x$osm_points,
                                     inherit.aes = FALSE,
                                     alpha = 0.5) +
                             labs(title = "Bus Stops in the \'public_transport\' Tag",
                                  caption = "Data source: OSM") +
                             mapTheme())

## 1. Export as png ----
map2(transit_point_maps,
     names(transit_point_maps),
    ~ ggsave(plot = .x,
             units = "in",
             device = "png",
             height = 8,
             dpi = 72,
             filename = paste("~plots/31_transit_point_maps/",
                              .y,
                              "_transitStops.png",
                              sep = "")))
