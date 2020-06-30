##########################################################################
# Mess around here
#
##########################################################################

Balt_busStops <- Balt_bbox %>% 
  add_osm_feature(key = "highway", value = "bus_stop") %>% 
  osmdata_sf() %>% 
  # keep only the points. Note that the query returned 4333 points, 9 polygons, and 1 multi-line feature
  .$osm_points

blt_transit <- Balt_bbox %>% 
  add_osm_feature(key = "public_transport") %>% 
  osmdata_sf()

sum(!Balt_busStops$osm_id %in% blt_transit$osm_points$osm_id)

blt_transit_points <- blt_transit$osm_points

tmap_mode("view")

tm_shape(blt_transit_points) +
  tm_dots(col = "blue") +
  tm_shape(blt_transit$osm_lines) +
  tm_lines(col = "black") +
  tm_shape(blt_transit$osm_polygons) +
  tm_polygons(col = "red") +
  tm_shape(tmp) +
  tm_dots(col = "yellow")

setdiff(names(blt_transit$osm_points),
        names(blt_transit$osm_lines))

tmp <- Balt_busStops %>% 
  filter(!Balt_busStops$osm_id %in% blt_transit$osm_points$osm_id)

tm_shape(blt_transit_points) + tm_dots(col = "black") +
  tm_shape(tmp) + tm_dots(col = "red")
