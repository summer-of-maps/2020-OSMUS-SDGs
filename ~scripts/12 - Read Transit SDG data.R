##########################################################################
# This script:
# 1. Runs the OSM data admin scripts
# 2. Downloads roads eligible for walking 
#     see: https://github.com/Project-OSRM/osrm-backend/blob/master/profiles/foot.lua#L34
# 3. Pulls transit stops
#   (a) buses
#   (b) trams
#   (c) rail
#   (d) ferries
#
# Exports: 
# 1. 
# 
# To-do:
# 1. Pull transit routes
#   - do this mostly to show they don't have timetable information
##########################################################################

## 1. ----
source("~scripts/10 - Data admin.R")

## 2. ----
walkroads <- map2(bboxes[1:3],
                  sdg_cities_list[1:3],
                 ~ .x %>% 
                   add_osm_feature(key = "highway", value = c("primary", 
                                                              "primary_link",
                                                              "secondary",
                                                              "secondary_link",
                                                              "tertiary",
                                                              "tertiary_link",
                                                              "unclassified",
                                                              "residential",
                                                              "road",
                                                              "living_street",
                                                              "service",
                                                              "track",
                                                              "path",
                                                              "steps",
                                                              "pedestrian",
                                                              "footway",
                                                              "pier")) %>%
                   
                   osmdata_sf() %>% 
                   unique_osmdata() %>% 
                   trim_osmdata(bb_poly = getbb(.y$city, 
                                                format_out = "polygon")) %>% 
                   .$osm_lines)

# download and process from geofrabrik since pulling from OSM API runs into rate limit
PA_walkroads <- st_read("~objects/~large_files/state_roads/PA/gis_osm_roads_free_1.shp") %>% 
  filter(fclass %in% c("primary", 
                       "primary_link",
                       "secondary",
                       "secondary_link",
                       "tertiary",
                       "tertiary_link",
                       "unclassified",
                       "residential",
                       "road",
                       "living_street",
                       "service",
                       "track",
                       "path",
                       "steps",
                       "pedestrian",
                       "footway",
                       "pier"))

## 3. ----
# buses
# server_url <- "https://overpass.kumi.systems/api/interpreter" # this server does not have a rate limit
# set_overpass_url(server_url)

buses1 <- map(bboxes,
              ~ .x %>% 
                add_osm_feature(key = "highway", value = "bus_stop") %>% 
                osmdata_sf() %>% 
                # below removes geometries that are part of a higher
                # level geometry. I.e., undoes "recursive search".
                unique_osmdata())

buses2 <- map(bboxes,
              ~ .x %>% 
                add_osm_feature(key = "public_transport", value = "platform") %>%
                add_osm_feature(key = "bus", value = "yes") %>%
                osmdata_sf() %>% 
                # below removes geometries that are part of a higher
                # level geometry. I.e., undoes "recursive search".
                unique_osmdata())

buses <- map2(buses1,
              buses2,
              ~ c(.x, .y))

# trams/streetcars/trolleys
trams <- map(bboxes,
             ~ .x %>% 
               add_osm_feature(key = "railway", value = "tram_stop") %>% 
               osmdata_sf() %>% 
               unique_osmdata())

# subways/metros/light rail
rail <- map(bboxes,
             ~ .x %>% 
               add_osm_feature(key = "railway", value = "station") %>% 
               # add_osm_feature(key = "station", value = c("subway", "light_rail")) %>% # some regional rail in PA has no station tag
               osmdata_sf() %>% 
               unique_osmdata())

# ferries
ferries <- map(bboxes,
              ~ .x %>% 
                add_osm_feature(key = "amenity", value = "ferry_terminal") %>% 
                osmdata_sf() %>% 
                unique_osmdata() %>% 
                .$osm_points)