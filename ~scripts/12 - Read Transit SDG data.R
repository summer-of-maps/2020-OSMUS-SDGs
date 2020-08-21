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
# 4. Downloads GTFS transit stops
#
# Exports: 
# 1. 
# 
# To-do:
# 1. 
##########################################################################

## 1. ----
source("~scripts/10 - Data admin.R")

## 2. ----
walkroads <- map2(bbox_mats[1:3],
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

walkroads$Philadelphia <- bbox_mats$`Philadelphia, Pennsylvania` %>% 
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
  .$osm_lines %>% 
  st_transform(sdg_cities_list$`Philadelphia, Pennsylvania`$proj) %>% 
  st_intersection(bbox_sfs$`Philadelphia, Pennsylvania`)

walkroads$Houston <- st_read("~objects/~large_files/state_roads/TX/gis_osm_roads_free_1.shp") %>% 
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
                       "pier")) %>% 
  dplyr::select(highway = fclass, geometry) %>% 
  st_transform(sdg_cities_list$`Houston, Texas`$proj) %>% 
  st_intersection(bbox_sfs$`Houston, Texas`)

walkroads$`San Francisco` <- bbox_mats$`San Francisco, California` %>% 
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
  .$osm_lines %>% 
  st_transform(sdg_cities_list$`San Francisco, California`$proj) %>% 
  st_intersection(bbox_sfs$`San Francisco, California`)



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

buses1 <- map(bbox_mats[1:6],
              ~ .x %>% 
                add_osm_feature(key = "highway", value = "bus_stop") %>% 
                osmdata_sf() %>% 
                # below removes geometries that are part of a higher
                # level geometry. I.e., undoes "recursive search".
                unique_osmdata())

buses2 <- map(bbox_mats[1:6],
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
trams <- map(bbox_mats[1:6],
             ~ .x %>% 
               add_osm_feature(key = "railway", value = "tram_stop") %>% 
               osmdata_sf() %>% 
               unique_osmdata())

# subways/metros/light rail
rail <- map(bbox_mats[1:6],
             ~ .x %>% 
               add_osm_feature(key = "railway", value = "station") %>% 
               # add_osm_feature(key = "station", value = c("subway", "light_rail")) %>% # some regional rail in PA has no station tag
               osmdata_sf() %>% 
               unique_osmdata())

# ferries
ferries <- map(bbox_mats[1:6],
              ~ .x %>% 
                add_osm_feature(key = "amenity", value = "ferry_terminal") %>% 
                osmdata_sf() %>% 
                unique_osmdata() %>% 
                .$osm_points)

## 4. ----
transit_openData_list <- vector("list", length(sdg_cities_list)) %>% 
  set_names(names(sdg_cities_list))

# Baltimore
balt_path <- "~objects/TransitSDG_GTFS_data/Baltimore"
balt_bus <- read_csv(file.path(balt_path, "bus/stops.txt")) %>% 
  dplyr::select(stop_id, stop_name, stop_lat, stop_lon) %>% 
  mutate(type = "bus") %>% 
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>% 
  st_transform(sdg_cities_list$`Baltimore, Maryland`$proj)

balt_subway <- read_csv(file.path(balt_path, "subway/stops.txt")) %>% 
  dplyr::select(stop_id, stop_name, stop_lat, stop_lon) %>% 
  mutate(type = "subway") %>% 
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>% 
  st_transform(sdg_cities_list$`Baltimore, Maryland`$proj)

balt_lightRail <- read_csv(file.path(balt_path, "light_rail/stops.txt")) %>% 
  dplyr::select(stop_id, stop_name, stop_lat, stop_lon) %>% 
  mutate(type = "light_rail") %>% 
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>% 
  st_transform(sdg_cities_list$`Baltimore, Maryland`$proj)

balt_commuterRail <- read_csv(file.path(balt_path, "commuter_rail/stops.txt")) %>% 
  dplyr::select(stop_id, stop_name, stop_lat, stop_lon) %>% 
  mutate(type = "commuter_rail") %>% 
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>% 
  st_transform(sdg_cities_list$`Baltimore, Maryland`$proj)

transit_openData_list$`Baltimore, Maryland` <- bind_rows(
  balt_bus,
  balt_subway,
  balt_lightRail,
  balt_commuterRail) %>% 
  mutate(stop_id = as.character(stop_id)) %>% 
  .[bbox_sfs$`Baltimore, Maryland`,]

# Minneapolis
mpls_path <- "~objects/TransitSDG_GTFS_data/Minneapolis"
mpls_tmp1 <- read_csv(file.path(mpls_path, "Metro Transit/stops.txt")) %>% 
  dplyr::select(stop_id, stop_name, stop_lat, stop_lon) %>% 
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>% 
  mutate(stop_id = as.character(stop_id)) %>% 
  st_transform(sdg_cities_list$`Minneapolis, Minnesota`$proj)
mpls_tmp2 <- read_csv(file.path(mpls_path, "MVTA/stops.txt")) %>% 
  dplyr::select(stop_id, stop_name, stop_lat, stop_lon) %>% 
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>% 
  st_transform(sdg_cities_list$`Minneapolis, Minnesota`$proj)

transit_openData_list$`Minneapolis, Minnesota` <- bind_rows(
  mpls_tmp1,
  mpls_tmp2) %>% 
  .[bbox_sfs$`Minneapolis, Minnesota`,]

# New Orleans
nola_path <- "~objects/TransitSDG_GTFS_data/New Orleans/stops.txt"

transit_openData_list$`New Orleans, Louisiana` <- read_csv(nola_path) %>% 
  dplyr::select(stop_id, stop_name, stop_lat, stop_lon) %>% 
  mutate(stop_id = as.character(stop_id)) %>% 
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>% 
  st_transform(sdg_cities_list$`New Orleans, Louisiana`$proj) %>% 
  .[bbox_sfs$`New Orleans, Louisiana`,]

# Philadelphia
phl_path <- "~objects/TransitSDG_GTFS_data/Philadelphia"

phl_tmp1 <- read_csv(file.path(phl_path, "bus/stops.txt")) %>% 
  dplyr::select(stop_id, stop_name, stop_lat, stop_lon) %>% 
  mutate(stop_id = as.character(stop_id)) %>% 
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>% 
  st_transform(sdg_cities_list$`Philadelphia, Pennsylvania`$proj) %>% 
  .[bbox_sfs$`Philadelphia, Pennsylvania`,]

phl_tmp2 <- read_csv(file.path(phl_path, "bus/stops.txt")) %>% 
  dplyr::select(stop_id, stop_name, stop_lat, stop_lon) %>% 
  mutate(stop_id = as.character(stop_id)) %>% 
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>% 
  st_transform(sdg_cities_list$`Philadelphia, Pennsylvania`$proj) %>% 
  .[bbox_sfs$`Philadelphia, Pennsylvania`,]

transit_openData_list$`Philadelphia, Pennsylvania` <- bind_rows(
  phl_tmp1,
  phl_tmp2) %>% 
  .[bbox_sfs$`Philadelphia, Pennsylvania`,]

# Houston
hou_path <- "~objects/TransitSDG_GTFS_data/Houston/stops.txt"

transit_openData_list$`Houston, Texas` <- read_csv(hou_path) %>% 
  dplyr::select(stop_id, stop_name, stop_lat, stop_lon) %>% 
  mutate(stop_id = as.character(stop_id)) %>% 
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>% 
  st_transform(sdg_cities_list$`Houston, Texas`$proj) %>% 
  .[bbox_sfs$`Houston, Texas`,]

# San Francisco
sf_path <- "~objects/TransitSDG_GTFS_data/San Francisco"

sf_tmp1 <- read_csv(file.path(sf_path, "BART/stops.txt")) %>% 
  dplyr::select(stop_id, stop_name, stop_lat, stop_lon) %>% 
  mutate(stop_id = as.character(stop_id)) %>% 
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>% 
  st_transform(sdg_cities_list$`San Francisco, California`$proj) %>% 
  .[bbox_sfs$`San Francisco, California`,]

sf_tmp2 <- read_csv(file.path(sf_path, "Caltrain/stops.txt")) %>% 
  dplyr::select(stop_id, stop_name, stop_lat, stop_lon) %>% 
  mutate(stop_id = as.character(stop_id)) %>% 
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>% 
  st_transform(sdg_cities_list$`San Francisco, California`$proj) %>% 
  .[bbox_sfs$`San Francisco, California`,]

sf_tmp3 <- read_csv(file.path(sf_path, "Muni/stops.txt")) %>% 
  dplyr::select(stop_id, stop_name, stop_lat, stop_lon) %>% 
  mutate(stop_id = as.character(stop_id)) %>% 
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>% 
  st_transform(sdg_cities_list$`San Francisco, California`$proj) %>% 
  .[bbox_sfs$`San Francisco, California`,]

sf_tmp4 <- read_csv(file.path(sf_path, "SamTrans/stops.txt")) %>% 
  dplyr::select(stop_id, stop_name, stop_lat, stop_lon) %>% 
  mutate(stop_id = as.character(stop_id)) %>% 
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>% 
  st_transform(sdg_cities_list$`San Francisco, California`$proj) %>% 
  .[bbox_sfs$`San Francisco, California`,]

sf_tmp5 <- read_csv(file.path(sf_path, "Mission Bay TMA/stops.txt")) %>% 
  dplyr::select(stop_id, stop_name, stop_lat, stop_lon) %>% 
  mutate(stop_id = as.character(stop_id)) %>% 
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>% 
  st_transform(sdg_cities_list$`San Francisco, California`$proj) %>% 
  .[bbox_sfs$`San Francisco, California`,]

sf_tmp6 <- read_csv(file.path(sf_path, "Golden Gate Transit/Ferry/stops.txt")) %>% 
  dplyr::select(stop_id, stop_name, stop_lat, stop_lon) %>% 
  mutate(stop_id = as.character(stop_id)) %>% 
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>% 
  st_transform(sdg_cities_list$`San Francisco, California`$proj) %>% 
  .[bbox_sfs$`San Francisco, California`,]

sf_tmp7 <- read_csv(file.path(sf_path, "Golden Gate Transit/Bus/stops.txt")) %>% 
  dplyr::select(stop_id, stop_name, stop_lat, stop_lon) %>% 
  mutate(stop_id = as.character(stop_id)) %>% 
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>% 
  st_transform(sdg_cities_list$`San Francisco, California`$proj) %>% 
  .[bbox_sfs$`San Francisco, California`,]

transit_openData_list$`San Francisco, California` <- bind_rows(
  sf_tmp1,
  sf_tmp2,
  sf_tmp3,
  sf_tmp4,
  sf_tmp5,
  sf_tmp6,
  sf_tmp7) %>% 
  .[bbox_sfs$`San Francisco, California`,]

  ## 4. Export as rds ----
# saveRDS(transit_openData_list,
#         "~objects/10/12_transit_openData_list.rds")
