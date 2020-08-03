##########################################################################
# This script:
# 1. Runs the OSM data admin scripts
# 2. Downloads additional map features for the SDG maps
#     Uses open data where available, TIGER otherwise
#   a. Water
#   b. Major roads
#   c. Land
#
# Exports: 
# 1. hydrology_list as 14_hydrology_list.rds
# 2. road_list as 14_road_list.rds
# 
# To-do:
# 1. 
##########################################################################


## 1. ----
source("~scripts/10 - Data admin.R")

## 2a. ----
hydrology_list <- vector("list", length(sdg_cities_list)) %>% 
  set_names(names(sdg_cities_list))

context_bbox_list <- vector("list", length(bbox_sfs)) %>% 
  set_names(names(bbox_sfs))
context_buffer_size <- 5280 # one mile

# Baltimore
context_bbox_list$`Baltimore, Maryland` <- bbox_sfs$`Baltimore, Maryland` %>% 
  st_buffer(context_buffer_size) %>% 
  st_make_grid(n = 1) %>% 
  st_sf()

balt_tmp1 <- st_read("https://data.baltimorecity.gov/api/geospatial/xmpa-487w?method=export&format=GeoJSON") %>% 
  st_make_valid() %>% 
  dplyr::select(geometry)
balt_tmp2 <- area_water(state = "Maryland",
                      county = c("Baltimore County"),
                      class = "sf") %>% 
  dplyr::select(geometry)
balt_tmp3 <- area_water(state = "Maryland",
                        county = c("Anne Arundel"),
                        class = "sf") %>% 
  dplyr::select(geometry)

hydrology_list$`Baltimore, Maryland` <- bind_rows(balt_tmp1, balt_tmp2, balt_tmp3) %>% 
  st_transform(sdg_cities_list$`Baltimore, Maryland`$proj) %>% 
  st_intersection(context_bbox_list$`Baltimore, Maryland`)

# Minneapolis
context_bbox_list$`Minneapolis, Minnesota` <- bbox_sfs$`Minneapolis, Minnesota` %>% 
  st_buffer(context_buffer_size) %>% 
  st_make_grid(n = 1) %>% 
  st_sf()

mpls_tmp1 <- area_water(state = "Minnesota",
              county = "Hennepin",
              class = "sf")

mpls_tmp2 <- area_water(state = "Minnesota",
                        county = "Ramsey",
                        class = "sf")

mpls_tmp3 <- area_water(state = "Minnesota",
                        county = "Anoka",
                        class = "sf")

hydrology_list$`Minneapolis, Minnesota` <- bind_rows(mpls_tmp1, mpls_tmp2, mpls_tmp3) %>% 
  st_transform(sdg_cities_list$`Minneapolis, Minnesota`$proj) %>% 
  st_intersection(context_bbox_list$`Minneapolis, Minnesota`)

# New Orleans
context_bbox_list$`New Orleans, Louisiana` <- bbox_sfs$`New Orleans, Louisiana` %>% 
  st_buffer(context_buffer_size) %>% 
  st_make_grid(n = 1) %>% 
  st_sf()

nola_tmp1 <- area_water(state = "Louisiana",
                        county = "Orleans",
                        class = "sf")
nola_tmp2 <- area_water(state = "Louisiana",
                        county = "St. Tammany",
                        class = "sf")
nola_tmp3 <- area_water(state = "Louisiana",
                        county = "Jefferson Parish",
                        class = "sf")
nola_tmp4 <- area_water(state = "Louisiana",
                        county = "St. Bernard",
                        class = "sf")
nola_tmp5 <- area_water(state = "Louisiana",
                        county = "Plaquemines",
                        class = "sf")

hydrology_list$`New Orleans, Louisiana` <- bind_rows(nola_tmp1, nola_tmp2, nola_tmp3, nola_tmp4, nola_tmp5) %>%
  st_transform(sdg_cities_list$`New Orleans, Louisiana`$proj) %>% 
  st_intersection(context_bbox_list$`New Orleans, Louisiana`)

# Philadelphia
context_bbox_list$`Philadelphia, Pennsylvania` <- bbox_sfs$`Philadelphia, Pennsylvania` %>% 
  st_buffer(context_buffer_size) %>% 
  st_make_grid(n = 1) %>% 
  st_sf()

 phl_tmp1 <- st_read("http://data.phl.opendata.arcgis.com/datasets/2b10034796f34c81a0eb44c676d86729_0.kml") %>% 
  st_make_valid()
 phl_tmp2 <- area_water(state = "Pennsylvania",
                        county = "Delaware",
                        class = "sf")
 phl_tmp3 <- area_water(state = "Pennsylvania",
                        county = "Montgomery",
                        class = "sf")
 phl_tmp4 <- area_water(state = "Pennsylvania",
                        county = "Bucks",
                        class = "sf")
 phl_tmp5 <- area_water(state = "New Jersey",
                        county = "Burlington",
                        class = "sf")
 phl_tmp6 <- area_water(state = "New Jersey",
                        county = "Camden",
                        class = "sf")
 phl_tmp7 <- area_water(state = "New Jersey",
                        county = "Gloucester",
                        class = "sf")

hydrology_list$`Philadelphia, Pennsylvania` <- bind_rows(phl_tmp1, phl_tmp2, phl_tmp3, phl_tmp4, phl_tmp5, phl_tmp6, phl_tmp7) %>%
  st_transform(sdg_cities_list$`Philadelphia, Pennsylvania`$proj) %>% 
  st_intersection(context_bbox_list$`Philadelphia, Pennsylvania`)

# Houston
context_bbox_list$`Houston, Texas` <- bbox_sfs$`Houston, Texas` %>% 
  st_buffer(context_buffer_size) %>% 
  st_make_grid(n = 1) %>% 
  st_sf()

hou_tmp1 <- area_water(state = "Texas",
                       county = c("Harris County"),
                       class = "sf")
hou_tmp2 <- area_water(state = "Texas",
                       county = c("Fort Bend"),
                       class = "sf")
hou_tmp3 <- area_water(state = "Texas",
                       county = c("Montgomery"),
                       class = "sf")

hydrology_list$`Houston, Texas` <- bind_rows(hou_tmp1) %>% 
  st_transform(sdg_cities_list$`Houston, Texas`$proj) %>% 
  st_intersection(context_bbox_list$`Houston, Texas`) %>% 
  st_buffer(0)

# San Francisco
context_bbox_list$`San Francisco, California` <- bbox_sfs$`San Francisco, California` %>% 
  st_buffer(context_buffer_size) %>% 
  st_make_grid(n = 1) %>% 
  st_sf()

sf_tmp1 <- area_water(state = "California",
              county = c("San Francisco"),
              class = "sf")
sf_tmp2 <- area_water(state = "California",
                      county = c("Marin"),
                      class = "sf")
sf_tmp3 <- area_water(state = "California",
                      county = c("San Mateo"),
                      class = "sf")
sf_tmp4 <- area_water(state = "California",
                      county = c("Contra Costa"),
                      class = "sf")
sf_tmp5 <- area_water(state = "California",
                      county = c("Alameda"),
                      class = "sf") 

hydrology_list$`San Francisco, California` <- bind_rows(sf_tmp1, sf_tmp2, sf_tmp3, sf_tmp4, sf_tmp5) %>% 
  st_transform(sdg_cities_list$`San Francisco, California`$proj) %>% 
  st_intersection(context_bbox_list$`San Francisco, California`) %>% 
  st_buffer(0)

## 2b. ----
# highways <- primary_roads(year = 2018,
#                           class = "sf") 

road_list <- vector("list", length(sdg_cities_list)) %>% 
  set_names(names(sdg_cities_list))

# Baltimore
balt_tmp <- primary_secondary_roads(state = "Maryland",
                                                                  year = 2018,
                                                                  class = "sf") %>%  
  st_transform(sdg_cities_list$`Baltimore, Maryland`$proj) %>% 
  st_intersection(context_bbox_list$`Baltimore, Maryland`) %>% 
  st_make_valid() %>% 
  filter(st_is(., c("LINESTRING", "MULTILINESTRING")))
  # filter(RTTYP %in% c("U", "I", "S"))

balt_arterials <- st_read("https://opendata.arcgis.com/datasets/a3d1259ebdfc430cb2a3918b2dc87b64_0.geojson") %>% 
  filter(SHA_CLASS %in% c("MART",
                          "PART")) %>% 
  st_transform(sdg_cities_list$`Baltimore, Maryland`$proj) %>% 
  mutate(RTTYP = "M") %>% 
  dplyr::select(FULLNAME = FULLNAME, RTTYP, geometry)

road_list$`Baltimore, Maryland` <- bind_rows(balt_tmp,
                                             balt_arterials)

# Minneapolis
mpls_tmp <- primary_secondary_roads(state = "Minnesota",
                                    year = 2018,
                                    class = "sf") %>%  
  st_transform(sdg_cities_list$`Minneapolis, Minnesota`$proj) %>% 
  st_intersection(context_bbox_list$`Minneapolis, Minnesota`) %>% 
  st_make_valid() %>% 
  filter(st_is(., c("LINESTRING", "MULTILINESTRING"))) 
  # filter(RTTYP %in% c("U", "I", "S"))

mpls_arterials <- st_read("https://opendata.arcgis.com/datasets/ec28f7083c9346bf8a9fe44f8546c055_0.geojson") %>%  
  st_zm(drop = TRUE) %>% 
  st_transform(sdg_cities_list$`Minneapolis, Minnesota`$proj) %>% 
  filter(STREET_TYPE %in% c("Industrial",
                            "Commerce",
                            "Neighborhood",
                            "Community"
  )) %>% 
  # st_intersection(bbox_sfs$`Minneapolis, Minnesota`) %>% 
  mutate(RTTYP = "M") %>% 
  dplyr::select(FULLNAME = STREET_T_NAME, RTTYP, geometry)

road_list$`Minneapolis, Minnesota` <- bind_rows(mpls_tmp,
                                                mpls_arterials)

# New Orleans
nola_tmp <- primary_secondary_roads(state = "Louisiana",
                                    year = 2018,
                                    class = "sf") %>%  
  st_transform(sdg_cities_list$`New Orleans, Louisiana`$proj) %>% 
  st_intersection(context_bbox_list$`New Orleans, Louisiana`) %>% 
  st_make_valid() %>% 
  filter(st_is(., c("LINESTRING", "MULTILINESTRING"))) 
  # filter(RTTYP %in% c("U", "I", "S"))

nola_arterials <- st_read("https://opendata.arcgis.com/datasets/bf8f32f8203247b9a6d982e145e5c3da_0.geojson") %>%  
  st_transform(sdg_cities_list$`New Orleans, Louisiana`$proj) %>% 
  st_intersection(context_bbox_list$`New Orleans, Louisiana`) %>% 
  filter(ROADCLASS %in% c("Major Arterial")) %>% 
  mutate(RTTYP = "M") %>% 
  dplyr::select(FULLNAME = FULLNAME, RTTYP, geometry)
  
road_list$`New Orleans, Louisiana` <- bind_rows(nola_tmp,
                                                nola_arterials)

# Philadelphia
phl_tmp1 <- primary_secondary_roads(state = "Pennsylvania",
                                    year = 2018,
                                    class = "sf")
phl_tmp2 <- primary_secondary_roads(state = "New Jersey",
                                    year = 2018,
                                    class = "sf")

road_list$`Philadelphia, Pennsylvania` <- bind_rows(phl_tmp1, phl_tmp2) %>%  
  st_transform(sdg_cities_list$`Philadelphia, Pennsylvania`$proj) %>% 
  st_intersection(context_bbox_list$`Philadelphia, Pennsylvania`) %>% 
  st_make_valid() %>% 
  filter(st_is(., c("LINESTRING", "MULTILINESTRING"))) 
# filter(RTTYP %in% c("U", "I", "S"))

# Houston
hou_tmp <- primary_secondary_roads(state = "Texas",
                                   year = 2018,
                                   class = "sf") %>%  
  st_transform(sdg_cities_list$`Houston, Texas`$proj) %>% 
  st_intersection(context_bbox_list$`Houston, Texas`) %>% 
  st_make_valid() %>% 
  filter(st_is(., c("LINESTRING", "MULTILINESTRING"))) 
  # filter(RTTYP %in% c("U", "I", "S"))

hou_arterials <-  st_read("~objects/10/open_space_openData/Houston/COH_ROADS_-_PDD.shp") %>% 
  st_transform(sdg_cities_list$`Houston, Texas`$proj) %>% 
  filter(ROAD_CLASS  %in% c("MAJOR"
                            # "URBAN FREEWAY",
                            # "URBAN"
  )) %>% 
  st_intersection(context_bbox_list$`Houston, Texas`) %>% 
  mutate(RTTYP = "M") %>% 
  dplyr::select(FULLNAME = NAME, RTTYP, geometry)

road_list$`Houston, Texas` <- bind_rows(hou_tmp,
                                        hou_arterials)
  
# San Francisco
sf_tmp <- primary_secondary_roads(state = "California",
                                  year = 2018,
                                  class = "sf") %>%  
  st_transform(sdg_cities_list$`San Francisco, California`$proj) %>% 
  st_intersection(context_bbox_list$`San Francisco, California`) %>% 
  st_make_valid() %>% 
  filter(st_is(., c("LINESTRING", "MULTILINESTRING"))) %>% 
  dplyr::select(FULLNAME, RTTYP, geometry)
  # filter(RTTYP %in% c("U", "I", "S"))

sf_arterials <- st_read("https://data.sfgov.org/api/geospatial/gnsq-9x5h?method=export&format=GeoJSON") %>%  
  st_transform(sdg_cities_list$`San Francisco, California`$proj) %>% 
  mutate(RTTYP = "M") %>% 
  dplyr::select(FULLNAME = streetname, RTTYP, geometry)

road_list$`San Francisco, California` <- bind_rows(sf_tmp, sf_arterials)

# road_list$`San Francisco, California` <- roads(state = "California",
#                                                county = "San Francisco",
#                                                year = 2018,
#                                                class = "sf") %>%  
#   st_transform(sdg_cities_list$`San Francisco, California`$proj) %>% 
#   st_intersection(bbox_sfs$`San Francisco, California`) %>% 
#   st_make_valid() %>% 
#   filter(st_is(., c("LINESTRING", "MULTILINESTRING"))) 
# filter(RTTYP %in% c("U", "I", "S"))

# 2c. ----
dryLand_list <- vector("list", length(sdg_cities_list)) %>% 
  set_names(names(sdg_cities_list))

# Baltimore
dryLand_list$`Baltimore, Maryland` <- counties(state = "Maryland",
                                                  year = 2018,
                                                  class = "sf") %>%  
  st_transform(sdg_cities_list$`Baltimore, Maryland`$proj) %>% 
  st_intersection(context_bbox_list$`Baltimore, Maryland`)

# Minneapolis
dryLand_list$`Minneapolis, Minnesota` <- counties(state = "Minnesota",
                                            year = 2018,
                                            class = "sf") %>%  
  st_transform(sdg_cities_list$`Minneapolis, Minnesota`$proj) %>% 
  st_intersection(context_bbox_list$`Minneapolis, Minnesota`)

# New Orleans
dryLand_list$`New Orleans, Louisiana` <- counties(state = "Louisiana",
                                               year = 2018,
                                               class = "sf") %>%  
  st_transform(sdg_cities_list$`New Orleans, Louisiana`$proj) %>% 
  st_intersection(context_bbox_list$`New Orleans, Louisiana`)

# Philadelphia
phl_tmp1 <- counties(state = "Pennsylvania",
                     year = 2018,
                     class = "sf")
phl_tmp2 <- counties(state = "New Jersey",
                     year = 2018,
                     class = "sf")

dryLand_list$`Philadelphia, Pennsylvania` <- bind_rows(phl_tmp1, phl_tmp2) %>%  
  st_transform(sdg_cities_list$`Philadelphia, Pennsylvania`$proj) %>% 
  st_intersection(context_bbox_list$`Philadelphia, Pennsylvania`)

# Houston
dryLand_list$`Houston, Texas` <- counties(state = "Texas",
                                               year = 2018,
                                               class = "sf") %>%  
  st_transform(sdg_cities_list$`Houston, Texas`$proj) %>% 
  st_intersection(context_bbox_list$`Houston, Texas`)

# San Francisco
dryLand_list$`San Francisco, California` <- counties(state = "California",
                                                  year = 2018,
                                                  class = "sf") %>%  
  st_transform(sdg_cities_list$`San Francisco, California`$proj) %>% 
  st_intersection(context_bbox_list$`San Francisco, California`)

## 2a. Export as rds ----
saveRDS(hydrology_list,
        "~objects/10/14_hydrology_list.rds")
saveRDS(road_list,
        "~objects/10/14_road_list.rds")
saveRDS(dryLand_list,
        "~objects/10/14_dryLand_list.rds")
