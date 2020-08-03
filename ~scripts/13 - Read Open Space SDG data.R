##########################################################################
# This script:
# 1. Runs the OSM data admin scripts
# 2. Downloads "open space" from OSM
#   (a) parks, dog parks, gardens, nature reserves, ball fields, playgrounds
#   (b) plazas, squares
#   (c) cemeteries
# 3. Download available open data for each city
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
parks <- map2(bbox_mats[1:6], # for querying features
              bbox_sfs[1:6], # for trimming features
              function(mats, sfs) {
                osm_fts <- mats %>%
                add_osm_feature(key = "leisure",
                                value = c("park",
                                          "dog_park",
                                          "garden",
                                          "nature_reserve",
                                          "pitch",
                                          "playground")) %>%
                osmdata_sf() %>% 
                  unique_osmdata()
                
                sfs_proj <- st_transform(sfs, 4326)
                
                poly <- bind_rows(
                  st_make_valid(osm_fts$osm_polygons),
                  st_make_valid(osm_fts$osm_multipolygons)) %>%
                  st_intersection(sfs_proj) %>% 
                  dplyr::select(osm_id, feature = leisure, geometry)
                
                return(poly)
                
                # sfs_proj <- st_transform(sfs, 4326)
                # tmp <- list()
                # for (row in 1:nrow(sfs_proj)) { # loop over rows
                #   tmp[[row]] <- trim_osmdata(
                #     osm_fts,
                #     bb_poly = sfs_proj[row,]
                #   )
                # }
                # reduce(tmp, c)
                }
              )

plazas <- map2(bbox_mats[1:6], # for querying features
              bbox_sfs[1:6], # for trimming features
              function(mats, sfs) {
                osm_fts <- mats %>%
                  add_osm_feature(key = "place",
                                  value = c("square")) %>%
                  osmdata_sf() %>% 
                  unique_osmdata()
                
                sfs_proj <- st_transform(sfs, 4326)
                
                if(!is.null(osm_fts$osm_multipolygons)){
                  
                  poly <- bind_rows(
                    st_make_valid(osm_fts$osm_polygons),
                    st_make_valid(osm_fts$osm_multipolygons)) %>%
                    st_intersection(sfs_proj)
                  
                  } else {
                    
                  poly <- st_make_valid(osm_fts$osm_polygons) %>%
                    st_intersection(sfs_proj)
                  
                  }
                
                poly <- poly %>% 
                  dplyr::select(osm_id, feature = place, geometry)
                
                return(poly)
                
                # sfs_proj <- st_transform(sfs, 4326)
                # tmp <- list()
                # for (row in 1:nrow(sfs_proj)) { # loop over rows
                #   tmp[[row]] <- trim_osmdata(
                #     osm_fts,
                #     bb_poly = sfs_proj[row,]
                #   )
                # }
                # reduce(tmp, c)
              }
              )

cemeteries <- map2(bbox_mats[1:6], # for querying features
               bbox_sfs[1:6], # for trimming features
               function(mats, sfs) {
                 osm_fts <- mats %>%
                   add_osm_feature(key = "landuse",
                                   value = c("cemetery")) %>%
                   osmdata_sf() %>% 
                   unique_osmdata()
                 
                 sfs_proj <- st_transform(sfs, 4326)
                 
                 if(!is.null(osm_fts$osm_multipolygons)){
                   
                   poly <- bind_rows(
                     st_make_valid(osm_fts$osm_polygons),
                     st_make_valid(osm_fts$osm_multipolygons)) %>%
                     st_intersection(sfs_proj)
                   
                 } else {
                   
                   poly <- st_make_valid(osm_fts$osm_polygons) %>%
                     st_intersection(sfs_proj)
                   
                 }
                 
                 poly <- poly %>% 
                   dplyr::select(osm_id, feature = landuse, geometry)
                 
                 return(poly)
                 
                 # sfs_proj <- st_transform(sfs, 4326)
                 # tmp <- list()
                 # for (row in 1:nrow(sfs_proj)) { # loop over rows
                 #   tmp[[row]] <- trim_osmdata(
                 #     osm_fts,
                 #     bb_poly = sfs_proj[row,]
                 #   )
                 # }
                 # reduce(tmp, c)
               }
)

parks_osmData_list <- list(parks = parks, 
                           plazas = plazas, 
                           cemeteries = cemeteries)

## 3. ----
# initialize list
parks_openData_list <- vector("list", length(sdg_cities_list)) %>% 
  set_names(names(sdg_cities_list))

# Baltimore
balt_parks_openData <- st_read("~objects/10/open_space_openData/Baltimore/landuse.shp") %>% 
  st_transform(sdg_cities_list$`Baltimore, Maryland`$proj) %>% 
  filter(LU_2008 %in% c("Cemetery",
                        "Natural Area",
                        "Parks/Recreation")) %>% 
  st_make_valid() %>% 
  st_intersection(bbox_sfs$`Baltimore, Maryland`)

parks_openData_list$`Baltimore, Maryland` <- balt_parks_openData

# Minneapolis
# note: parks is fine, no need to use the land use file
mpls_parks_openData <- st_read("https://opendata.arcgis.com/datasets/a1847c4cc69940f99b46b16e2b4fe7e3_0.geojson") %>% 
  st_transform(sdg_cities_list$`Minneapolis, Minnesota`$proj) %>% 
  st_make_valid() %>% 
  st_intersection(bbox_sfs$`Minneapolis, Minnesota`)

# mpls_LU <- st_read("~objects/~large_files/mpls_landUse/GeneralizedLandUse2010.shp") %>% 
#   st_transform(sdg_cities_list$`Baltimore, Maryland`$proj) %>% 
#   filter(LUSE_DESC %in% c("Park, Recreational, or Preserve")) %>% 
#   st_transform(sdg_cities_list$`Minneapolis, Minnesota`$proj) %>% 
#   .[bbox_sfs$`Minneapolis, Minnesota`,]

parks_openData_list$`Minneapolis, Minnesota` <- mpls_parks_openData

# New Orleans
nola_parks_openData <- st_read("https://opendata.arcgis.com/datasets/3273a5f8334d40838681ff0337eddb8c_0.geojson") %>% 
  st_transform(sdg_cities_list$`New Orleans, Louisiana`$proj) %>% 
  st_make_valid() %>% 
  st_intersection(bbox_sfs$`New Orleans, Louisiana`)

parks_openData_list$`New Orleans, Louisiana` <- nola_parks_openData

# Philadelphia
# This includes Parks and Rec assets only
# phl_parks_openData <- st_read("http://data.phl.opendata.arcgis.com/datasets/4df9250e3d624ea090718e56a9018694_0.geojson") %>% 
#   st_transform(sdg_cities_list$`Philadelphia, Pennsylvania`$proj) %>% 
#   .[bbox_sfs$`Philadelphia, Pennsylvania`,] %>% 
#   filter(PPR_USE %in% c("ATHLETIC",
#                         "COMMUNITY_PARK",
#                         "GARDEN",
#                         "GOLF",
#                         "GREENWAY_PARKWAY",
#                         "MINI_PARK",
#                         "MULTI_USE",
#                         "NEIGHBORHOOD_PARK",
#                         "RECREATION_SITE",
#                         "SQUARE_PLAZA",
#                         "YOUTH_TOT_PLAY_AREAS"))

# use this
phl_LU <- st_read("http://data-phl.opendata.arcgis.com/datasets/e433504739bd41049de5d8f4a22d34ba_0.geojson") %>% 
  filter(C_DIG2 %in% c(62, # active recreation, e.g. golf courses 
                       71, # parks
                       72  # cemeteries
  )) %>% 
  st_transform(sdg_cities_list$`Philadelphia, Pennsylvania`$proj) %>% 
  st_intersection(bbox_sfs$`Philadelphia, Pennsylvania`)

parks_openData_list$`Philadelphia, Pennsylvania` <- phl_LU

# Houston
hou_parks_openData <- st_read("https://opendata.arcgis.com/datasets/0511438736024780bbb29e9f36c93b86_14.geojson") %>% 
  st_transform(sdg_cities_list$`Houston, Texas`$proj) %>% 
  st_make_valid() %>% 
  st_intersection(bbox_sfs$`Houston, Texas`)

# this doesn't include land use classifications
# hou_LU <- st_read("~objects/10/open_space_openData/Houston/COH_LAND_USE.shp") %>% 
#   st_transform(sdg_cities_list$`Houston, Texas`$proj) %>% 
#   .[bbox_sfs$`Houston, Texas`,]

parks_openData_list$`Houston, Texas` <- hou_parks_openData

# San Francisco
# parks1 <- st_read("~objects/10/open_space_openData/San Francisco/RPD_Parks.shp") %>% 
#   st_transform(sdg_cities_list$`San Francisco, California`$proj) %>% 
#   .[bbox_sfs$`San Francisco, California`,]

sf_parks_openData <- st_read("https://data.sfgov.org/resource/gtr9-ntp6.geojson") %>% 
  filter(!propertytype %in% c("SF Public Library property",
                              "War Memorial & Performing Arts Center",
                              "Real Estate/Administrative Services property")) %>% 
  st_transform(sdg_cities_list$`San Francisco, California`$proj) %>% 
  st_make_valid() %>% 
  st_intersection(bbox_sfs$`San Francisco, California`)

parks_openData_list$`San Francisco, California` <- sf_parks_openData
parks_openData_list <- map(parks_openData_list[1:6],
                           st_make_valid)

## 1. Export as rds ----
saveRDS(parks_osmData_list,
        "~objects/10/13_parks_osmData_list.rds")

## 2. Exports as rds ----
saveRDS(parks_openData_list,
        "~objects/10/13_parks_openData_list.rds")
