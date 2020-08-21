##########################################################################
# This script assigns any variables used when collecting OSM data for both SDGs:
# 1. Define the geographic areas for the cities of interest and adds them to a list bbox_sfs
# 2. Bounding boxes for each sdg city for querying OSM features in a list bbox_mats
#
# Exports: 
# 1. 
# 
# To-do:
#
##########################################################################

## 1. ----
# sf bboxes for trimming OSM features
bbox_sfs <- vector("list",
                   length(sdg_cities_list)) %>% 
  set_names(names(sdg_cities_list))

# Baltimore
# bbox_sfs$`Baltimore, Maryland` <- places("Maryland",
#                year = 2018,
#                class = "sf",
#                cb = TRUE) %>% 
#   filter(NAME == "Baltimore") %>% 
#   st_transform(sdg_cities_list$`Baltimore, Maryland`$proj) %>% 
#   st_cast("POLYGON")

bbox_sfs$`Baltimore, Maryland` <- counties("Maryland", class = "sf") %>% 
  filter(NAMELSAD == "Baltimore city") %>% 
  st_transform(sdg_cities_list$`Baltimore, Maryland`$proj) %>% 
  st_cast("POLYGON")

# Minneapolis
bbox_sfs$`Minneapolis, Minnesota` <- places("Minnesota",
               year = 2018,
               class = "sf",
               cb = TRUE) %>% 
  filter(NAME == "Minneapolis") %>% 
  st_transform(sdg_cities_list$`Minneapolis, Minnesota`$proj) %>% 
  st_cast("POLYGON")

# New Orleans
bbox_sfs$`New Orleans, Louisiana` <- places("Louisiana",
               year = 2018,
               class = "sf",
               cb = TRUE) %>% 
  filter(NAME == "New Orleans") %>% 
  st_transform(sdg_cities_list$`New Orleans, Louisiana`$proj) %>% 
  st_cast("POLYGON")

# Philly
bbox_sfs$`Philadelphia, Pennsylvania` <- places("Pennsylvania",
               year = 2018,
               class = "sf",
               cb = TRUE) %>% 
  filter(NAME == "Philadelphia") %>% 
  st_transform(sdg_cities_list$`Philadelphia, Pennsylvania`$proj) %>% 
  st_cast("POLYGON")

# Houston
bbox_sfs$`Houston, Texas` <- places("Texas",
              year = 2018,
              class = "sf",
              cb = TRUE) %>% 
  filter(NAME == "Houston") %>% 
  st_transform(sdg_cities_list$`Houston, Texas`$proj) %>% 
  st_cast("POLYGON")

# San Francisco
bbox_sfs$`San Francisco, California` <- places("California",
              year = 2018,
              class = "sf",
              cb = TRUE) %>% 
  filter(NAME == "San Francisco") %>% 
  st_transform(sdg_cities_list$`San Francisco, California`$proj) %>% 
  st_cast("POLYGON") %>% 
  rownames_to_column() %>% 
  filter(rowname %in% c("1.1", "1.3")) %>% 
  dplyr::select(-rowname)

# Pennsylvania
bbox_sfs$Pennsylvania <- states(year = 2018,
             class = "sf",
             cb = TRUE) %>% 
  filter(NAME == "Pennsylvania") %>% 
  st_transform(sdg_cities_list$Pennsylvania$proj) %>% 
  st_cast("POLYGON")

## 2. ---- 
# matrix bboxes for querying OSM features
bbox_mats <- map(bbox_sfs,
                 ~ .x %>% 
                   st_transform(4326) %>% 
                   base_map_bb() %>% 
                   opq(timeout = 900, # 900 seconds
                       memsize = 1073741824 * 3)) # 3 GB
