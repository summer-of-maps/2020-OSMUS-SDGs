##########################################################################
# This script assigns any variables used when collecting OSM data for both SDGs:
# 1. Define the geographic areas for the cities of interest
#     (IS THIS NEEDED?)
# 2. Bounding boxes for each sdg city
#
# Exports: 
# 1. 
# 
# To-do:
#
##########################################################################

## 1. ----
# (a) Baltimore
balt <- places("Maryland",
               year = 2018,
               class = "sf",
               cb = TRUE) %>% 
  filter(NAME == "Baltimore") %>% 
  st_transform(4326)

# Minneapolis
mpls <- places("Minnesota",
               year = 2018,
               class = "sf",
               cb = TRUE) %>% 
  filter(NAME == "Minneapolis") %>% 
  st_transform(4326)

## 2. ---- 
bboxes <- map(set_names(sdg_cities$city), # set names adds names to the list output
              ~ getbb(.x) %>% 
                opq(timeout = 900, # 900 seconds
                    memsize = 1073741824 * 3)) # 3 GB
