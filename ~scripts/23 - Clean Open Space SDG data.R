##########################################################################
# This script:
# 1. Cleans the open space data sets
#   (a) OSM
#   (b) Open data
#
# Exports: 
# 1. parks_osmData_combined as 23_parks_osmData_combined.rds: Parks, plazas, and cemeteries unioned for each city
# 2. parks_osmData as 23_parks_osmData.rds: Combined into single dataframes for each city
# 
# To-do:
# 1. 
##########################################################################

## 1(a). ----
parks_osmData_list <- readRDS("~objects/10/13_parks_osmData_list.rds")
parks_openData_list <- readRDS("~objects/10/13_parks_openData_list.rds")

# combine the polygon and multipolygon layers using st_union
parks_osmData_combined <- vector("list", length(parks_openData_list)) %>% 
  set_names(names(parks_openData_list))

for (city in 1:length(parks_openData_list)) {
  
  tmp <- bind_rows(
    parks_osmData_list[[1]][[city]],
    parks_osmData_list[[2]][[city]],
    parks_osmData_list[[3]][[city]]
  ) %>% 
    st_transform(sdg_cities_list[[city]]$proj) %>% 
    st_buffer(0) %>% 
    mutate(parkType = case_when(feature == "cemetery" ~ "Cemetery",
                                feature %in% c("dog_park", 
                                               "park",
                                               "pitch") ~ "Park",
                                feature %in% c("garden",
                                               "nature_reserve") ~ "Reserve/Garden",
                                feature %in% c("playground",
                                               "square") ~ "Playground/Square"))
  
  parks_osmData_combined[[city]] <- tmp
  
}


## 1(b). ----
#nothing to do

## 1(a). Export as rds ----
# saveRDS(parks_osmData_combined,
#         "~objects/20/23_parks_osmData_combined.rds")
