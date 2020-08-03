##########################################################################
# This script:
# 1. Finds the land area for every city in square miles
# 2. Calculates open space 
#   a. OSM
#   b. Open data
# 3. Find the areas of overlap and separation for OSM and Open Data files
# 4. Calculates percentages and combines into a summary table
#
# Exports: 
# 1. 
# 
# To-do:
#
##########################################################################

## 1. ----
land_list <- map2(bbox_sfs[1:6],
                  hydrology_list[1:6],
                  ~ st_difference(st_union(.x),
                                  st_union(.y)))

landArea_list <- map(land_list, 
                     ~ st_area(.x) %>% 
                       as.numeric() %>% 
                       conv_unit(from = "ft2",
                                 to = "mi2"))

## 2a. ----
parks_osmArea_list <- map(
  parks_osmData_combined,
  ~ st_area(.x) %>% 
    as.numeric() %>%
    conv_unit(from = "ft2",
              to = "mi2") %>% 
    sum())

## 2b. ----
parks_openDataArea_list <- map(
  parks_openData_list,
  ~ st_area(.x) %>% 
    as.numeric() %>%
    conv_unit(from = "ft2",
              to = "mi2") %>% 
    sum())

## 3a. ----
parks_osmData_combined <- readRDS("~objects/20/23_parks_osmData_combined.rds")
parks_openData_list <- readRDS("~objects/10/13_parks_openData_list.rds")

parks_overlap_list <- map2(
  parks_osmData_combined,
  parks_openData_list,
  ~ st_intersection(st_union(.x), 
                    st_union(.y)) %>% 
    st_sf() %>% 
    dplyr::select(geometry) %>% 
    mutate(Source = "Both")
)

parks_osmOnly_list <- map2(
  parks_osmData_combined,
  parks_openData_list,
  ~ st_difference(st_union(.x), 
                  st_union(.y)) %>% 
    st_sf() %>% 
    dplyr::select(geometry) %>%
    mutate(Source = "OSM only")
)

parks_openDataOnly_list <- map2(
  parks_openData_list,
  parks_osmData_combined,
  ~ st_difference(st_union(.x), 
                  st_union(.y)) %>%
    st_sf() %>% 
    dplyr::select(geometry) %>% 
    mutate(Source = "Open Data only")
)

parks_comparison_list <- pmap(
  .l = list(parks_overlap_list, parks_osmOnly_list, parks_openDataOnly_list),
  function (overlap, osm, openData)
    bind_rows(overlap, osm, openData) %>% 
    mutate(area = conv_unit(as.numeric(st_area(.)), 
           from = "ft2", to = "mi2"))
)

## 4. ----
openSpace_results <- pmap_dfr(
  .l = list(parks_osmArea_list,
            parks_openDataArea_list,
            landArea_list,
            parks_comparison_list),
  function(osm, open, land, overlap) {
    
    osm_perc <- osm / land
    open_perc <- open / land
    overlap_area <- overlap$area[1]
    osmOnly_area <- overlap$area[2]
    openDataOnly_area <- overlap$area[3]
    combined_area <- overlap_area + osmOnly_area + openDataOnly_area
    overlap_perc <- overlap_area / combined_area
    overlap_SDG_perc <- combined_area / land
    
    result <- data.frame(OSM_sqMiles = round(osm, 2),
                         OSM_percentage = round(osm_perc, 4),
                         openData_sqMiles = round(open, 2),
                         openData_percentage = round(open_perc, 4),
                         OSM_only_sqMiles = round(osmOnly_area, 2),
                         openData_only_sqMiles = round(openDataOnly_area, 2),
                         overlap_sqMiles = round(overlap_area, 2),
                         overlap_combinedArea_perc = round(overlap_perc, 4),
                         overlap_SDG_percentage = round(overlap_SDG_perc, 4),
                         stringsAsFactors = FALSE)
    
  },
  .id = "City"
)

## 1. Export as rds ----
saveRDS(land_list, 
        file = "~objects/30/33_land_list.rds")
saveRDS(landArea_list, 
        file = "~objects/30/33_landArea_list.rds")

## 2a. Export as rds ----
saveRDS(parks_osmArea_list, 
        file = "~objects/30/33_parks_osmArea_list.rds")

## 2b. Export as rds ----
saveRDS(parks_openDataArea_list, 
        file = "~objects/30/33_parks_openDataArea_list.rds")

## 3. Export as rds ----
saveRDS(parks_overlap_list,
        "~objects/30/33_parks_overlap_list.rds")
saveRDS(parks_osmOnly_list,
        "~objects/30/33_parks_osmOnly_list.rds")
saveRDS(parks_openDataOnly_list,
        "~objects/30/33_parks_openDataOnly_list.rds")
saveRDS(parks_comparison_list,
        "~objects/30/33_parks_comparison_list.rds")

## 4. Export as rds ----
saveRDS(openSpace_results, 
        file = "~objects/30/33_openSpace_results.rds")
