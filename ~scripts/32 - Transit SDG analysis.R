##########################################################################
# This script:
# 1. Count bus stops per grid cell
# 2. Count bus stops per buffer around grid cell centroid
#     See markdown for info on how I determined buffer size.
# 3. Finds the isometric polygons (0.5km road distance) for each city
#     Note: requires OSRM local server
# 4. Combines the isometric polygons for each city into a list and 
#     count bus stops within 0.5km of road distance
#   (a) OSM data
#   (b) OpenData
# 5. Snap the cell centroids to the nearest roads for mapping purposes
#
# Exports: 
# 1. 
# 
# To-do:
#
##########################################################################

## 1. ----
# How many transit stops inside each grid cell?
transit_openData_list <- readRDS("~objects/10/12_transit_openData_list.rds")
grids <- readRDS("~objects/20/21_grids.RDS")
grids <- pmap(.l = list(grids[1:6],
                      pubTrans[1:6],
                      transit_openData_list[1:6]),
             function(grid, OSM, GTFS) grid %>% 
               dplyr::select(cell_ID = Group.1,
                             TotPop,
                             geometry) %>% 
               mutate(cell_ID = as.character(cell_ID),
                      transitStops_OSM_inCell = lengths(st_intersects(.,
                                                          OSM)),
                      transitStops_GTFS_inCell = lengths(st_intersects(.,
                                                                       GTFS))))

# population
OSM_grids_summary <- map(grids,
                     ~ .x %>% 
                       st_drop_geometry() %>% 
                       mutate(hasOSMTransit = ifelse(transitStops_OSM_inCell > 0, "yes", "no")) %>% 
                       group_by(hasOSMTransit) %>% 
                       summarize(OSM_TotPop = sum(TotPop)) %>% 
                       mutate(OSM_prop = OSM_TotPop / sum(OSM_TotPop)))

GTFS_grids_summary <- map(grids,
                         ~ .x %>% 
                           st_drop_geometry() %>% 
                           mutate(hasGTFSTransit = ifelse(transitStops_GTFS_inCell > 0, "yes", "no")) %>% 
                           group_by(hasGTFSTransit) %>% 
                           summarize(GTFS_TotPop = sum(TotPop)) %>% 
                           mutate(GTFS_prop = GTFS_TotPop / sum(GTFS_TotPop)))

grids_summary <- map2(OSM_grids_summary,
                      GTFS_grids_summary,
                      ~ cbind(.x, .y))

## 2. ----
# How many within a 0.25mi buffer of the center of the grid cell?
buffer_dist <- conv_unit(0.25,
      # see note in SDG Analysis markdown. Use the below if hex cell size changes
                         # 0.5 + 
                         #   conv_unit(find_hex_cellsize(cell_size), from = "ft", to = "km") / 4,
                         from = "mi",
                         to = "ft")

buffers <- map2(grids,
                pubTrans,
                ~ .x %>% 
                  st_centroid() %>% 
                  st_buffer(dist = buffer_dist) %>% 
                  mutate(transitStops = lengths(st_intersects(.,
                                                              .y))))

buffers_summary <- map(buffers,
                       ~ .x %>% 
                         st_drop_geometry() %>% 
                         mutate(hasTransit = ifelse(transitStops > 0, "yes", "no")) %>% 
                         group_by(hasTransit) %>% 
                         summarize(TotPop = sum(TotPop)) %>% 
                         mutate(prop = TotPop / sum(TotPop)))

## 3. ----
# how many within a 0.5km walk by road
# initialize OSRM server using Docker for each state
# point to the server
options(osrm.server = "http://localhost:5000/", osrm.profile = "walk")

# Maryland
# Baltimore
centroids <- st_centroid(grids$Baltimore)

# (a)
balt_isometric_list <- list()
for (row in seq_len(nrow(centroids))) {
  tryCatch({
    balt_isometric_list[[row]] <- osrmIsometric( # find isochrones
    centroids[row,],
    breaks = 500, # 500-meter walk
    exclude = NULL, # don't exclude any road types
    res = 50, # resolution
    returnclass = "sf"
  )}, error = function(e){})
}

balt_isometric <- map(balt_isometric_list,
                 ~ as.data.frame(.x)) %>% 
  bind_rows(.id = "cell_ID") %>% 
  st_as_sf()

# Minnesota
# Minneapolis
centroids <- st_centroid(grids$Minneapolis)

mpls_isometric_list <- list()
for (row in seq_len(nrow(centroids))) {
  tryCatch({
    mpls_isometric_list[[row]] <- osrmIsometric( # find isochrones
      centroids[row,],
      breaks = 500, # 500-meter walk
      exclude = NULL, # don't exclude any road types
      res = 50, # resolution
      returnclass = "sf"
    )}, error = function(e){})
}

mpls_isometric <- map(mpls_isometric_list,
                      ~ as.data.frame(.x)) %>% 
  bind_rows(.id = "cell_ID") %>% 
  st_as_sf()

# Louisiana
# New Orleans
centroids <- st_centroid(grids$`New Orleans`)

nola_isometric_list <- list()
for (row in seq_len(nrow(centroids))) {
  tryCatch({
    nola_isometric_list[[row]] <- osrmIsometric( # find isochrones
      centroids[row,],
      breaks = 500, # 500-meter walk
      exclude = NULL, # don't exclude any road types
      res = 50, # resolution
      returnclass = "sf"
    )}, error = function(e){})
}

nola_isometric <- map(nola_isometric_list,
                      ~ as.data.frame(.x)) %>% 
  bind_rows(.id = "cell_ID") %>% 
  st_as_sf()

# Pennsylvania
# Philadelphia
centroids <- st_centroid(grids$Philadelphia)

phl_isometric_list <- list()
for (row in seq_len(nrow(centroids))) {
  print(paste(row,
              "out of",
              nrow(centroids)))
  tryCatch({
    phl_isometric_list[[row]] <- osrmIsometric( # find isochrones
      centroids[row,],
      breaks = 500, # 500-meter walk
      exclude = NULL, # don't exclude any road types
      res = 30, # resolution
      returnclass = "sf"
    )}, error = function(e){})
}

phl_isometric <- phl_isometric_list %>% 
  bind_rows(.id = "cell_ID") 

# Texas
# Houston
centroids <- st_centroid(grids$Houston)

hou_isometric_list <- list()
for (row in seq_len(nrow(centroids))) {
  print(paste(row,
              "out of",
              nrow(centroids)))
  tryCatch({
    hou_isometric_list[[row]] <- osrmIsometric( # find isochrones
      centroids[row,],
      breaks = 500, # 500-meter walk
      exclude = NULL, # don't exclude any road types
      res = 30, # resolution
      returnclass = "sf"
    )}, error = function(e){})
}

hou_isometric <- hou_isometric_list %>% 
  bind_rows(.id = "cell_ID") 

# California
# San Francisco
centroids <- st_centroid(grids$`San Francisco`)

sf_isometric_list <- list()
for (row in seq_len(nrow(centroids))) {
  print(paste(row,
              "out of",
              nrow(centroids)))
  tryCatch({
    sf_isometric_list[[row]] <- osrmIsometric( # find isochrones
      centroids[row,],
      breaks = 500, # 500-meter walk
      exclude = NULL, # don't exclude any road types
      res = 30, # resolution
      returnclass = "sf"
    )}, error = function(e){})
}

sf_isometric <- sf_isometric_list %>% 
  bind_rows(.id = "cell_ID") 

## 4. ----
balt_isometric <- readRDS("~objects/30/32_balt_isometric.RDS")
mpls_isometric <- readRDS("~objects/30/32_mpls_isometric.RDS")
nola_isometric <- readRDS("~objects/30/32_nola_isometric.RDS")
phl_isometric <- readRDS("~objects/30/32_phl_isometric.RDS")
hou_isometric <- readRDS("~objects/30/32_hou_isometric.RDS")
sf_isometric <- readRDS("~objects/30/32_sf_isometric.RDS")

# isometric_list <- readRDS("~objects/30/32_isometric_list.RDS")

tmp_list <- list("Baltimore" = balt_isometric,
                 "Minneapolis" = mpls_isometric,
                 "New Orleans" = nola_isometric,
                 "Philadelphia" = phl_isometric,
                 "Houston" = hou_isometric,
                 "San Francisco" = sf_isometric)

isometric_list <- pmap(
  .l = list(
    tmp_list,
    pubTrans[c(1:6)],
    transit_openData_list[c(1:6)]),
  function(iso, OSM, GTFS){
    iso %>%
      mutate(transitStops_OSM = lengths(st_intersects(.,
                                                      OSM)),
             transitStops_GTFS = lengths(st_intersects(.,
                                                       GTFS))
                )
         }
       ) %>% 
  map2(.x = .,
       .y = grids[c(1:6)],
       ~ .x %>% 
         right_join(x = st_drop_geometry(.),
                   y = .y,  # join to the grid cells
                   by = "cell_ID") %>% 
         st_as_sf() %>% 
         dplyr::select(cell_ID,
                       TotPop,
                       inCell_OSM = transitStops_OSM_inCell,
                       inCell_GTFS = transitStops_GTFS_inCell,
                       walkshed_OSM = transitStops_OSM,
                       walkshed_GTFS = transitStops_GTFS,
                       geometry) %>% 
         mutate(area_sqmi = conv_unit(as.numeric(st_area(.)),
                                      from = "ft2",
                                      to = "mi2"),
                popDens_sqmi = TotPop / area_sqmi,
                walkshed_OSM_denseEnough = ifelse(popDens_sqmi > 3000, 
                                                  walkshed_OSM, 
                                                  NA),
                walkshed_GTFS_denseEnough = ifelse(popDens_sqmi > 3000, 
                                                   walkshed_GTFS, 
                                                  NA)))

isometric_list <- map(
  isometric_list,
  ~ .x %>% 
    mutate(overlap = case_when(is.na(walkshed_OSM_denseEnough) ~ NA_character_,
                               walkshed_OSM_denseEnough  == 0 &
                                 walkshed_GTFS_denseEnough == 0 ~ "Neither",
                               walkshed_OSM_denseEnough  > 0 &
                                 walkshed_GTFS_denseEnough > 0 ~ "Both",
                               walkshed_OSM_denseEnough  > 0 &
                                 walkshed_GTFS_denseEnough == 0 ~ "OSM only",
                               walkshed_OSM_denseEnough  == 0 &
                                 walkshed_GTFS_denseEnough > 0 ~ "GTFS only",
                               TRUE ~ NA_character_)))

# for all cells
OSM_isometric_summary_allCells <- map(isometric_list,
                             ~ .x %>% 
                               st_drop_geometry() %>% 
                               mutate(hasTransit_OSM = ifelse(walkshed_OSM > 0, "yes", "no")) %>% 
                               group_by(hasTransit_OSM) %>% 
                               filter(!is.na(hasTransit_OSM)) %>% 
                               summarize(OSM_population = sum(TotPop, na.rm = T)) %>% 
                               mutate(OSM_prop = OSM_population / sum(OSM_population)))

GTFS_isometric_summary_allCells <- map(isometric_list,
                              ~ .x %>% 
                                st_drop_geometry() %>% 
                                mutate(hasTransit_GTFS = ifelse(walkshed_GTFS > 0, "yes", "no")) %>% 
                                filter(!is.na(hasTransit_GTFS)) %>% 
                                group_by(hasTransit_GTFS) %>% 
                                summarize(GTFS_population = sum(TotPop, na.rm = T)) %>% 
                                mutate(GTFS_prop = GTFS_population / sum(GTFS_population)))

overlap_isometric_summary_allCells <- map(isometric_list,
                                 ~ .x %>% 
                                   st_drop_geometry() %>% 
                                   mutate(hasTransit_both = ifelse(walkshed_GTFS > 0 |
                                                                     walkshed_OSM > 0, "yes", "no")) %>% 
                                   filter(!is.na(hasTransit_both)) %>% 
                                   group_by(hasTransit_both) %>% 
                                   summarize(overlap_population = sum(TotPop, na.rm = T)) %>% 
                                   mutate(overlap_prop = overlap_population / sum(overlap_population)))

isometric_summary_allCells <- pmap(.l = list(OSM_isometric_summary_allCells,
                                             GTFS_isometric_summary_allCells,
                                             overlap_isometric_summary_allCells),
                          function(a, b, c) cbind(a, b, c) %>% 
                            dplyr::select(OSM_population,
                                          everything()))

isometric_summary_allCells$Baltimore
isometric_summary_allCells$Minneapolis
isometric_summary_allCells$`New Orleans`
isometric_summary_allCells$Houston
isometric_summary_allCells$Philadelphia
isometric_summary_allCells$`San Francisco`

# for those with minimum population density only
isometric_summary <- readRDS("~objects/30/32_isometric_summary.RDS")

OSM_isometric_summary <- map(isometric_list,
                         ~ .x %>% 
                           st_drop_geometry() %>% 
                           mutate(densePop = ifelse(!is.na(walkshed_OSM_denseEnough),
                                                    TotPop,
                                                    NA),
                                  hasTransit_OSM = ifelse(walkshed_OSM_denseEnough > 0, "yes", "no")) %>% 
                           group_by(hasTransit_OSM) %>% 
                           filter(!is.na(hasTransit_OSM)) %>% 
                           summarize(OSM_population = sum(densePop, na.rm = T)) %>% 
                           mutate(OSM_prop = OSM_population / sum(OSM_population)))

GTFS_isometric_summary <- map(isometric_list,
                             ~ .x %>% 
                               st_drop_geometry() %>% 
                               mutate(densePop = ifelse(!is.na(walkshed_GTFS_denseEnough),
                                                        TotPop,
                                                        NA),
                                      hasTransit_GTFS = ifelse(walkshed_GTFS_denseEnough > 0, "yes", "no")) %>% 
                               filter(!is.na(hasTransit_GTFS)) %>% 
                               group_by(hasTransit_GTFS) %>% 
                               summarize(GTFS_population = sum(densePop, na.rm = T)) %>% 
                               mutate(GTFS_prop = GTFS_population / sum(GTFS_population)))

overlap_isometric_summary <- map(isometric_list,
                              ~ .x %>% 
                                st_drop_geometry() %>% 
                                mutate(densePop = ifelse(!is.na(walkshed_GTFS_denseEnough),
                                                         TotPop,
                                                         NA),
                                       hasTransit_both = ifelse(walkshed_GTFS_denseEnough > 0 |
                                                                  walkshed_OSM_denseEnough > 0, "yes", "no")) %>% 
                                filter(!is.na(hasTransit_both)) %>% 
                                group_by(hasTransit_both) %>% 
                                summarize(overlap_population = sum(densePop, na.rm = T)) %>% 
                                mutate(overlap_prop = overlap_population / sum(overlap_population)))

isometric_summary <- pmap(.l = list(OSM_isometric_summary,
                                    GTFS_isometric_summary,
                                    overlap_isometric_summary),
                          function(a, b, c) cbind(a, b, c) %>% 
                            dplyr::select(OSM_population,
                                          everything()))

isometric_summary$Baltimore
isometric_summary$Minneapolis
isometric_summary$`New Orleans`
isometric_summary$Houston
isometric_summary$Philadelphia
isometric_summary$`San Francisco`

## 5. ----
balt_centroids_snap <- st_snap_points(balt_centroids,
                                      walk_roads,
                                      max_dist = 1000)

## 1. Export as RDS ----
# saveRDS(grids,
#         "~objects/30/32_grids.RDS")
# saveRDS(buffers,
#         "~objects/~large_files/32_buffers.RDS")

## 2. Export as RDS ----
# saveRDS(balt_isometric,
#         "~objects/30/32_balt_isometric.RDS")
# saveRDS(mpls_isometric,
#         "~objects/30/32_mpls_isometric.RDS")
# saveRDS(nola_isometric,
#         "~objects/30/32_nola_isometric.RDS")
# saveRDS(phl_isometric,
#         "~objects/30/32_phl_isometric.RDS")
# saveRDS(hou_isometric,
#         "~objects/30/32_hou_isometric.RDS")
# saveRDS(sf_isometric,
#         "~objects/30/32_sf_isometric.RDS")
# saveRDS(isometric_list,
#         "~objects/30/32_isometric_list.RDS")

## 3. Export as RDS ----
# saveRDS(grids_summary,
#         "~objects/30/32_grids_summary.RDS")
# saveRDS(buffers_summary,
#         "~objects/30/32_buffers_summary.RDS")
# saveRDS(isometric_summary_allCells,
#         "~objects/30/32_isometric_summary_allCells.RDS")
# saveRDS(isometric_summary,
#         "~objects/30/32_isometric_summary.RDS")