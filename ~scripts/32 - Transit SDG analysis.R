##########################################################################
# This script:
# 1. Count bus stops per grid cell
# 2. Count bus stops per buffer around grid cell centroid
#     See markdown for info on how I determined buffer size.
# 3. Finds the isometric polygons (0.5km road distance) for each city
#     Note: requires OSRM local server
# 4. Combines the isometric polygons for each city into a list and 
#     count bus stops within 0.5km of road distance
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
# grids <- readRDS("~objects/21_grids.RDS")
grids <- map2(grids,
              pubTrans,
             ~ .x %>% 
               mutate(transitStops = lengths(st_intersects(.,
                                                           .y))))

# population
grids_summary <- map(grids,
                     ~ .x %>% 
                       st_drop_geometry() %>% 
                       mutate(hasTransit = ifelse(transitStops > 0, "yes", "no")) %>% 
                       group_by(hasTransit) %>% 
                       summarize(TotPop = sum(TotPop)) %>% 
                       mutate(prop = TotPop / sum(TotPop)))

grids_summary$Baltimore
grids_summary$Minneapolis
grids_summary$`New Orleans`

## 2. ----
# How many within a 0.5km buffer of the center of the grid cell?
buffer_dist <- conv_unit(0.5,
      # see note in SDG Analysis markdown. Use the below if hex cell size changes
                         # 0.5 + 
                         #   conv_unit(find_hex_cellsize(cell_size), from = "ft", to = "km") / 4,
                         from = "km",
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

buffers_summary$Baltimore
buffers_summary$Minneapolis
buffers_summary$`New Orleans`

## 3. ----
# how many within a 0.5km walk by road
# initialize OSRM server using Docker for each state
# point to the server
options(osrm.server = "http://localhost:5000/", osrm.profile = "walk")

# Maryland
# Baltimore
centroids <- st_centroid(grids$Baltimore)

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

## 4. ----
isometric_list <- list("Baltimore" = balt_isometric,
                       "Minneapolis" = mpls_isometric,
                       "New Orleans" = nola_isometric) %>% 
  map2(.x = .,
       .y = pubTrans,
       ~ .x %>% 
         mutate(transitStops = lengths(st_intersects(.,
                                                     .y)))) %>% 
  map2(.x = .,
       .y = grids,
       ~ .x %>% 
         left_join(x = .,
                   y = st_drop_geometry(.y) %>% # join to the grid cells
                     mutate(Group.1 = as.character(Group.1)) %>% 
                     dplyr::select(cell_ID = Group.1,
                                   TotPop),
                   by = "cell_ID"))

isometric_summary <- map(isometric_list,
                       ~ .x %>% 
                         st_drop_geometry() %>% 
                         mutate(hasTransit = ifelse(transitStops > 0, "yes", "no")) %>% 
                         group_by(hasTransit) %>% 
                         summarize(TotPop = sum(TotPop)) %>% 
                         mutate(prop = TotPop / sum(TotPop)))

isometric_summary$Baltimore
isometric_summary$Minneapolis
isometric_summary$`New Orleans`

## 5. ----
balt_centroids_snap <- st_snap_points(balt_centroids,
                                      walk_roads,
                                      max_dist = 1000)

tm_shape(isochrone_test2) + tm_polygons(col = "green", alpha = 0.5) +
  tm_shape(balt_centroids) + tm_dots(col = "blue") +
  tm_shape(buses$Baltimore) + tm_dots(col = "orange")

## 1. Export as RDS ----
# saveRDS(grids,
#         "~objects/30/32_grids.RDS")
# saveRDS(buffers,
#         "~objects/30/32_buffers.RDS")

## 2. Export as RDS ----
# saveRDS(balt_isometric,
#         "~objects/30/32_balt_isometric.RDS")
# saveRDS(mpls_isometric,
#         "~objects/30/32_mpls_isometric.RDS")
# saveRDS(nola_isometric,
#         "~objects/30/32_nola_isometric.RDS")
# 
# saveRDS(isometric_list,
#         "~objects/30/32_isometric_list.RDS")

## 3. Export as RDS ----
# saveRDS(grids_summary,
#         "~objects/30/32_grids_summary.RDS")
# saveRDS(buffers_summary,
#         "~objects/30/32_buffers_summary.RDS")
# saveRDS(isometric_summary,
#         "~objects/30/32_isometric_summary.RDS")






tmp <- st_intersection(walk_roads,
                       isochrone[100,])

tmp2 <- st_intersection(buses$Baltimore,
                        isochrone[100,])

tm_shape(isochrone[100,]) + tm_polygons(col = "blue", alpha = 0.3) +
  tm_shape(tmp) + tm_lines(col = "orange", lwd = 3) +
  tm_shape(buses$Baltimore) + tm_dots(col = "red", size = 0.3) +
  tm_shape(balt_centroids[100,]) + tm_dots(col = "green", size = 1) +
  tm_shape(balt_centroids_snap[100,]) + tm_dots(col = "purple", size = 1) 


