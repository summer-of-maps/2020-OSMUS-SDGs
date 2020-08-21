##########################################################################
# Mess around here
#
##########################################################################

NO_busStops <- bboxes$`New Orleans` %>% 
  add_osm_feature(key = "highway", value = "bus_stop") %>% 
  osmdata_sf() %>% 
  unique_osmdata()

NO_busStops2 <- bboxes$`New Orleans` %>% 
  add_osm_feature(key = "public_transport") %>% 
  osmdata_sf() %>% 
  unique_osmdata()

tmp <- c(NO_busStops, NO_busStops2)

blt_transit <- Balt_bbox %>% 
  add_osm_feature(key = "public_transport") %>% 
  osmdata_sf() %>% 
  unique_osmdata()

tmp <- list(Balt_busStops, blt_transit)

tmp3 <- c(Balt_busStops, blt_transit)

tmp2 <- do.call("c", tmp)

tmp4 <- tmp2 %>% unique_osmdata()

tmap_mode("view")

tm_shape(blt_transit_points) +
  tm_dots(col = "blue") +
  # tm_shape(blt_transit$osm_lines) +
  # tm_lines(col = "black") +
  tm_shape(blt_transit$osm_polygons) +
  tm_polygons(col = "red") +
  tm_shape(tmp) +
  tm_dots(col = "yellow")

tm_shape(NO_busStops$osm_points) + tm_dots() +
  tm_shape(NO_busStops2$osm_points) + tm_dots()

phl_busStops <- getbb("Philadelphia") %>%
  opq() %>%  
  add_osm_feature(key = "public_transport",
                  # value = "stop",
                  value_exact = FALSE,
                  key_exact = FALSE) %>% 
  # add_osm_feature(key = "route_ref",
  #                 value = "21",
  #                 value_exact = FALSE,
  #                 key_exact = ) %>%
  osmdata_sf() %>% 
  unique_osmdata()

phl_busRoutes <- getbb("Philadelphia") %>%
  opq() %>% 
  add_osm_feature(key = "route",
                  value = "tram",
                  value_exact = FALSE,
                  key_exact = FALSE) %>% 
  add_osm_feature(key = "ref",
                  value = "10",
                  # value_exact = FALSE,
                  key_exact = T) %>% 
  osmdata_sf()

phl_busStops$osm_points %>% qtm
phl_busRoutes$osm_points %>% View

ggplot() + geom_sf(data = phl_busStops$osm_multilines) 
phl_busRoutes$osm_lines %>% st_cast(to = "LINESTRING") %>% qtm

tm_shape(phl_busRoutes$osm_multilines %>% st_cast(to = "LINESTRING")) + tm_lines(col = "black") +
  # tm_shape(phl_busStops$osm_lines) + tm_lines(col = "red") +
  tm_shape(phl_busRoutes$osm_points) + tm_dots(col = "blue")


# isochrones

# baltimore
home <- st_point(c(-76.622381, 39.285293)) %>% 
  st_sfc(crs = 4326) %>% 
  st_sf

dest <- st_point(c(-76.633711, 39.291105)) %>% 
  st_sfc(crs = 4326) %>% 
  st_sf

# philly
home <- st_point(c(-75.220040, 39.951271)) %>% 
  st_sfc(crs = 4326) %>% 
  st_sf

dest <- st_point(c(-75.173476, 39.903796)) %>% 
  st_sfc(crs = 4326) %>% 
  st_sf

route1 <- osrmRoute(src = home, dst = dest, 
                    returnclass="sf")

iso <- osrmIsochrone(
  home,
  breaks = seq(from = 0, to = 60, length.out = 7),
  exclude = NULL,
  res = 40,
  returnclass = "sf"
)



tm_shape(iso) + tm_polygons(alpha = 0.5) +
  tm_shape(home) + tm_dots(col = "red")

options(osrm.server = "http://localhost:5000/", osrm.profile = "walk")

walkscore <- walkshed(x = -75.220110,
                      y = 39.951210,
                      key = "73eb172557d031e57d1d1ab9dcf07c2e")

coords <- walkscore$coordinates %>% 
  st_as_sf(coords = c("lonlist", "latlist"),
            crs = 4326) %>% 
  st_cast()


roads <- st_read("http://data-phl.opendata.arcgis.com/datasets/c36d828494cd44b5bd8b038be696c839_0.geojson")
roads2 <- roads %>% 
  st_transform(2272) %>% 
  as(roads, "Spatial")
tracts <- st_read("http://data.phl.opendata.arcgis.com/datasets/8bc0786524a4486bb3cf0f9862ad0fbf_0.geojson")

tracts2 <- tracts %>% 
  st_transform(2272) %>% 
  as("Spatial")
home2 <- as(home, "Spatial")

network <- SpatialLinesNetwork(roads2)
network2 <- sln_clean_graph(network)

netcatch800m <- calc_network_catchment(
  sln = network2,
  polygonlayer = tracts2,
  targetlayer = home2,
  calccols = c('AWATER10'),
  maximpedance = 3000, # measured in meters for some reason
  distance = 40
)

netcatch800m2 <- netcatch800m %>% 
  st_as_sf() %>% 
  st_transform(4326)

tm_shape(home) + tm_dots(col = "red") +
  tm_shape(tracts) + tm_polygons(col = "gray", alpha = 0.3) +
  tm_shape(netcatch800m2) + tm_polygons(col = "blue", alpha = 0.7)


