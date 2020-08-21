##########################################################################
# This script includes all mapping tasks for the Transit SDG:
# 1. All transit stop points
# 2. Map transit access
#   (a) OSM features - walksheds
#   (b) GTFS features - walksheds
#   (c) Differences
#
# Exports: 
# 1. transit_point_maps as 31_transit_point_maps/[City]_transitStops.png
# 
# To-do:
#
##########################################################################

## 1. ----
# walkroads <- readRDS("~objects/~large_files/22_walkroads.RDS")
pubTrans <- readRDS("~objects/20/22_pubTrans.RDS")
context_bbox_list <- readRDS("~objects/10/14_context_bbox_list.rds")

transit_point_maps <- map2(pubTrans,
                           sdg_basemaps,
                           ~ ggmap(.y) +
                             geom_sf(data = .x$osm_points,
                                     inherit.aes = FALSE,
                                     alpha = 0.5) +
                             labs(title = "Bus Stops in the \'public_transport\' Tag",
                                  caption = "Data source: OSM") +
                             mapTheme())

## 2a. ----
source("~scripts/10 - Data admin.R")
land_list <- readRDS("~objects/30/33_land_list.rds")
dryLand_list <- readRDS("~objects/10/14_dryLand_list.rds")
hydrology_list <- readRDS("~objects/10/14_hydrology_list.rds")
road_list <- readRDS("~objects/10/14_road_list.rds")
# openSpace_results <- readRDS("~objects/30/33_openSpace_results.rds")
grids <- readRDS("~objects/30/32_grids.RDS")
isometric_list <- readRDS("~objects/30/32_isometric_list.RDS")
isometric_summary <- readRDS("~objects/30/32_isometric_summary.RDS")

transit_OSM_mapList <- vector("list", length(bbox_sfs)) %>% 
  set_names(names(bbox_sfs))

transit_OSM_wholeCity_mapList <- vector("list", length(bbox_sfs)) %>% 
  set_names(names(bbox_sfs))

# Baltimore
# minimum density
transit_OSM_mapList$`Baltimore, Maryland` <-
  ggplot() +
    geom_sf(data = dryLand_list$`Baltimore, Maryland`,
            fill = "#4d4d4d",
            # alpha = 0.5,
            color = NA) +
    # geom_sf(data = bbox_sfs$`Baltimore, Maryland`,
    #         fill = NA,
    #         color = NA) +
    geom_sf(data = hydrology_list$`Baltimore, Maryland`,
            color = NA,
            fill = "#97DBF2") +
    geom_sf(data = road_list$`Baltimore, Maryland`,
            aes(size = factor(RTTYP,
                              levels = c("M", "S", "U", "I")),
                color = factor(RTTYP,
                               levels = c("M", "S", "U", "I")))) +
    geom_sf(data = isometric_list$Baltimore,
            color = "#999999",
            size = 0.01,
            # alpha = 0.5,
            # alpha = 0.3,
            aes(fill = walkshed_OSM_denseEnough)) +
    scale_fill_gradientn(name = "Transit Stops\n(1/4 mile walk)",
                         na.value = "#bababa",
                         colors = c("red", "lightblue", "darkblue"),
                         values = scales::rescale(c(0, 0.1, max(isometric_list$Baltimore$walkshed_OSM_denseEnough, na.rm = T))),
                         guide = "legend") +
    scale_color_manual(guide = FALSE,
                       breaks = c("I", "S", "U", "M"),
                       values = c("#feb24c", "#feb24c", "#F7F7F7", "#F7F7F7")) +
    scale_size_manual(guide = FALSE,
                      breaks = c("I", "S", "U", "M"),
                      values = c(0.5, 0.25, 0.25, 0.25)) +
    mapTheme() +
    labs(title = "Transit Access - OSM Data",
         subtitle = paste0("In areas with at least 3,000 people / square mile, ",
                           round(isometric_summary$Baltimore$OSM_prop[2], 3) * 100,
                           "% of people have access to transit"))

ggsave(plot = transit_OSM_mapList$`Baltimore, Maryland`,
       filename = paste0("~plots/Transit SDG/OSM Maps/",
                         "Baltimore, Maryland",
                         "_OSM.pdf"),
       units = "in",
       dpi = 600,
       width = 8.5,
       height = 11)

# whole city
transit_OSM_wholeCity_mapList$`Baltimore, Maryland` <-
  ggplot() +
  geom_sf(data = dryLand_list$`Baltimore, Maryland`,
          fill = "#4d4d4d",
          # alpha = 0.5,
          color = NA) +
  # geom_sf(data = bbox_sfs$`Baltimore, Maryland`,
  #         fill = NA,
  #         color = NA) +
  geom_sf(data = hydrology_list$`Baltimore, Maryland`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`Baltimore, Maryland`,
          aes(size = factor(RTTYP,
                            levels = c("M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("M", "S", "U", "I")))) +
  geom_sf(data = isometric_list$Baltimore %>% 
            mutate(walkshed_OSM = ifelse(TotPop == 0, NA, walkshed_OSM)),
          color = "#999999",
          size = 0.01,
          # alpha = 0.5,
          # alpha = 0.3,
          aes(fill = walkshed_OSM)) +
  scale_fill_gradientn(name = "Transit Stops\n(1/4 mile walk)",
                       na.value = "#bababa",
                       colors = c("red", "lightblue", "darkblue"),
                       values = scales::rescale(c(0, 0.1, max(isometric_list$Baltimore$walkshed_OSM_denseEnough, na.rm = T))),
                       guide = "legend") +
  scale_color_manual(guide = FALSE,
                     breaks = c("I", "S", "U", "M"),
                     values = c("#feb24c", "#feb24c", "#F7F7F7", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("I", "S", "U", "M"),
                    values = c(0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "Transit Access - OSM Data",
       subtitle = paste0("In the whole city, ",
                         round(isometric_summary_allCells$Baltimore$OSM_prop[2], 3) * 100,
                         "% of people have access to transit"))

ggsave(plot = transit_OSM_wholeCity_mapList$`Baltimore, Maryland`,
       filename = paste0("~plots/Transit SDG/OSM Maps/",
                         "Baltimore, Maryland",
                         "_OSM_wholeCity.pdf"),
       units = "in",
       dpi = 600,
       width = 8.5,
       height = 11)

# Minneapolis
# minimum density
transit_OSM_mapList$`Minneapolis, Minnesota` <-
  ggplot() +
  geom_sf(data = dryLand_list$`Minneapolis, Minnesota`,
          fill = "#4d4d4d",
          # alpha = 0.5,
          color = NA) +
  # geom_sf(data = bbox_sfs$`Baltimore, Maryland`,
  #         fill = NA,
  #         color = NA) +
  geom_sf(data = hydrology_list$`Minneapolis, Minnesota`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`Minneapolis, Minnesota`,
          aes(size = factor(RTTYP,
                            levels = c("C", "M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("C", "M", "S", "U", "I")))) +
  geom_sf(data = isometric_list$Minneapolis,
          color = "#999999",
          size = 0.01,
          # alpha = 0.5,
          # alpha = 0.3,
          aes(fill = walkshed_OSM_denseEnough)) +
  scale_fill_gradientn(name = "Transit Stops\n(1/4 mile walk)",
                       na.value = "#bababa",
                       colors = c("red", "lightblue", "darkblue"),
                       values = scales::rescale(c(0, 0.1, max(isometric_list$Minneapolis$walkshed_OSM_denseEnough, na.rm = T))),
                       guide = "legend") +
  scale_color_manual(guide = FALSE,
                     breaks = c("C", "I", "S", "U", "M"),
                     values = c("#F7F7F7", "#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("C", "I", "S", "U", "M"),
                    values = c(0.25, 0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "Transit Access - OSM Data",
       subtitle = paste0("In areas with at least 3,000 people / square mile, ",
                         round(isometric_summary$Minneapolis$OSM_prop[2], 3) * 100,
                         "% of people have access to transit"))

ggsave(plot = transit_OSM_mapList$`Minneapolis, Minnesota`,
       filename = paste0("~plots/Transit SDG/OSM Maps/",
                         "Minneapolis, Minnesota",
                         "_OSM.pdf"),
       units = "in",
       dpi = 600,
       width = 8.5,
       height = 11)

# whole city
transit_OSM_wholeCity_mapList$`Minneapolis, Minnesota` <-
  ggplot() +
  geom_sf(data = dryLand_list$`Minneapolis, Minnesota`,
          fill = "#4d4d4d",
          # alpha = 0.5,
          color = NA) +
  # geom_sf(data = bbox_sfs$`Baltimore, Maryland`,
  #         fill = NA,
  #         color = NA) +
  geom_sf(data = hydrology_list$`Minneapolis, Minnesota`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`Minneapolis, Minnesota`,
          aes(size = factor(RTTYP,
                            levels = c("C", "M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("C", "M", "S", "U", "I")))) +
  geom_sf(data = isometric_list$Minneapolis %>% 
            mutate(walkshed_OSM = ifelse(TotPop == 0, NA, walkshed_OSM)),
          color = "#999999",
          size = 0.01,
          # alpha = 0.5,
          # alpha = 0.3,
          aes(fill = walkshed_OSM)) +
  scale_fill_gradientn(name = "Transit Stops\n(1/4 mile walk)",
                       na.value = "#bababa",
                       colors = c("red", "lightblue", "darkblue"),
                       values = scales::rescale(c(0, 0.1, max(isometric_list$Minneapolis$walkshed_OSM_denseEnough, na.rm = T))),
                       guide = "legend") +
  scale_color_manual(guide = FALSE,
                     breaks = c("C", "I", "S", "U", "M"),
                     values = c("#F7F7F7", "#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("C", "I", "S", "U", "M"),
                    values = c(0.25, 0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "Transit Access - OSM Data",
       subtitle = paste0("In the whole city, ",
                         round(isometric_summary_allCells$Minneapolis$OSM_prop[2], 3) * 100,
                         "% of people have access to transit"))


ggsave(plot = transit_OSM_wholeCity_mapList$`Minneapolis, Minnesota`,
       filename = paste0("~plots/Transit SDG/OSM Maps/",
                         "Minneapolis, Minnesota",
                         "_OSM_wholeCity.pdf"),
       units = "in",
       dpi = 600,
       width = 8.5,
       height = 11)

# New Orleans
# minimum density
transit_OSM_mapList$`New Orleans, Louisiana` <-
  ggplot() +
  geom_sf(data = dryLand_list$`New Orleans, Louisiana`,
          fill = "#4d4d4d",
          # alpha = 0.5,
          color = NA) +
  # geom_sf(data = bbox_sfs$`Baltimore, Maryland`,
  #         fill = NA,
  #         color = NA) +
  geom_sf(data = hydrology_list$`New Orleans, Louisiana`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`New Orleans, Louisiana`,
          aes(size = factor(RTTYP,
                            levels = c("M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("M", "S", "U", "I")))) +
  geom_sf(data = isometric_list$`New Orleans`,
          color = "#999999",
          size = 0.01,
          # alpha = 0.5,
          # alpha = 0.3,
          aes(fill = walkshed_OSM_denseEnough)) +
  scale_fill_gradientn(name = "Transit Stops\n(1/4 mile walk)",
                       na.value = "#bababa",
                       colors = c("red", "lightblue", "darkblue"),
                       values = scales::rescale(c(0, 0.1, max(isometric_list$`New Orleans`$walkshed_OSM_denseEnough, na.rm = T))),
                       guide = "legend") +
  scale_color_manual(guide = FALSE,
                     breaks = c("I", "S", "U", "M"),
                     values = c("#feb24c", "#feb24c", "#F7F7F7", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("I", "S", "U", "M"),
                    values = c(0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "Transit Access - OSM Data",
       subtitle = paste0("In areas with at least 3,000 people / square mile, ",
                         round(isometric_summary$`New Orleans`$OSM_prop[2], 3) * 100,
                         "% of people have access to transit"))

ggsave(plot = transit_OSM_mapList$`New Orleans, Louisiana`,
       filename = paste0("~plots/Transit SDG/OSM Maps/",
                         "New Orleans, Louisiana",
                         "_OSM.pdf"),
       units = "in",
       dpi = 600,
       width = 8.5,
       height = 11)

# whole city
transit_OSM_wholeCity_mapList$`New Orleans, Louisiana` <-
  ggplot() +
  geom_sf(data = dryLand_list$`New Orleans, Louisiana`,
          fill = "#4d4d4d",
          # alpha = 0.5,
          color = NA) +
  # geom_sf(data = bbox_sfs$`Baltimore, Maryland`,
  #         fill = NA,
  #         color = NA) +
  geom_sf(data = hydrology_list$`New Orleans, Louisiana`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`New Orleans, Louisiana`,
          aes(size = factor(RTTYP,
                            levels = c("M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("M", "S", "U", "I")))) +
  geom_sf(data = isometric_list$`New Orleans` %>% 
            mutate(walkshed_OSM = ifelse(TotPop == 0, NA, walkshed_OSM)),
          color = "#999999",
          size = 0.01,
          # alpha = 0.5,
          # alpha = 0.3,
          aes(fill = walkshed_OSM)) +
  scale_fill_gradientn(name = "Transit Stops\n(1/4 mile walk)",
                       na.value = "#bababa",
                       colors = c("red", "lightblue", "darkblue"),
                       values = scales::rescale(c(0, 0.1, max(isometric_list$`New Orleans`$walkshed_OSM_denseEnough, na.rm = T))),
                       guide = "legend") +
  scale_color_manual(guide = FALSE,
                     breaks = c("I", "S", "U", "M"),
                     values = c("#feb24c", "#feb24c", "#F7F7F7", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("I", "S", "U", "M"),
                    values = c(0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "Transit Access - OSM Data",
       subtitle = paste0("In the whole city, ",
                         round(isometric_summary_allCells$`New Orleans`$OSM_prop[2], 3) * 100,
                         "% of people have access to transit"))


ggsave(plot = transit_OSM_wholeCity_mapList$`New Orleans, Louisiana`,
       filename = paste0("~plots/Transit SDG/OSM Maps/",
                         "New Orleans, Louisiana",
                         "_OSM_wholeCity.pdf"),
       units = "in",
       dpi = 600,
       width = 8.5,
       height = 11)

# Philadelphia
# minimum density
transit_OSM_mapList$`Philadelphia, Pennsylvania` <-
  ggplot() +
  geom_sf(data = dryLand_list$`Philadelphia, Pennsylvania`,
          fill = "#4d4d4d",
          # alpha = 0.5,
          color = NA) +
  # geom_sf(data = bbox_sfs$`Baltimore, Maryland`,
  #         fill = NA,
  #         color = NA) +
  geom_sf(data = hydrology_list$`Philadelphia, Pennsylvania`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`Philadelphia, Pennsylvania`,
          aes(size = factor(RTTYP,
                            levels = c("O", "M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("O", "M", "S", "U", "I")))) +
  geom_sf(data = isometric_list$Philadelphia,
          color = "#999999",
          size = 0.01,
          # alpha = 0.5,
          # alpha = 0.3,
          aes(fill = walkshed_OSM_denseEnough)) +
  scale_fill_gradientn(name = "Transit Stops\n(1/4 mile walk)",
                       na.value = "#bababa",
                       colors = c("red", "lightblue", "darkblue"),
                       values = scales::rescale(c(0, 0.1, max(isometric_list$Philadelphia$walkshed_OSM_denseEnough, na.rm = T))),
                       guide = "legend") +
  scale_color_manual(guide = FALSE,
                     breaks = c("O", "I", "S", "U", "M"),
                     values = c("#F7F7F7", "#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("O", "I", "S", "U", "M"),
                    values = c(0.25, 0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "Transit Access - OSM Data",
       subtitle = paste0("In areas with at least 3,000 people / square mile, ",
                         round(isometric_summary$Philadelphia$OSM_prop[2], 3) * 100,
                         "% of people have access to transit"))

ggsave(plot = transit_OSM_mapList$`Philadelphia, Pennsylvania`,
       filename = paste0("~plots/Transit SDG/OSM Maps/",
                         "Philadelphia, Pennsylvania",
                         "_OSM.pdf"),
       units = "in",
       dpi = 600,
       width = 8.5,
       height = 11)

# whole city
transit_OSM_wholeCity_mapList$`Philadelphia, Pennsylvania` <-
  ggplot() +
  geom_sf(data = dryLand_list$`Philadelphia, Pennsylvania`,
          fill = "#4d4d4d",
          # alpha = 0.5,
          color = NA) +
  # geom_sf(data = bbox_sfs$`Baltimore, Maryland`,
  #         fill = NA,
  #         color = NA) +
  geom_sf(data = hydrology_list$`Philadelphia, Pennsylvania`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`Philadelphia, Pennsylvania`,
          aes(size = factor(RTTYP,
                            levels = c("O", "M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("O", "M", "S", "U", "I")))) +
  geom_sf(data = isometric_list$Philadelphia %>% 
            mutate(walkshed_OSM = ifelse(TotPop == 0, NA, walkshed_OSM)),
          color = "#999999",
          size = 0.01,
          # alpha = 0.5,
          # alpha = 0.3,
          aes(fill = walkshed_OSM)) +
  scale_fill_gradientn(name = "Transit Stops\n(1/4 mile walk)",
                       na.value = "#bababa",
                       colors = c("red", "lightblue", "darkblue"),
                       values = scales::rescale(c(0, 0.1, max(isometric_list$Philadelphia$walkshed_OSM_denseEnough, na.rm = T))),
                       guide = "legend") +
  scale_color_manual(guide = FALSE,
                     breaks = c("O", "I", "S", "U", "M"),
                     values = c("#F7F7F7", "#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("O", "I", "S", "U", "M"),
                    values = c(0.25, 0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "Transit Access - OSM Data",
       subtitle = paste0("In the whole city, ",
                         round(isometric_summary_allCells$Philadelphia$OSM_prop[2], 3) * 100,
                         "% of people have access to transit"))

ggsave(plot = transit_OSM_wholeCity_mapList$`Philadelphia, Pennsylvania`,
       filename = paste0("~plots/Transit SDG/OSM Maps/",
                         "Philadelphia, Pennsylvania",
                         "_OSM_wholeCity.pdf"),
       units = "in",
       dpi = 600,
       width = 8.5,
       height = 11)


# Houston
# minimum density
transit_OSM_mapList$`Houston, Texas` <-
  ggplot() +
  geom_sf(data = dryLand_list$`Houston, Texas`,
          fill = "#4d4d4d",
          # alpha = 0.5,
          color = NA) +
  # geom_sf(data = bbox_sfs$`Baltimore, Maryland`,
  #         fill = NA,
  #         color = NA) +
  geom_sf(data = hydrology_list$`Houston, Texas`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`Houston, Texas`,
          aes(size = factor(RTTYP,
                            levels = c("C", "O", "M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("C", "O", "M", "S", "U", "I")))) +
  geom_sf(data = isometric_list$Houston,
          color = "#999999",
          size = 0.01,
          # alpha = 0.5,
          # alpha = 0.3,
          aes(fill = walkshed_OSM_denseEnough)) +
  scale_fill_gradientn(name = "Transit Stops\n(1/4 mile walk)",
                       na.value = "#bababa",
                       colors = c("red", "lightblue", "darkblue"),
                       values = scales::rescale(c(0, 0.1, max(isometric_list$Houston$walkshed_OSM_denseEnough, na.rm = T))),
                       guide = "legend") +
  scale_color_manual(guide = FALSE,
                     breaks = c("C", "O", "I", "S", "U", "M"),
                     values = c("#F7F7F7", "#F7F7F7", "#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("C", "O", "I", "S", "U", "M"),
                    values = c(0.25, 0.25, 0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "Transit Access - OSM Data",
       subtitle = paste0("In areas with at least 3,000 people / square mile, ",
                         round(isometric_summary$Houston$OSM_prop[2], 3) * 100,
                         "% of people have access to transit"))

ggsave(plot = transit_OSM_mapList$`Houston, Texas`,
       filename = paste0("~plots/Transit SDG/OSM Maps/",
                         "Houston, Texas",
                         "_OSM.pdf"),
       units = "in",
       dpi = 600,
       width = 8.5,
       height = 11)

# whole city
transit_OSM_wholeCity_mapList$`Houston, Texas` <-
  ggplot() +
  geom_sf(data = dryLand_list$`Houston, Texas`,
          fill = "#4d4d4d",
          # alpha = 0.5,
          color = NA) +
  # geom_sf(data = bbox_sfs$`Baltimore, Maryland`,
  #         fill = NA,
  #         color = NA) +
  geom_sf(data = hydrology_list$`Houston, Texas`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`Houston, Texas`,
          aes(size = factor(RTTYP,
                            levels = c("C", "O", "M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("C", "O", "M", "S", "U", "I")))) +
  geom_sf(data = isometric_list$Houston %>% 
            mutate(walkshed_OSM = ifelse(TotPop == 0, NA, walkshed_OSM)),
          color = "#999999",
          size = 0.01,
          # alpha = 0.5,
          # alpha = 0.3,
          aes(fill = walkshed_OSM)) +
  scale_fill_gradientn(name = "Transit Stops\n(1/4 mile walk)",
                       na.value = "#bababa",
                       colors = c("red", "lightblue", "darkblue"),
                       values = scales::rescale(c(0, 0.1, max(isometric_list$Houston$walkshed_OSM_denseEnough, na.rm = T))),
                       guide = "legend") +
  scale_color_manual(guide = FALSE,
                     breaks = c("C", "O", "I", "S", "U", "M"),
                     values = c("#F7F7F7", "#F7F7F7", "#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("C", "O", "I", "S", "U", "M"),
                    values = c(0.25, 0.25, 0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "Transit Access - OSM Data",
       subtitle = paste0("In the whole city, ",
                         round(isometric_summary_allCells$Houston$OSM_prop[2], 3) * 100,
                         "% of people have access to transit"))

ggsave(plot = transit_OSM_wholeCity_mapList$`Houston, Texas`,
       filename = paste0("~plots/Transit SDG/OSM Maps/",
                         "Houston, Texas",
                         "_OSM_wholeCity.pdf"),
       units = "in",
       dpi = 600,
       width = 8.5,
       height = 11)

# San Francisco
# minimum density
transit_OSM_mapList$`San Francisco, California` <-
  ggplot() +
  geom_sf(data = dryLand_list$`San Francisco, California`,
          fill = "#4d4d4d",
          # alpha = 0.5,
          color = NA) +
  # geom_sf(data = bbox_sfs$`Baltimore, Maryland`,
  #         fill = NA,
  #         color = NA) +
  geom_sf(data = hydrology_list$`San Francisco, California`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`San Francisco, California`,
          aes(size = factor(RTTYP,
                            levels = c("M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("M", "S", "U", "I")))) +
  geom_sf(data = isometric_list$`San Francisco`,
          color = "#999999",
          size = 0.01,
          # alpha = 0.5,
          # alpha = 0.3,
          aes(fill = walkshed_OSM_denseEnough)) +
  scale_fill_gradientn(name = "Transit Stops\n(1/4 mile walk)",
                       na.value = "#bababa",
                       colors = c("red", "lightblue", "darkblue"),
                       values = scales::rescale(c(0, 0.1, max(isometric_list$`San Francisco`$walkshed_OSM_denseEnough, na.rm = T))),
                       guide = "legend") +
  scale_color_manual(guide = FALSE,
                     breaks = c("I", "S", "U", "M"),
                     values = c("#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("I", "S", "U", "M"),
                    values = c(0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "Transit Access - OSM Data",
       subtitle = paste0("In areas with at least 3,000 people / square mile, ",
                         round(isometric_summary$`San Francisco`$OSM_prop[2], 3) * 100,
                         "% of people have access to transit"))

ggsave(plot = transit_OSM_mapList$`San Francisco, California`,
       filename = paste0("~plots/Transit SDG/OSM Maps/",
                         "San Francisco, California",
                         "_OSM.pdf"),
       units = "in",
       dpi = 600,
       width = 8.5,
       height = 11)

# whole city
transit_OSM_wholeCity_mapList$`San Francisco, California` <-
  ggplot() +
  geom_sf(data = dryLand_list$`San Francisco, California`,
          fill = "#4d4d4d",
          # alpha = 0.5,
          color = NA) +
  # geom_sf(data = bbox_sfs$`Baltimore, Maryland`,
  #         fill = NA,
  #         color = NA) +
  geom_sf(data = hydrology_list$`San Francisco, California`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`San Francisco, California`,
          aes(size = factor(RTTYP,
                            levels = c("M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("M", "S", "U", "I")))) +
  geom_sf(data = isometric_list$`San Francisco` %>% 
            mutate(walkshed_OSM = ifelse(TotPop == 0, NA, walkshed_OSM)),
          color = "#999999",
          size = 0.01,
          # alpha = 0.5,
          # alpha = 0.3,
          aes(fill = walkshed_OSM)) +
  scale_fill_gradientn(name = "Transit Stops\n(1/4 mile walk)",
                       na.value = "#bababa",
                       colors = c("red", "lightblue", "darkblue"),
                       values = scales::rescale(c(0, 0.1, max(isometric_list$`San Francisco`$walkshed_OSM_denseEnough, na.rm = T))),
                       guide = "legend") +
  scale_color_manual(guide = FALSE,
                     breaks = c("I", "S", "U", "M"),
                     values = c("#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("I", "S", "U", "M"),
                    values = c(0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
    labs(title = "Transit Access - OSM Data",
         subtitle = paste0("In the whole city, ",
                         round(isometric_summary_allCells$`San Francisco`$OSM_prop[2], 3) * 100,
                         "% of people have access to transit"))

ggsave(plot = transit_OSM_wholeCity_mapList$`San Francisco, California`,
       filename = paste0("~plots/Transit SDG/OSM Maps/",
                         "San Francisco, California",
                         "_OSM_wholeCity.pdf"),
       units = "in",
       dpi = 600,
       width = 8.5,
       height = 11)

## 2b. ----
transit_GTFS_mapList <- vector("list", length(bbox_sfs)) %>% 
  set_names(names(bbox_sfs))

transit_GTFS_wholeCity_mapList <- vector("list", length(bbox_sfs)) %>% 
  set_names(names(bbox_sfs))

# Baltimore
# minimum density
transit_GTFS_mapList$`Baltimore, Maryland` <-
  ggplot() +
  geom_sf(data = dryLand_list$`Baltimore, Maryland`,
          fill = "#4d4d4d",
          # alpha = 0.5,
          color = NA) +
  # geom_sf(data = bbox_sfs$`Baltimore, Maryland`,
  #         fill = NA,
  #         color = NA) +
  geom_sf(data = hydrology_list$`Baltimore, Maryland`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`Baltimore, Maryland`,
          aes(size = factor(RTTYP,
                            levels = c("M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("M", "S", "U", "I")))) +
  geom_sf(data = isometric_list$Baltimore,
          color = "#999999",
          size = 0.01,
          # alpha = 0.5,
          # alpha = 0.3,
          aes(fill = walkshed_GTFS_denseEnough)) +
  scale_fill_gradientn(name = "Transit Stops\n(1/4 mile walk)",
                       na.value = "#bababa",
                       colors = c("red", "lightblue", "darkblue"),
                       values = scales::rescale(c(0, 0.1, max(isometric_list$Baltimore$walkshed_GTFS_denseEnough, na.rm = T))),
                       guide = "legend") +
  scale_color_manual(guide = FALSE,
                     breaks = c("I", "S", "U", "M"),
                     values = c("#feb24c", "#feb24c", "#F7F7F7", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("I", "S", "U", "M"),
                    values = c(0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "Transit Access - GTFS Data",
       subtitle = paste0("In areas with at least 3,000 people / square mile, ",
                         round(isometric_summary$Baltimore$GTFS_prop[2], 3) * 100,
                         "% of people have access to transit"))
  
ggsave(plot = transit_GTFS_mapList$`Baltimore, Maryland`,
       filename = paste0("~plots/Transit SDG/GTFS Maps/",
                         "Baltimore, Maryland",
                         "_GTFS.pdf"),
       units = "in",
       dpi = 600,
       width = 8.5,
       height = 11)

# whole city
transit_GTFS_wholeCity_mapList$`Baltimore, Maryland` <-
  ggplot() +
  geom_sf(data = dryLand_list$`Baltimore, Maryland`,
          fill = "#4d4d4d",
          # alpha = 0.5,
          color = NA) +
  # geom_sf(data = bbox_sfs$`Baltimore, Maryland`,
  #         fill = NA,
  #         color = NA) +
  geom_sf(data = hydrology_list$`Baltimore, Maryland`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`Baltimore, Maryland`,
          aes(size = factor(RTTYP,
                            levels = c("M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("M", "S", "U", "I")))) +
  geom_sf(data = isometric_list$Baltimore %>% 
            mutate(walkshed_GTFS = ifelse(TotPop == 0, NA, walkshed_GTFS)),
          color = "#999999",
          size = 0.01,
          # alpha = 0.5,
          # alpha = 0.3,
          aes(fill = walkshed_GTFS)) +
  scale_fill_gradientn(name = "Transit Stops\n(1/4 mile walk)",
                       na.value = "#bababa",
                       colors = c("red", "lightblue", "darkblue"),
                       values = scales::rescale(c(0, 0.1, max(isometric_list$Baltimore$walkshed_OSM_denseEnough, na.rm = T))),
                       guide = "legend") +
  scale_color_manual(guide = FALSE,
                     breaks = c("I", "S", "U", "M"),
                     values = c("#feb24c", "#feb24c", "#F7F7F7", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("I", "S", "U", "M"),
                    values = c(0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "Transit Access - GTFS Data",
       subtitle = paste0("In the whole city, ",
                         round(isometric_summary_allCells$Baltimore$GTFS_prop[2], 3) * 100,
                         "% of people have access to transit"))

ggsave(plot = transit_GTFS_wholeCity_mapList$`Baltimore, Maryland`,
       filename = paste0("~plots/Transit SDG/GTFS Maps/",
                         "Baltimore, Maryland",
                         "_GTFS_wholeCity.pdf"),
       units = "in",
       dpi = 600,
       width = 8.5,
       height = 11)

# Minneapolis
# minimum density
transit_GTFS_mapList$`Minneapolis, Minnesota` <-
  ggplot() +
  geom_sf(data = dryLand_list$`Minneapolis, Minnesota`,
          fill = "#4d4d4d",
          # alpha = 0.5,
          color = NA) +
  # geom_sf(data = bbox_sfs$`Baltimore, Maryland`,
  #         fill = NA,
  #         color = NA) +
  geom_sf(data = hydrology_list$`Minneapolis, Minnesota`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`Minneapolis, Minnesota`,
          aes(size = factor(RTTYP,
                            levels = c("C", "M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("C", "M", "S", "U", "I")))) +
  geom_sf(data = isometric_list$Minneapolis,
          color = "#999999",
          size = 0.01,
          # alpha = 0.5,
          # alpha = 0.3,
          aes(fill = walkshed_GTFS_denseEnough)) +
  scale_fill_gradientn(name = "Transit Stops\n(1/4 mile walk)",
                       na.value = "#bababa",
                       colors = c("red", "lightblue", "darkblue"),
                       values = scales::rescale(c(0, 0.1, max(isometric_list$Minneapolis$walkshed_GTFS_denseEnough, na.rm = T))),
                       guide = "legend") +
  scale_color_manual(guide = FALSE,
                     breaks = c("C", "I", "S", "U", "M"),
                     values = c("#F7F7F7", "#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("C", "I", "S", "U", "M"),
                    values = c(0.25, 0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "Transit Access - GTFS Data",
       subtitle = paste0("In areas with at least 3,000 people / square mile, ",
                         round(isometric_summary$Minneapolis$GTFS_prop[2], 3) * 100,
                         "% of people have access to transit"))

ggsave(plot = transit_GTFS_mapList$`Minneapolis, Minnesota`,
       filename = paste0("~plots/Transit SDG/GTFS Maps/",
                         "Minneapolis, Minnesota",
                         "_GTFS.pdf"),
       units = "in",
       dpi = 600,
       width = 8.5,
       height = 11)

# whole city
transit_GTFS_wholeCity_mapList$`Minneapolis, Minnesota` <-
  ggplot() +
  geom_sf(data = dryLand_list$`Minneapolis, Minnesota`,
          fill = "#4d4d4d",
          # alpha = 0.5,
          color = NA) +
  # geom_sf(data = bbox_sfs$`Baltimore, Maryland`,
  #         fill = NA,
  #         color = NA) +
  geom_sf(data = hydrology_list$`Minneapolis, Minnesota`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`Minneapolis, Minnesota`,
          aes(size = factor(RTTYP,
                            levels = c("C", "M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("C", "M", "S", "U", "I")))) +
  geom_sf(data = isometric_list$Minneapolis %>% 
            mutate(walkshed_GTFS = ifelse(TotPop == 0, NA, walkshed_GTFS)),
          color = "#999999",
          size = 0.01,
          # alpha = 0.5,
          # alpha = 0.3,
          aes(fill = walkshed_GTFS)) +
  scale_fill_gradientn(name = "Transit Stops\n(1/4 mile walk)",
                       na.value = "#bababa",
                       colors = c("red", "lightblue", "darkblue"),
                       values = scales::rescale(c(0, 0.1, max(isometric_list$Minneapolis$walkshed_OSM_denseEnough, na.rm = T))),
                       guide = "legend") +
  scale_color_manual(guide = FALSE,
                     breaks = c("C", "I", "S", "U", "M"),
                     values = c("#F7F7F7", "#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("C", "I", "S", "U", "M"),
                    values = c(0.25, 0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "Transit Access - GTFS Data",
       subtitle = paste0("In the whole city, ",
                         round(isometric_summary_allCells$Minneapolis$GTFS_prop[2], 3) * 100,
                         "% of people have access to transit"))


ggsave(plot = transit_GTFS_wholeCity_mapList$`Minneapolis, Minnesota`,
       filename = paste0("~plots/Transit SDG/GTFS Maps/",
                         "Minneapolis, Minnesota",
                         "_GTFS_wholeCity.pdf"),
       units = "in",
       dpi = 600,
       width = 8.5,
       height = 11)

# New Orleans
# minimum density
transit_GTFS_mapList$`New Orleans, Louisiana` <-
  ggplot() +
  geom_sf(data = dryLand_list$`New Orleans, Louisiana`,
          fill = "#4d4d4d",
          # alpha = 0.5,
          color = NA) +
  # geom_sf(data = bbox_sfs$`Baltimore, Maryland`,
  #         fill = NA,
  #         color = NA) +
  geom_sf(data = hydrology_list$`New Orleans, Louisiana`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`New Orleans, Louisiana`,
          aes(size = factor(RTTYP,
                            levels = c("M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("M", "S", "U", "I")))) +
  geom_sf(data = isometric_list$`New Orleans`,
          color = "#999999",
          size = 0.01,
          # alpha = 0.5,
          # alpha = 0.3,
          aes(fill = walkshed_GTFS_denseEnough)) +
  scale_fill_gradientn(name = "Transit Stops\n(1/4 mile walk)",
                       na.value = "#bababa",
                       colors = c("red", "lightblue", "darkblue"),
                       values = scales::rescale(c(0, 0.1, max(isometric_list$`New Orleans`$walkshed_GTFS_denseEnough, na.rm = T))),
                       guide = "legend") +
  scale_color_manual(guide = FALSE,
                     breaks = c("I", "S", "U", "M"),
                     values = c("#feb24c", "#feb24c", "#F7F7F7", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("I", "S", "U", "M"),
                    values = c(0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "Transit Access - GTFS Data",
       subtitle = paste0("In areas with at least 3,000 people / square mile, ",
                         round(isometric_summary$`New Orleans`$GTFS_prop[2], 3) * 100,
                         "% of people have access to transit"))

ggsave(plot = transit_GTFS_mapList$`New Orleans, Louisiana`,
       filename = paste0("~plots/Transit SDG/GTFS Maps/",
                         "New Orleans, Louisiana",
                         "_GTFS.pdf"),
       units = "in",
       dpi = 600,
       width = 8.5,
       height = 11)

# whole city
transit_GTFS_wholeCity_mapList$`New Orleans, Louisiana` <-
  ggplot() +
  geom_sf(data = dryLand_list$`New Orleans, Louisiana`,
          fill = "#4d4d4d",
          # alpha = 0.5,
          color = NA) +
  # geom_sf(data = bbox_sfs$`Baltimore, Maryland`,
  #         fill = NA,
  #         color = NA) +
  geom_sf(data = hydrology_list$`New Orleans, Louisiana`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`New Orleans, Louisiana`,
          aes(size = factor(RTTYP,
                            levels = c("M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("M", "S", "U", "I")))) +
  geom_sf(data = isometric_list$`New Orleans` %>% 
            mutate(walkshed_GTFS = ifelse(TotPop == 0, NA, walkshed_GTFS)),
          color = "#999999",
          size = 0.01,
          # alpha = 0.5,
          # alpha = 0.3,
          aes(fill = walkshed_GTFS)) +
  scale_fill_gradientn(name = "Transit Stops\n(1/4 mile walk)",
                       na.value = "#bababa",
                       colors = c("red", "lightblue", "darkblue"),
                       values = scales::rescale(c(0, 0.1, max(isometric_list$`New Orleans`$walkshed_OSM_denseEnough, na.rm = T))),
                       guide = "legend") +
  scale_color_manual(guide = FALSE,
                     breaks = c("I", "S", "U", "M"),
                     values = c("#feb24c", "#feb24c", "#F7F7F7", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("I", "S", "U", "M"),
                    values = c(0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "Transit Access - GTFS Data",
       subtitle = paste0("In the whole city, ",
                         round(isometric_summary_allCells$`New Orleans`$GTFS_prop[2], 3) * 100,
                         "% of people have access to transit"))

ggsave(plot = transit_GTFS_wholeCity_mapList$`New Orleans, Louisiana`,
       filename = paste0("~plots/Transit SDG/GTFS Maps/",
                         "New Orleans, Louisiana",
                         "_GTFS_wholeCity.pdf"),
       units = "in",
       dpi = 600,
       width = 8.5,
       height = 11)

# Philadelphia
# minimum density
transit_GTFS_mapList$`Philadelphia, Pennsylvania` <-
  ggplot() +
  geom_sf(data = dryLand_list$`Philadelphia, Pennsylvania`,
          fill = "#4d4d4d",
          # alpha = 0.5,
          color = NA) +
  # geom_sf(data = bbox_sfs$`Baltimore, Maryland`,
  #         fill = NA,
  #         color = NA) +
  geom_sf(data = hydrology_list$`Philadelphia, Pennsylvania`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`Philadelphia, Pennsylvania`,
          aes(size = factor(RTTYP,
                            levels = c("O", "M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("O", "M", "S", "U", "I")))) +
  geom_sf(data = isometric_list$Philadelphia,
          color = "#999999",
          size = 0.01,
          # alpha = 0.5,
          # alpha = 0.3,
          aes(fill = walkshed_GTFS_denseEnough)) +
  scale_fill_gradientn(name = "Transit Stops\n(1/4 mile walk)",
                       na.value = "#bababa",
                       colors = c("red", "lightblue", "darkblue"),
                       values = scales::rescale(c(0, 0.1, max(isometric_list$Philadelphia$walkshed_GTFS_denseEnough, na.rm = T))),
                       guide = "legend") +
  scale_color_manual(guide = FALSE,
                     breaks = c("O", "I", "S", "U", "M"),
                     values = c("#F7F7F7", "#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("O", "I", "S", "U", "M"),
                    values = c(0.25, 0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "Transit Access - GTFS Data",
       subtitle = paste0("In areas with at least 3,000 people / square mile, ",
                         round(isometric_summary$Philadelphia$GTFS_prop[2], 3) * 100,
                         "% of people have access to transit"))

ggsave(plot = transit_GTFS_mapList$`Philadelphia, Pennsylvania`,
       filename = paste0("~plots/Transit SDG/GTFS Maps/",
                         "Philadelphia, Pennsylvania",
                         "_GTFS.pdf"),
       units = "in",
       dpi = 600,
       width = 8.5,
       height = 11)

# whole city
transit_GTFS_wholeCity_mapList$`Philadelphia, Pennsylvania` <-
  ggplot() +
  geom_sf(data = dryLand_list$`Philadelphia, Pennsylvania`,
          fill = "#4d4d4d",
          # alpha = 0.5,
          color = NA) +
  # geom_sf(data = bbox_sfs$`Baltimore, Maryland`,
  #         fill = NA,
  #         color = NA) +
  geom_sf(data = hydrology_list$`Philadelphia, Pennsylvania`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`Philadelphia, Pennsylvania`,
          aes(size = factor(RTTYP,
                            levels = c("O", "M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("O", "M", "S", "U", "I")))) +
  geom_sf(data = isometric_list$Philadelphia %>% 
            mutate(walkshed_GTFS = ifelse(TotPop == 0, NA, walkshed_GTFS)),
          color = "#999999",
          size = 0.01,
          # alpha = 0.5,
          # alpha = 0.3,
          aes(fill = walkshed_GTFS)) +
  scale_fill_gradientn(name = "Transit Stops\n(1/4 mile walk)",
                       na.value = "#bababa",
                       colors = c("red", "lightblue", "darkblue"),
                       values = scales::rescale(c(0, 0.1, max(isometric_list$Philadelphia$walkshed_OSM_denseEnough, na.rm = T))),
                       guide = "legend") +
  scale_color_manual(guide = FALSE,
                     breaks = c("O", "I", "S", "U", "M"),
                     values = c("#F7F7F7", "#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("O", "I", "S", "U", "M"),
                    values = c(0.25, 0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "Transit Access - GTFS Data",
       subtitle = paste0("In the whole city, ",
                         round(isometric_summary_allCells$Philadelphia$GTFS_prop[2], 3) * 100,
                         "% of people have access to transit"))

ggsave(plot = transit_GTFS_wholeCity_mapList$`Philadelphia, Pennsylvania`,
       filename = paste0("~plots/Transit SDG/GTFS Maps/",
                         "Philadelphia, Pennsylvania",
                         "_GTFS_wholeCity.pdf"),
       units = "in",
       dpi = 600,
       width = 8.5,
       height = 11)

# Houston
# minimum density
transit_GTFS_mapList$`Houston, Texas` <-
  ggplot() +
  geom_sf(data = dryLand_list$`Houston, Texas`,
          fill = "#4d4d4d",
          # alpha = 0.5,
          color = NA) +
  # geom_sf(data = bbox_sfs$`Baltimore, Maryland`,
  #         fill = NA,
  #         color = NA) +
  geom_sf(data = hydrology_list$`Houston, Texas`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`Houston, Texas`,
          aes(size = factor(RTTYP,
                            levels = c("C", "O", "M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("C", "O", "M", "S", "U", "I")))) +
  geom_sf(data = isometric_list$Houston,
          color = "#999999",
          size = 0.01,
          # alpha = 0.5,
          # alpha = 0.3,
          aes(fill = walkshed_GTFS_denseEnough)) +
  scale_fill_gradientn(name = "Transit Stops\n(1/4 mile walk)",
                       na.value = "#bababa",
                       colors = c("red", "lightblue", "darkblue"),
                       values = scales::rescale(c(0, 0.1, max(isometric_list$Houston$walkshed_GTFS_denseEnough, na.rm = T))),
                       guide = "legend") +
  scale_color_manual(guide = FALSE,
                     breaks = c("C", "O", "I", "S", "U", "M"),
                     values = c("#F7F7F7", "#F7F7F7", "#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("C", "O", "I", "S", "U", "M"),
                    values = c(0.25, 0.25, 0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "Transit Access - OSM Data",
       subtitle = paste0("In areas with at least 3,000 people / square mile, ",
                         round(isometric_summary$Houston$GTFS_prop[2], 3) * 100,
                         "% of people have access to transit"))

ggsave(plot = transit_GTFS_mapList$`Houston, Texas`,
       filename = paste0("~plots/Transit SDG/GTFS Maps/",
                         "Houston, Texas",
                         "_GTFS.pdf"),
       units = "in",
       dpi = 600,
       width = 8.5,
       height = 11)

# San Francisco
# minimum density
transit_GTFS_mapList$`San Francisco, California` <-
  ggplot() +
  geom_sf(data = dryLand_list$`San Francisco, California`,
          fill = "#4d4d4d",
          # alpha = 0.5,
          color = NA) +
  # geom_sf(data = bbox_sfs$`Baltimore, Maryland`,
  #         fill = NA,
  #         color = NA) +
  geom_sf(data = hydrology_list$`San Francisco, California`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`San Francisco, California`,
          aes(size = factor(RTTYP,
                            levels = c("M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("M", "S", "U", "I")))) +
  geom_sf(data = isometric_list$`San Francisco`,
          color = "#999999",
          size = 0.01,
          # alpha = 0.5,
          # alpha = 0.3,
          aes(fill = walkshed_GTFS_denseEnough)) +
  scale_fill_gradientn(name = "Transit Stops\n(1/4 mile walk)",
                       na.value = "#bababa",
                       colors = c("red", "lightblue", "darkblue"),
                       values = scales::rescale(c(0, 0.1, max(isometric_list$`San Francisco`$walkshed_GTFS_denseEnough, na.rm = T))),
                       guide = "legend") +
  scale_color_manual(guide = FALSE,
                     breaks = c("I", "S", "U", "M"),
                     values = c("#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("I", "S", "U", "M"),
                    values = c(0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "Transit Access - GTFS Data",
       subtitle = paste0("In areas with at least 3,000 people / square mile, ",
                         round(isometric_summary$`San Francisco`$GTFS_prop[2], 3) * 100,
                         "% of people have access to transit"))

ggsave(plot = transit_GTFS_mapList$`San Francisco, California`,
       filename = paste0("~plots/Transit SDG/GTFS Maps/",
                         "San Francisco, California",
                         "_GTFS.pdf"),
       units = "in",
       dpi = 600,
       width = 8.5,
       height = 11)

# whole city
transit_GTFS_wholeCity_mapList$`San Francisco, California` <-
  ggplot() +
  geom_sf(data = dryLand_list$`San Francisco, California`,
          fill = "#4d4d4d",
          # alpha = 0.5,
          color = NA) +
  # geom_sf(data = bbox_sfs$`Baltimore, Maryland`,
  #         fill = NA,
  #         color = NA) +
  geom_sf(data = hydrology_list$`San Francisco, California`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`San Francisco, California`,
          aes(size = factor(RTTYP,
                            levels = c("M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("M", "S", "U", "I")))) +
  geom_sf(data = isometric_list$`San Francisco` %>% 
            mutate(walkshed_GTFS = ifelse(TotPop == 0, NA, walkshed_GTFS)),
          color = "#999999",
          size = 0.01,
          # alpha = 0.5,
          # alpha = 0.3,
          aes(fill = walkshed_GTFS)) +
  scale_fill_gradientn(name = "Transit Stops\n(1/4 mile walk)",
                       na.value = "#bababa",
                       colors = c("red", "lightblue", "darkblue"),
                       values = scales::rescale(c(0, 0.1, max(isometric_list$`San Francisco`$walkshed_OSM_denseEnough, na.rm = T))),
                       guide = "legend") +
  scale_color_manual(guide = FALSE,
                     breaks = c("I", "S", "U", "M"),
                     values = c("#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("I", "S", "U", "M"),
                    values = c(0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "Transit Access - GTFS Data",
       subtitle = paste0("In the whole city, ",
                         round(isometric_summary_allCells$`San Francisco`$GTFS_prop[2], 3) * 100,
                         "% of people have access to transit"))

ggsave(plot = transit_GTFS_wholeCity_mapList$`San Francisco, California`,
       filename = paste0("~plots/Transit SDG/GTFS Maps/",
                         "San Francisco, California",
                         "_GTFS_wholeCity.pdf"),
       units = "in",
       dpi = 600,
       width = 8.5,
       height = 11)

## 2c. ----
transit_overlap_mapList <- vector("list", length(bbox_sfs)) %>% 
  set_names(names(bbox_sfs))

# Baltimore
transit_overlap_mapList$`Baltimore, Maryland` <-
  ggplot() +
  geom_sf(data = dryLand_list$`Baltimore, Maryland`,
          fill = "#4d4d4d",
          # alpha = 0.5,
          color = NA) +
  # geom_sf(data = bbox_sfs$`Baltimore, Maryland`,
  #         fill = NA,
  #         color = NA) +
  geom_sf(data = hydrology_list$`Baltimore, Maryland`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`Baltimore, Maryland`,
          aes(size = factor(RTTYP,
                            levels = c("M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("M", "S", "U", "I")))) +
  geom_sf(data = isometric_list$Baltimore,
          color = "#999999",
          size = 0.01,
          # alpha = 0.5,
          # alpha = 0.3,
          aes(fill = factor(overlap,
                            levels = c("OSM only", "GTFS only", "Both", "Neither")))) +
  scale_fill_manual(name = "Data Source",
                    na.value = "#bababa",
                    labels = c("OSM only", "GTFS only", "Both", "Neither"),
                    breaks = c("OSM only", "GTFS only", "Both", "Neither"),
                    values = c("#d95f02", "#7570b3", "#41ab5d", "#e7298a"),
                    drop = FALSE) +
  scale_color_manual(guide = FALSE,
                     breaks = c("I", "S", "U", "M"),
                     values = c("#feb24c", "#feb24c", "#F7F7F7", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("I", "S", "U", "M"),
                    values = c(0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "Transit Access - Both Datasets",
       subtitle = paste0("In areas with at least 3,000 people / square mile, ",
                         round(isometric_summary$Baltimore$overlap_prop[2], 3) * 100,
                         "% of people have access to transit"))

ggsave(plot = transit_overlap_mapList$`Baltimore, Maryland`,
       filename = paste0("~plots/Transit SDG/Overlap Maps/",
                         "Baltimore, Maryland",
                         "_overlap.pdf"),
       units = "in",
       dpi = 600,
       width = 8.5,
       height = 11)

# Minneapolis
transit_overlap_mapList$`Minneapolis, Minnesota` <-
  ggplot() +
  geom_sf(data = dryLand_list$`Minneapolis, Minnesota`,
          fill = "#4d4d4d",
          # alpha = 0.5,
          color = NA) +
  # geom_sf(data = bbox_sfs$`Minneapolis, Minnesota`,
  #         fill = NA,
  #         color = NA) +
  geom_sf(data = hydrology_list$`Minneapolis, Minnesota`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`Minneapolis, Minnesota`,
          aes(size = factor(RTTYP,
                            levels = c("C", "M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("C", "M", "S", "U", "I")))) +
  geom_sf(data = isometric_list$Minneapolis,
          color = "#999999",
          size = 0.01,
          # alpha = 0.5,
          # alpha = 0.3,
          aes(fill = factor(overlap,
                            levels = c("OSM only", "GTFS only", "Both", "Neither")))) +
  scale_fill_manual(name = "Data Source",
                    na.value = "#bababa",
                    labels = c("OSM only", "GTFS only", "Both", "Neither"),
                    breaks = c("OSM only", "GTFS only", "Both", "Neither"),
                    values = c("#d95f02", "#7570b3", "#41ab5d", "#e7298a"),
                    drop = FALSE) +
  scale_color_manual(guide = FALSE,
                     breaks = c("C", "I", "S", "U", "M"),
                     values = c("#F7F7F7", "#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("C", "I", "S", "U", "M"),
                    values = c(0.25, 0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "Transit Access - Both Datasets",
       subtitle = paste0("In areas with at least 3,000 people / square mile, ",
                         round(isometric_summary$Minneapolis$overlap_prop[2], 3) * 100,
                         "% of people have access to transit"))

ggsave(plot = transit_overlap_mapList$`Minneapolis, Minnesota`,
       filename = paste0("~plots/Transit SDG/Overlap Maps/",
                         "Minneapolis, Minnesota",
                         "_overlap.pdf"),
       units = "in",
       dpi = 600,
       width = 8.5,
       height = 11)

# New Orleans
transit_overlap_mapList$`New Orleans, Louisiana` <-
  ggplot() +
  geom_sf(data = dryLand_list$`New Orleans, Louisiana`,
          fill = "#4d4d4d",
          # alpha = 0.5,
          color = NA) +
  # geom_sf(data = bbox_sfs$`New Orleans, Louisiana`,
  #         fill = NA,
  #         color = NA) +
  geom_sf(data = hydrology_list$`New Orleans, Louisiana`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`New Orleans, Louisiana`,
          aes(size = factor(RTTYP,
                            levels = c("M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("M", "S", "U", "I")))) +
  geom_sf(data = isometric_list$`New Orleans`,
          color = "#999999",
          size = 0.01,
          # alpha = 0.5,
          # alpha = 0.3,
          aes(fill = factor(overlap,
                            levels = c("OSM only", "GTFS only", "Both", "Neither")))) +
  scale_fill_manual(name = "Data Source",
                    na.value = "#bababa",
                    labels = c("OSM only", "GTFS only", "Both", "Neither"),
                    breaks = c("OSM only", "GTFS only", "Both", "Neither"),
                    values = c("#d95f02", "#7570b3", "#41ab5d", "#e7298a"),
                    drop = FALSE) +
  scale_color_manual(guide = FALSE,
                     breaks = c("I", "S", "U", "M"),
                     values = c("#feb24c", "#feb24c", "#F7F7F7", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("I", "S", "U", "M"),
                    values = c(0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "Transit Access - Both Datasets",
       subtitle = paste0("In areas with at least 3,000 people / square mile, ",
                         round(isometric_summary$`New Orleans`$overlap_prop[2], 3) * 100,
                         "% of people have access to transit"))

ggsave(plot = transit_overlap_mapList$`New Orleans, Louisiana`,
       filename = paste0("~plots/Transit SDG/Overlap Maps/",
                         "New Orleans, Louisiana",
                         "_overlap.pdf"),
       units = "in",
       dpi = 600,
       width = 8.5,
       height = 11)

# Philadelphia
transit_overlap_mapList$`Philadelphia, Pennsylvania` <-
  ggplot() +
  geom_sf(data = dryLand_list$`Philadelphia, Pennsylvania`,
          fill = "#4d4d4d",
          # alpha = 0.5,
          color = NA) +
  # geom_sf(data = bbox_sfs$`Philadelphia, Pennsylvania`,
  #         fill = NA,
  #         color = NA) +
  geom_sf(data = hydrology_list$`Philadelphia, Pennsylvania`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`Philadelphia, Pennsylvania`,
          aes(size = factor(RTTYP,
                            levels = c("O", "M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("O", "M", "S", "U", "I")))) +
  geom_sf(data = isometric_list$Philadelphia,
          color = "#999999",
          size = 0.01,
          # alpha = 0.5,
          # alpha = 0.3,
          aes(fill = factor(overlap,
                            levels = c("OSM only", "GTFS only", "Both", "Neither")))) +
  scale_fill_manual(name = "Data Source",
                    na.value = "#bababa",
                    labels = c("OSM only", "GTFS only", "Both", "Neither"),
                    breaks = c("OSM only", "GTFS only", "Both", "Neither"),
                    values = c("#d95f02", "#7570b3", "#41ab5d", "#e7298a"),
                    drop = FALSE) +
  scale_color_manual(guide = FALSE,
                     breaks = c("O", "I", "S", "U", "M"),
                     values = c("#F7F7F7", "#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("O", "I", "S", "U", "M"),
                    values = c(0.25, 0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "Transit Access - Both Datasets",
       subtitle = paste0("In areas with at least 3,000 people / square mile, ",
                         round(isometric_summary$Philadelphia$overlap_prop[2], 3) * 100,
                         "% of people have access to transit"))

ggsave(plot = transit_overlap_mapList$`Philadelphia, Pennsylvania`,
       filename = paste0("~plots/Transit SDG/Overlap Maps/",
                         "Philadelphia, Pennsylvania",
                         "_overlap.pdf"),
       units = "in",
       dpi = 600,
       width = 8.5,
       height = 11)

# Houston
transit_overlap_mapList$`Houston, Texas` <-
  ggplot() +
  geom_sf(data = dryLand_list$`Houston, Texas`,
          fill = "#4d4d4d",
          # alpha = 0.5,
          color = NA) +
  # geom_sf(data = bbox_sfs$`Houston, Texas`,
  #         fill = NA,
  #         color = NA) +
  geom_sf(data = hydrology_list$`Houston, Texas`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`Houston, Texas`,
          aes(size = factor(RTTYP,
                            levels = c("C", "O", "M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("C", "O", "M", "S", "U", "I")))) +
  geom_sf(data = isometric_list$Houston,
          color = "#999999",
          size = 0.01,
          # alpha = 0.5,
          # alpha = 0.3,
          aes(fill = factor(overlap,
                            levels = c("OSM only", "GTFS only", "Both", "Neither")))) +
  scale_fill_manual(name = "Data Source",
                    na.value = "#bababa",
                    labels = c("OSM only", "GTFS only", "Both", "Neither"),
                    breaks = c("OSM only", "GTFS only", "Both", "Neither"),
                    values = c("#d95f02", "#7570b3", "#41ab5d", "#e7298a"),
                    drop = FALSE) +
  scale_color_manual(guide = FALSE,
                     breaks = c("C", "O", "I", "S", "U", "M"),
                     values = c("#F7F7F7", "#F7F7F7", "#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("C", "O", "I", "S", "U", "M"),
                    values = c(0.25, 0.25, 0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "Transit Access - Both Datasets",
       subtitle = paste0("In areas with at least 3,000 people / square mile, ",
                         round(isometric_summary$Houston$overlap_prop[2], 3) * 100,
                         "% of people have access to transit"))

ggsave(plot = transit_overlap_mapList$`Houston, Texas`,
       filename = paste0("~plots/Transit SDG/Overlap Maps/",
                         "Houston, Texas",
                         "_overlap.pdf"),
       units = "in",
       dpi = 600,
       width = 8.5,
       height = 11)

# San Francisco
transit_overlap_mapList$`San Francisco, California` <-
  ggplot() +
  geom_sf(data = dryLand_list$`San Francisco, California`,
          fill = "#4d4d4d",
          # alpha = 0.5,
          color = NA) +
  # geom_sf(data = bbox_sfs$`San Francisco, California`,
  #         fill = NA,
  #         color = NA) +
  geom_sf(data = hydrology_list$`San Francisco, California`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`San Francisco, California`,
          aes(size = factor(RTTYP,
                            levels = c("M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("M", "S", "U", "I")))) +
  geom_sf(data = isometric_list$`San Francisco`,
          color = "#999999",
          size = 0.01,
          # alpha = 0.5,
          # alpha = 0.3,
          aes(fill = factor(overlap,
                            levels = c("OSM only", "GTFS only", "Both", "Neither")))) +
  scale_fill_manual(name = "Data Source",
                    na.value = "#bababa",
                    labels = c("OSM only", "GTFS only", "Both", "Neither"),
                    breaks = c("OSM only", "GTFS only", "Both", "Neither"),
                    values = c("#d95f02", "#7570b3", "#41ab5d", "#e7298a"),
                    drop = FALSE) +
  scale_color_manual(guide = FALSE,
                     breaks = c("I", "S", "U", "M"),
                     values = c("#feb24c", "#feb24c", "#F7F7F7", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("I", "S", "U", "M"),
                    values = c(0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "Transit Access - Both Datasets",
       subtitle = paste0("In areas with at least 3,000 people / square mile, ",
                         round(isometric_summary$`San Francisco`$overlap_prop[2], 3) * 100,
                         "% of people have access to transit"))

ggsave(plot = transit_overlap_mapList$`San Francisco, California`,
       filename = paste0("~plots/Transit SDG/Overlap Maps/",
                         "San Francisco, California",
                         "_overlap.pdf"),
       units = "in",
       dpi = 600,
       width = 8.5,
       height = 11)

## 1. Export as png ----
map2(transit_point_maps,
     names(transit_point_maps),
    ~ ggsave(plot = .x,
             units = "in",
             device = "png",
             height = 8,
             dpi = 72,
             filename = paste("~plots/31_transit_point_maps/",
                              .y,
                              "_transitStops.png",
                              sep = "")))