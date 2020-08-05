##########################################################################
# This script:
# 1. Makes the OSM maps
# 2. Makes the Open Data maps
# 3. Makes the combined maps
#
# Exports: 
# 1. 
# 
# To-do:
#
##########################################################################

## 1. ----
source("~scripts/10 - Data admin.R")
parks_osmData_list <- readRDS("~objects/10/13_parks_osmData_list.rds")
parks_osmData_combined <- readRDS("~objects/20/23_parks_osmData_combined.rds")
parks_openData_list <- readRDS("~objects/10/13_parks_openData_list.rds")
land_list <- readRDS("~objects/30/33_land_list.rds")
dryLand_list <- readRDS("~objects/10/14_dryLand_list.rds")
hydrology_list <- readRDS("~objects/10/14_hydrology_list.rds")
road_list <- readRDS("~objects/10/14_road_list.rds")
openSpace_results <- readRDS("~objects/30/33_openSpace_results.rds")

OpenSpace_OSM_mapList <- vector("list", length(bbox_sfs)) %>% 
  set_names(names(bbox_sfs))

# Baltimore ----
OpenSpace_OSM_mapList$`Baltimore, Maryland` <- ggplot() +
  geom_sf(data = dryLand_list$`Baltimore, Maryland`,
          fill = "#f0f0f0",
          # alpha = 0.5,
          color = NA) +
  geom_sf(data = bbox_sfs$`Baltimore, Maryland`,
          fill = "#E1E1E1",
          color = NA) +
  geom_sf(data = parks_osmData_combined$`Baltimore, Maryland`,
          aes(fill = parkType),
          size = 0.01) +
  geom_sf(data = hydrology_list$`Baltimore, Maryland`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`Baltimore, Maryland`,
          aes(size = factor(RTTYP,
                            levels = c("M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("M", "S", "U", "I")))) +
  geom_sf(data = bbox_sfs$`Baltimore, Maryland`,
          fill = NA,
          color = "black",
          size = 0.01) +
  scale_fill_manual(name = "Open Space",
                    labels = c("Park", "Cemetery", "Nature Reserve / Garden", "Playground / Square"),
                    breaks = c("Park", "Cemetery", "Reserve/Garden", "Playground/Square"),
                    values = c("#41ab5d", "#c7e9c0", "#006d2c", "#807dba")) +
  scale_color_manual(guide = FALSE,
                     breaks = c("I", "S", "U", "M"),
                     values = c("#feb24c", "#feb24c", "#F7F7F7", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("I", "S", "U", "M"),
                    values = c(0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "OpenStreetMap Public Space",
       subtitle = paste0(openSpace_results$OSM_sqMiles[1], " square miles of public space"))

# ggsave(plot = OpenSpace_OSM_mapList$`Baltimore, Maryland`,
#        filename = "~plots/Open Space SDG/OSM Maps/test.pdf",
#        units = "in",
#        width = 8.5,
#        height = 11)

# Minneapolis ----
OpenSpace_OSM_mapList$`Minneapolis, Minnesota` <- ggplot() +
  geom_sf(data = dryLand_list$`Minneapolis, Minnesota`,
          fill = "#f0f0f0",
          # alpha = 0.5,
          color = NA) +
  geom_sf(data = bbox_sfs$`Minneapolis, Minnesota`,
          fill = "#E1E1E1",
          color = NA) +
  geom_sf(data = parks_osmData_combined$`Minneapolis, Minnesota`,
          aes(fill = parkType),
          size = 0.01) +
  geom_sf(data = hydrology_list$`Minneapolis, Minnesota`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`Minneapolis, Minnesota`,
          aes(size = factor(RTTYP,
                            levels = c("C", "M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("C", "M", "S", "U", "I")))) +
  geom_sf(data = bbox_sfs$`Minneapolis, Minnesota`,
          fill = NA,
          color = "black",
          size = 0.01) +
  scale_fill_manual(name = "Open Space",
                    labels = c("Park", "Cemetery", "Nature Reserve / Garden", "Playground / Square"),
                    breaks = c("Park", "Cemetery", "Reserve/Garden", "Playground/Square"),
                    values = c("#41ab5d", "#c7e9c0", "#006d2c", "#807dba")) +
  scale_color_manual(guide = FALSE,
                     breaks = c("C", "I", "S", "U", "M"),
                     values = c("#F7F7F7", "#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("C", "I", "S", "U", "M"),
                    values = c(0.25, 0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "OpenStreetMap Public Space",
       subtitle = paste0(openSpace_results$OSM_sqMiles[2], " square miles of public space"))

# New Orleans ----
OpenSpace_OSM_mapList$`New Orleans, Louisiana` <- ggplot() +
  geom_sf(data = dryLand_list$`New Orleans, Louisiana`,
          fill = "#f0f0f0",
          # alpha = 0.5,
          color = NA) +
  geom_sf(data = bbox_sfs$`New Orleans, Louisiana`,
          fill = "#E1E1E1",
          color = NA) +
  geom_sf(data = parks_osmData_combined$`New Orleans, Louisiana`,
          aes(fill = parkType),
          size = 0.01) +
  geom_sf(data = hydrology_list$`New Orleans, Louisiana`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`New Orleans, Louisiana`,
          aes(size = factor(RTTYP,
                            levels = c("M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("M", "S", "U", "I")))) +
  geom_sf(data = bbox_sfs$`New Orleans, Louisiana`,
          fill = NA,
          color = "black",
          size = 0.01) +
  scale_fill_manual(name = "Open Space",
                    labels = c("Park", "Cemetery", "Nature Reserve / Garden", "Playground / Square"),
                    breaks = c("Park", "Cemetery", "Reserve/Garden", "Playground/Square"),
                    values = c("#41ab5d", "#c7e9c0", "#006d2c", "#807dba")) +
  scale_color_manual(guide = FALSE,
                     breaks = c("I", "S", "U", "M"),
                     values = c("#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("I", "S", "U", "M"),
                    values = c(0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "OpenStreetMap Public Space",
       subtitle = paste0(openSpace_results$OSM_sqMiles[3], " square miles of public space"))


# Philadelphia ----
OpenSpace_OSM_mapList$`Philadelphia, Pennsylvania` <- ggplot() +
  geom_sf(data = dryLand_list$`Philadelphia, Pennsylvania`,
          fill = "#f0f0f0",
          # alpha = 0.5,
          color = NA) +
  geom_sf(data = bbox_sfs$`Philadelphia, Pennsylvania`,
          fill = "#E1E1E1",
          color = NA) +
  geom_sf(data = parks_osmData_combined$`Philadelphia, Pennsylvania`,
          aes(fill = parkType),
          size = 0.01) +
  geom_sf(data = hydrology_list$`Philadelphia, Pennsylvania`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`Philadelphia, Pennsylvania`,
          aes(size = factor(RTTYP,
                            levels = c("O", "M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("O", "M", "S", "U", "I")))) +
  geom_sf(data = bbox_sfs$`Philadelphia, Pennsylvania`,
          fill = NA,
          color = "black",
          size = 0.01) +
  scale_fill_manual(name = "Open Space",
                    labels = c("Park", "Cemetery", "Nature Reserve / Garden", "Playground / Square"),
                    breaks = c("Park", "Cemetery", "Reserve/Garden", "Playground/Square"),
                    values = c("#41ab5d", "#c7e9c0", "#006d2c", "#807dba")) +
  scale_color_manual(guide = FALSE,
                     breaks = c("O", "I", "S", "U", "M"),
                     values = c("#F7F7F7", "#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("O", "I", "S", "U", "M"),
                    values = c(0.25, 0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "OpenStreetMap Public Space",
       subtitle = paste0(openSpace_results$OSM_sqMiles[4], " square miles of public space"))

# Houston ----
OpenSpace_OSM_mapList$`Houston, Texas` <- ggplot() +
  geom_sf(data = dryLand_list$`Houston, Texas`,
          fill = "#f0f0f0",
          # alpha = 0.5,
          color = NA) +
  geom_sf(data = bbox_sfs$`Houston, Texas`,
          fill = "#E1E1E1",
          color = NA) +
  geom_sf(data = parks_osmData_combined$`Houston, Texas`,
          aes(fill = parkType),
          size = 0.01) +
  geom_sf(data = hydrology_list$`Houston, Texas`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`Houston, Texas`,
          aes(size = factor(RTTYP,
                            levels = c("C", "O", "M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("C", "O", "M", "S", "U", "I")))) +
  geom_sf(data = bbox_sfs$`Houston, Texas`,
          fill = NA,
          color = "black",
          size = 0.01) +
  scale_fill_manual(name = "Open Space",
                    labels = c("Park", "Cemetery", "Nature Reserve / Garden", "Playground / Square"),
                    breaks = c("Park", "Cemetery", "Reserve/Garden", "Playground/Square"),
                    values = c("#41ab5d", "#c7e9c0", "#006d2c", "#807dba")) +
  scale_color_manual(guide = FALSE,
                     breaks = c("C", "O", "I", "S", "U", "M"),
                     values = c("#F7F7F7", "#F7F7F7", "#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("C", "O", "I", "S", "U", "M"),
                    values = c(0.25, 0.25, 0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "OpenStreetMap Public Space",
       subtitle = paste0(openSpace_results$OSM_sqMiles[5], " square miles of public space"))

# San Francisco ----
OpenSpace_OSM_mapList$`San Francisco, California` <- ggplot() +
  geom_sf(data = dryLand_list$`San Francisco, California`,
          fill = "#E1E1E1",
          # alpha = 0.5,
          color = NA) +
  geom_sf(data = bbox_sfs$`San Francisco, California`,
          fill = "#E1E1E1",
          color = NA) +
  geom_sf(data = parks_osmData_combined$`San Francisco, California`,
          aes(fill = parkType),
          size = 0.01) +
  geom_sf(data = hydrology_list$`San Francisco, California`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`San Francisco, California`,
          aes(size = factor(RTTYP,
                            levels = c("M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("M", "S", "U", "I")))) +
  geom_sf(data = bbox_sfs$`San Francisco, California`,
          fill = NA,
          color = "black",
          size = 0.01) +
  scale_fill_manual(name = "Open Space",
                    labels = c("Park", "Cemetery", "Nature Reserve / Garden", "Playground / Square"),
                    breaks = c("Park", "Cemetery", "Reserve/Garden", "Playground/Square"),
                    values = c("#41ab5d", "#c7e9c0", "#006d2c", "#807dba")) +
  scale_color_manual(guide = FALSE,
                     breaks = c("I", "S", "U", "M"),
                     values = c("#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("I", "S", "U", "M"),
                    values = c(0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "OpenStreetMap Public Space",
       subtitle = paste0(openSpace_results$OSM_sqMiles[6], " square miles of public space"))

## 2. ----
OpenSpace_openData_mapList <- vector("list", length(bbox_sfs)) %>% 
  set_names(names(bbox_sfs))

# Baltimore ----
OpenSpace_openData_mapList$`Baltimore, Maryland` <- ggplot() +
  geom_sf(data = dryLand_list$`Baltimore, Maryland`,
          fill = "#f0f0f0",
          # alpha = 0.5,
          color = NA) +
  geom_sf(data = bbox_sfs$`Baltimore, Maryland`,
          fill = "#E1E1E1",
          color = NA) +
  geom_sf(data = parks_openData_list$`Baltimore, Maryland`,
          aes(fill = LU_2008),
          size = 0.01) +
  geom_sf(data = hydrology_list$`Baltimore, Maryland`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`Baltimore, Maryland`,
          aes(size = factor(RTTYP,
                            levels = c("M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("M", "S", "U", "I")))) +
  geom_sf(data = bbox_sfs$`Baltimore, Maryland`,
          fill = NA,
          color = "black",
          size = 0.01) +
  scale_fill_manual(name = "Open Space",
                    labels = c("Park / Recreation", "Cemetery", "Nature Reserve"),
                    breaks = c("Parks/Recreation", "Cemetery", "Natural Area"),
                    values = c("#41ab5d", "#c7e9c0", "#006d2c")) +
  scale_color_manual(guide = FALSE,
                     breaks = c("I", "S", "U", "M"),
                     values = c("#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("I", "S", "U", "M"),
                    values = c(0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "OpenData Public Space",
       subtitle = paste0(openSpace_results$openData_sqMiles[1], " square miles of public space"))

# Minneapolis ----
OpenSpace_openData_mapList$`Minneapolis, Minnesota` <- ggplot() +
  geom_sf(data = dryLand_list$`Minneapolis, Minnesota`,
          fill = "#f0f0f0",
          # alpha = 0.5,
          color = NA) +
  geom_sf(data = bbox_sfs$`Minneapolis, Minnesota`,
          fill = "#E1E1E1",
          color = NA) +
  geom_sf(data = parks_openData_list$`Minneapolis, Minnesota` %>% 
            mutate(Park = "Park"),
          aes(fill = Park),
          size = 0.01) +
  geom_sf(data = hydrology_list$`Minneapolis, Minnesota`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`Minneapolis, Minnesota`,
          aes(size = factor(RTTYP,
                            levels = c("C", "M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("C", "M", "S", "U", "I")))) +
  geom_sf(data = bbox_sfs$`Minneapolis, Minnesota`,
          fill = NA,
          color = "black",
          size = 0.01) +
  scale_fill_manual(name = "Open Space",
                    labels = c("Park"),
                    breaks = c("Park"),
                    values = c("#41ab5d")) +
  scale_color_manual(guide = FALSE,
                     breaks = c("C", "I", "S", "U", "M"),
                     values = c("#F7F7F7", "#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("C", "I", "S", "U", "M"),
                    values = c(0.25, 0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "OpenData Public Space",
       subtitle = paste0(openSpace_results$openData_sqMiles[2], " square miles of public space"))

# New Orleans ----
parks_openData_list$`New Orleans, Louisiana` <- parks_openData_list$`New Orleans, Louisiana` %>% 
  mutate(parkType = case_when(ALL_PURPOSE == "Y" ~ "All Purpose Park",
                              ALL_PURPOSE != "Y" &
                                (BASKETBALL_COURT == "Y" |
                                   TENNIS_COURT == "Y" |
                                   BASEBALL_DIAMOND == "Y") ~ "Ballfield / Court only",
                              ALL_PURPOSE != "Y" &
                                (BASKETBALL_COURT != "Y" &
                                   TENNIS_COURT != "Y" &
                                   BASEBALL_DIAMOND != "Y") &
                                PLAYGROUND == "Y" ~ "Playground only",
                              TRUE ~ "All Purpose Park"))

OpenSpace_openData_mapList$`New Orleans, Louisiana` <- ggplot() +
  geom_sf(data = dryLand_list$`New Orleans, Louisiana`,
          fill = "#f0f0f0",
          # alpha = 0.5,
          color = NA) +
  geom_sf(data = bbox_sfs$`New Orleans, Louisiana`,
          fill = "#E1E1E1",
          color = NA) +
  geom_sf(data = parks_openData_list$`New Orleans, Louisiana`,
          aes(fill = parkType),
          size = 0.01) +
  geom_sf(data = hydrology_list$`New Orleans, Louisiana`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`New Orleans, Louisiana`,
          aes(size = factor(RTTYP,
                            levels = c("M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("M", "S", "U", "I")))) +
  geom_sf(data = bbox_sfs$`New Orleans, Louisiana`,
          fill = NA,
          color = "black",
          size = 0.01) +
  scale_fill_manual(name = "Open Space",
                    labels = c("All Purpose Park", "Ballfield / Court", "Playground"),
                    breaks = c("All Purpose Park", "Ballfield / Court only", "Playground only"),
                    values = c("#41ab5d", "#c7e9c0", "#807dba")) +
  scale_color_manual(guide = FALSE,
                     breaks = c("I", "S", "U", "M"),
                     values = c("#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("I", "S", "U", "M"),
                    values = c(0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "OpenStreetMap Public Space",
       subtitle = paste0(openSpace_results$openData_sqMiles[3], " square miles of public space"))


# Philadelphia ----
parks_openData_list$`Philadelphia, Pennsylvania` <- parks_openData_list$`Philadelphia, Pennsylvania` %>% 
  mutate(parkType = case_when(C_DIG2 == 62 ~ "Active Recreation",
                              C_DIG2 == 71 ~ "Park", 
                              C_DIG2 == 72 ~ "Cemetery"))

OpenSpace_openData_mapList$`Philadelphia, Pennsylvania` <- ggplot() +
  geom_sf(data = dryLand_list$`Philadelphia, Pennsylvania`,
          fill = "#f0f0f0",
          # alpha = 0.5,
          color = NA) +
  geom_sf(data = bbox_sfs$`Philadelphia, Pennsylvania`,
          fill = "#E1E1E1",
          color = NA) +
  geom_sf(data = parks_openData_list$`Philadelphia, Pennsylvania`,
          aes(fill = parkType),
          size = 0.01) +
  geom_sf(data = hydrology_list$`Philadelphia, Pennsylvania`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`Philadelphia, Pennsylvania`,
          aes(size = factor(RTTYP,
                            levels = c("O", "M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("O", "M", "S", "U", "I")))) +
  geom_sf(data = bbox_sfs$`Philadelphia, Pennsylvania`,
          fill = NA,
          color = "black",
          size = 0.01) +
  scale_fill_manual(name = "Open Space",
                    labels = c("Park", "Cemetery", "Ballfield, court, or golf course"),
                    breaks = c("Park", "Cemetery", "Active Recreation"),
                    values = c("#41ab5d", "#c7e9c0", "#006d2c")) +
  scale_color_manual(guide = FALSE,
                     breaks = c("O", "I", "S", "U", "M"),
                     values = c("#F7F7F7", "#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("O", "I", "S", "U", "M"),
                    values = c(0.25, 0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "OpenStreetMap Public Space",
       subtitle = paste0(openSpace_results$openData_sqMiles[4], " square miles of public space"))

# Houston ----
parks_openData_list$`Houston, Texas` <- parks_openData_list$`Houston, Texas` %>% 
  mutate(parkType = case_when(str_detect(Park_Type, "Square") ~ "Square" ,
                              str_detect(Park_Type, "Reserve") ~ "Nature Reserve",
                              TRUE ~ "Park"))

OpenSpace_openData_mapList$`Houston, Texas` <- ggplot() +
  geom_sf(data = dryLand_list$`Houston, Texas`,
          fill = "#f0f0f0",
          # alpha = 0.5,
          color = NA) +
  geom_sf(data = bbox_sfs$`Houston, Texas`,
          fill = "#E1E1E1",
          color = NA) +
  geom_sf(data = parks_openData_list$`Houston, Texas`,
          aes(fill = parkType),
          size = 0.01) +
  geom_sf(data = hydrology_list$`Houston, Texas`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`Houston, Texas`,
          aes(size = factor(RTTYP,
                            levels = c("C", "O", "M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("C", "O", "M", "S", "U", "I")))) +
  geom_sf(data = bbox_sfs$`Houston, Texas`,
          fill = NA,
          color = "black",
          size = 0.01) +
  scale_fill_manual(name = "Open Space",
                    labels = c("Park", "Nature Reserve", "Square"),
                    breaks = c("Park", "Nature Reserve", "Square"),
                    values = c("#41ab5d", "#006d2c", "#807dba")) +
  scale_color_manual(guide = FALSE,
                     breaks = c("C", "O", "I", "S", "U", "M"),
                     values = c("#F7F7F7", "#F7F7F7", "#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("C", "O", "I", "S", "U", "M"),
                    values = c(0.25, 0.25, 0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "OpenStreetMap Public Space",
       subtitle = paste0(openSpace_results$openData_sqMiles[5], " square miles of public space"))

# San Francisco ----
parks_openData_list$`San Francisco, California` <- parks_openData_list$`San Francisco, California` %>% 
  mutate(parkType = case_when(propertytype == "Civic Plaza or Square" ~ "Square",
                              propertytype %in% c("Community Garden", "Zoological Garden") ~ "Zoo / Garden",
                              propertytype %in% c("Concession", 
                                                  "Mini Park",
                                                  "Neighborhood Park or Playground",
                                                  "Parkway",
                                                  "Regional Park") ~ "Park"
                              ))

OpenSpace_openData_mapList$`San Francisco, California` <- ggplot() +
  geom_sf(data = dryLand_list$`San Francisco, California`,
          fill = "#f0f0f0",
          # alpha = 0.5,
          color = NA) +
  geom_sf(data = bbox_sfs$`San Francisco, California`,
          fill = "#E1E1E1",
          color = NA) +
  geom_sf(data = parks_openData_list$`San Francisco, California`,
          aes(fill = parkType),
          size = 0.01) +
  geom_sf(data = hydrology_list$`San Francisco, California`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`San Francisco, California`,
          aes(size = factor(RTTYP,
                            levels = c("M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("M", "S", "U", "I")))) +
  geom_sf(data = bbox_sfs$`San Francisco, California`,
          fill = NA,
          color = "black",
          size = 0.01) +
  scale_fill_manual(name = "Open Space",
                    labels = c("Park", "Zoo / Garden", "Square"),
                    breaks = c("Park", "Zoo / Garden", "Square"),
                    values = c("#41ab5d", "#006d2c", "#807dba")) +
  scale_color_manual(guide = FALSE,
                     breaks = c("I", "S", "U", "M"),
                     values = c("#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("I", "S", "U", "M"),
                    values = c(0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "OpenStreetMap Public Space",
       subtitle = paste0(openSpace_results$openData_sqMiles[6], " square miles of public space"))

## 3. ----
parks_comparison_list <- readRDS("~objects/30/33_parks_comparison_list.rds")

OpenSpace_overlap_mapList <- vector("list", length(bbox_sfs)) %>% 
  set_names(names(bbox_sfs))

# Baltimore ----
OpenSpace_overlap_mapList$`Baltimore, Maryland` <- ggplot() +
  geom_sf(data = dryLand_list$`Baltimore, Maryland`,
          fill = "#f0f0f0",
          # alpha = 0.5,
          color = NA) +
  geom_sf(data = bbox_sfs$`Baltimore, Maryland`,
          fill = "#E1E1E1",
          color = NA) +
  geom_sf(data = parks_comparison_list$`Baltimore, Maryland`,
          aes(fill = Source),
          color = NA,
          size = 0.01) +
  geom_sf(data = hydrology_list$`Baltimore, Maryland`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`Baltimore, Maryland`,
          aes(size = factor(RTTYP,
                            levels = c("M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("M", "S", "U", "I")))) +
  geom_sf(data = bbox_sfs$`Baltimore, Maryland`,
          fill = NA,
          color = "black",
          size = 0.01) +
  scale_fill_manual(name = "Data Source",
                    labels = c("OSM only", "Open Data only", "Both"),
                    breaks = c("OSM only", "Open Data only", "Both"),
                    values = c("#7570b3", "#d95f02", "#1b9e77")) +
  scale_color_manual(guide = FALSE,
                     breaks = c("I", "S", "U", "M"),
                     values = c("#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("I", "S", "U", "M"),
                    values = c(0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "OpenData Public Space",
       subtitle = paste0("OSM and OpenData datasets have ", openSpace_results$overlap_sqMiles[1], 
                         " square miles of overlap (", round(openSpace_results$overlap_combinedArea_perc[1] * 100), "% of combined area)"))

# Minneapolis ----
OpenSpace_overlap_mapList$`Minneapolis, Minnesota` <- ggplot() +
  geom_sf(data = dryLand_list$`Minneapolis, Minnesota`,
          fill = "#f0f0f0",
          # alpha = 0.5,
          color = NA) +
  geom_sf(data = bbox_sfs$`Minneapolis, Minnesota`,
          fill = "#E1E1E1",
          color = NA) +
  geom_sf(data = parks_comparison_list$`Minneapolis, Minnesota`,
          aes(fill = Source),
          color = NA,
          size = 0.01) +
  geom_sf(data = hydrology_list$`Minneapolis, Minnesota`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`Minneapolis, Minnesota`,
          aes(size = factor(RTTYP,
                            levels = c("C", "M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("C", "M", "S", "U", "I")))) +
  geom_sf(data = bbox_sfs$`Minneapolis, Minnesota`,
          fill = NA,
          color = "black",
          size = 0.01) +
  scale_fill_manual(name = "Data Source",
                    labels = c("OSM only", "Open Data only", "Both"),
                    breaks = c("OSM only", "Open Data only", "Both"),
                    values = c("#7570b3", "#d95f02", "#1b9e77")) +
  scale_color_manual(guide = FALSE,
                     breaks = c("C", "I", "S", "U", "M"),
                     values = c("#F7F7F7", "#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("C", "I", "S", "U", "M"),
                    values = c(0.25, 0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "OpenData Public Space",
       subtitle = paste0("OSM and OpenData datasets have ", openSpace_results$overlap_sqMiles[2], 
                         " square miles of overlap (", round(openSpace_results$overlap_combinedArea_perc[2] * 100), "% of combined area)"))

# New Orleans ----
OpenSpace_overlap_mapList$`New Orleans, Louisiana` <- ggplot() +
  geom_sf(data = dryLand_list$`New Orleans, Louisiana`,
          fill = "#f0f0f0",
          # alpha = 0.5,
          color = NA) +
  geom_sf(data = bbox_sfs$`New Orleans, Louisiana`,
          fill = "#E1E1E1",
          color = NA) +
  geom_sf(data = parks_comparison_list$`New Orleans, Louisiana`,
          aes(fill = Source),
          color = NA,
          size = 0.01) +
  geom_sf(data = hydrology_list$`New Orleans, Louisiana`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`New Orleans, Louisiana`,
          aes(size = factor(RTTYP,
                            levels = c("M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("M", "S", "U", "I")))) +
  geom_sf(data = bbox_sfs$`New Orleans, Louisiana`,
          fill = NA,
          color = "black",
          size = 0.01) +
  scale_fill_manual(name = "Data Source",
                    labels = c("OSM only", "Open Data only", "Both"),
                    breaks = c("OSM only", "Open Data only", "Both"),
                    values = c("#7570b3", "#d95f02", "#1b9e77")) +
  scale_color_manual(guide = FALSE,
                     breaks = c("I", "S", "U", "M"),
                     values = c("#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("I", "S", "U", "M"),
                    values = c(0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "OpenData Public Space",
       subtitle = paste0("OSM and OpenData datasets have ", openSpace_results$overlap_sqMiles[3], 
                         " square miles of overlap (", round(openSpace_results$overlap_combinedArea_perc[3] * 100), "% of combined area)"))


# Philadelphia ----
OpenSpace_overlap_mapList$`Philadelphia, Pennsylvania` <- ggplot() +
  geom_sf(data = dryLand_list$`Philadelphia, Pennsylvania`,
          fill = "#f0f0f0",
          # alpha = 0.5,
          color = NA) +
  geom_sf(data = bbox_sfs$`Philadelphia, Pennsylvania`,
          fill = "#E1E1E1",
          color = NA) +
  geom_sf(data = parks_comparison_list$`Philadelphia, Pennsylvania`,
          aes(fill = Source),
          color = NA,
          size = 0.01) +
  geom_sf(data = hydrology_list$`Philadelphia, Pennsylvania`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`Philadelphia, Pennsylvania`,
          aes(size = factor(RTTYP,
                            levels = c("O", "M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("O", "M", "S", "U", "I")))) +
  geom_sf(data = bbox_sfs$`Philadelphia, Pennsylvania`,
          fill = NA,
          color = "black",
          size = 0.01) +
  scale_fill_manual(name = "Data Source",
                    labels = c("OSM only", "Open Data only", "Both"),
                    breaks = c("OSM only", "Open Data only", "Both"),
                    values = c("#7570b3", "#d95f02", "#1b9e77")) +
  scale_color_manual(guide = FALSE,
                     breaks = c("O", "I", "S", "U", "M"),
                     values = c("#F7F7F7", "#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("O", "I", "S", "U", "M"),
                    values = c(0.25, 0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "OpenData Public Space",
       subtitle = paste0("OSM and OpenData datasets have ", openSpace_results$overlap_sqMiles[4], 
                         " square miles of overlap (", round(openSpace_results$overlap_combinedArea_perc[4] * 100), "% of combined area)"))

# Houston ----
OpenSpace_overlap_mapList$`Houston, Texas` <- ggplot() +
  geom_sf(data = dryLand_list$`Houston, Texas`,
          fill = "#f0f0f0",
          # alpha = 0.5,
          color = NA) +
  geom_sf(data = bbox_sfs$`Houston, Texas`,
          fill = "#E1E1E1",
          color = NA) +
  geom_sf(data = parks_comparison_list$`Houston, Texas`,
          aes(fill = Source),
          color = NA,
          size = 0.01) +
  geom_sf(data = hydrology_list$`Houston, Texas`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`Houston, Texas`,
          aes(size = factor(RTTYP,
                            levels = c("C", "O", "M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("C", "O", "M", "S", "U", "I")))) +
  geom_sf(data = bbox_sfs$`Houston, Texas`,
          fill = NA,
          color = "black",
          size = 0.01) +
  scale_fill_manual(name = "Data Source",
                    labels = c("OSM only", "Open Data only", "Both"),
                    breaks = c("OSM only", "Open Data only", "Both"),
                    values = c("#7570b3", "#d95f02", "#1b9e77")) +
  scale_color_manual(guide = FALSE,
                     breaks = c("C", "O", "I", "S", "U", "M"),
                     values = c("#F7F7F7", "#F7F7F7", "#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("C", "O", "I", "S", "U", "M"),
                    values = c(0.25, 0.25, 0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "OpenData Public Space",
       subtitle = paste0("OSM and OpenData datasets have ", openSpace_results$overlap_sqMiles[5], 
                         " square miles of overlap (", round(openSpace_results$overlap_combinedArea_perc[5] * 100), "% of combined area)"))

# San Francisco ----
OpenSpace_overlap_mapList$`San Francisco, California` <- ggplot() +
  geom_sf(data = dryLand_list$`San Francisco, California`,
          fill = "#f0f0f0",
          # alpha = 0.5,
          color = NA) +
  geom_sf(data = bbox_sfs$`San Francisco, California`,
          fill = "#E1E1E1",
          color = NA) +
  geom_sf(data = parks_comparison_list$`San Francisco, California`,
          aes(fill = Source),
          color = NA,
          size = 0.01) +
  geom_sf(data = hydrology_list$`San Francisco, California`,
          color = NA,
          fill = "#97DBF2") +
  geom_sf(data = road_list$`San Francisco, California`,
          aes(size = factor(RTTYP,
                            levels = c("M", "S", "U", "I")),
              color = factor(RTTYP,
                             levels = c("M", "S", "U", "I")))) +
  geom_sf(data = bbox_sfs$`San Francisco, California`,
          fill = NA,
          color = "black",
          size = 0.01) +
  scale_fill_manual(name = "Data Source",
                    labels = c("OSM only", "Open Data only", "Both"),
                    breaks = c("OSM only", "Open Data only", "Both"),
                    values = c("#7570b3", "#d95f02", "#1b9e77")) +
  scale_color_manual(guide = FALSE,
                     breaks = c("I", "S", "U", "M"),
                     values = c("#feb24c", "#feb24c", "#feb24c", "#F7F7F7")) +
  scale_size_manual(guide = FALSE,
                    breaks = c("I", "S", "U", "M"),
                    values = c(0.5, 0.25, 0.25, 0.25)) +
  mapTheme() +
  labs(title = "OpenData Public Space",
       subtitle = paste0("OSM and OpenData datasets have ", openSpace_results$overlap_sqMiles[6], 
                         " square miles of overlap (", round(openSpace_results$overlap_combinedArea_perc[6] * 100), "% of combined area)"))

## 1. Export as pdf ----
walk2(OpenSpace_OSM_mapList,
      names(OpenSpace_OSM_mapList),
     ~ ggsave(plot = .x,
              filename = paste0("~plots/Open Space SDG/OSM Maps/",
                                .y,
                                "_OSM.pdf"),
              units = "in",
              dpi = 600,
              width = 8.5,
              height = 11))

## 2. Export as pdf ----
walk2(OpenSpace_openData_mapList,
      names(OpenSpace_openData_mapList),
      ~ ggsave(plot = .x,
               filename = paste0("~plots/Open Space SDG/Open Data Maps/",
                                 .y,
                                 "_OpenData.pdf"),
               units = "in",
               dpi = 600,
               width = 8.5,
               height = 11))

## 3. Export as pdf ----
walk2(OpenSpace_overlap_mapList,
      names(OpenSpace_overlap_mapList),
      ~ ggsave(plot = .x,
               filename = paste0("~plots/Open Space SDG/Overlap Maps/",
                                 .y,
                                 "_Overlap.pdf"),
               units = "in",
               dpi = 600,
               width = 8.5,
               height = 11))
