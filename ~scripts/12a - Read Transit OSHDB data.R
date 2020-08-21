##########################################################################
# This script:
# 1. Downloads the OSHDB historical data for transit features in each city
# 2. Creates plots
#
# Exports: 
# 1. 
# 
# To-do:
# 1. 
##########################################################################

source("~scripts/10 - Data admin.R")

## 1a. ----
# Admin for API queries
base_url <- "https://api.ohsome.org/v1"
api_metadata <- GET(url= paste(base_url, "/metadata", sep = "")) %>% 
  content("text") %>% 
  fromJSON()

## 1b. ----
# query bounding boxes
oshdb_bboxes <- map(
  bbox_sfs,
  ~ .x %>% 
    st_transform(4326) %>% 
    tmaptools::bb(output = "bbox") %>% 
    paste(collapse = ", ")
  )

## 1c. ----
api_key <- "highway"
api_value <- "bus_stop"

api_list <- list(bus = "highway=bus_stop",
                 tram = "railway=tram_stop",
                 train = "railway=station",
                 ferry = "amenity=ferry_terminal")

## 1d. ----
intervals <- "2007-11-01/2020-06-29/P1M"


## 2. ----
oshdb_transit_histories <- vector("list", length(oshdb_bboxes[1:6])) %>% 
  set_names(names(oshdb_bboxes[1:6]))

for (city in names(oshdb_transit_histories)) {
  
  print(city)
  
  tmp <- vector("list", length(api_list)) %>% 
    set_names(names(api_list))
  
  for (mode in names(tmp)) {
    
    tmp[[mode]] <- 
      GET(url = paste(base_url, "/elements/count", sep = ""),
          query = list(
            bboxes = oshdb_bboxes[[city]],
            filter = api_list[[mode]],
            time = intervals)) %>% 
      content(as = "text") %>% 
      fromJSON() %>% 
      .$result %>% 
      rename(!!mode := value)
    
  }
  
  oshdb_transit_histories[[city]] <- 
    reduce(tmp,
           left_join,
           by = "timestamp") %>% 
    mutate(total = bus + tram + train + ferry)

}

oshdb_user_histories <- vector("list", length(oshdb_bboxes[1:6])) %>% 
  set_names(names(oshdb_bboxes[1:6]))

for (city in names(oshdb_user_histories)) {
  
  print(city)
  
  tmp <- vector("list", length(api_list)) %>% 
    set_names(names(api_list))
  
  for (mode in names(tmp)) {
    
    tmp[[mode]] <- 
      GET(url = paste(base_url, "/users/count", sep = ""),
          query = list(
            bboxes = oshdb_bboxes[[city]],
            filter = api_list[[mode]],
            time = intervals)) %>% 
      content(as = "text") %>% 
      fromJSON() %>% 
      .$result %>% 
      mutate(rolling_3mo = (value + lag(value, 1) + lag(value, 2)) / 3) %>% 
      rename(!!mode := rolling_3mo) %>% 
        dplyr::select(-value, -fromTimestamp)
    
  }
  
  oshdb_user_histories[[city]] <- 
    reduce(tmp,
           left_join,
           by = "toTimestamp") %>% 
    mutate(total = bus + tram + train + ferry)
  
}

## 3. ----


oshdb_plots <- map2(
  oshdb_transit_histories,
  oshdb_user_histories,
  ~ {
    
    feature_history <- .x %>% 
      gather(key = "mode",
             value = "stop_count",
             bus:total) %>% 
      mutate(mode = factor(mode,
                           levels = c("bus", "tram", "train", "ferry", "total"))) %>% 
      ggplot(aes(color = mode,
                 x = as.Date(timestamp),
                 y = stop_count)) +
      geom_line(size = 1) +
      plotTheme() +
      scale_color_manual(name = "Mode",
                         breaks = c("bus", "tram", "train", "ferry", "total"),
                         values = c('#377eb8','#4daf4a','#984ea3', '#ff7f00', '#e41a1c'),
                         labels = c("Bus", "Tram / Streetcar", "Train / Subway", "Ferry / Water Taxi", "Total")) +
      scale_x_date(date_breaks = "2 years",
                   labels = lubridate::year) +
      labs(title = "Transit Stops Mapped Over Time",
           caption = "Source: OSHDB, ohsome API",
           x = "Date",
           y = "Number of Stops") +
      theme(legend.position = "bottom")
    
    user_history <- .y %>% 
      ggplot(aes(x = as.Date(toTimestamp),
                 y = total)) +
      geom_line(size = 1,
                color = "#377eb8") +
      plotTheme() +
      scale_x_date(date_breaks = "2 years",
                   labels = lubridate::year) +
      scale_y_continuous(breaks = scales::pretty_breaks()) +
      labs(title = "Unique Contributors to OSM Transit Features, 3-month Rolling Average",
           caption = "Source: OSHDB, ohsome API",
           x = "Date",
           y = "Average Contributors")
    
    arrangeGrob(feature_history,
                user_history,
                ncol = 1)
    
  }
                    )

walk2(oshdb_plots,
     names(oshdb_plots),
     ~ ggsave(plot = .x,
              filename = paste0("~plots/Transit SDG/OSM Maps/",
                                .y,
                                "_OSHDB.pdf"),
              unit = "in",
              width = 9,
              height = 5))


## 2. Export as rds ----
saveRDS(oshdb_transit_histories,
        "~objects/10/12a_oshdb_transit_histories.rds")
saveRDS(oshdb_user_histories,
        "~objects/10/12a_oshdb_user_histories.rds")
saveRDS(oshdb_plots,
        "~objects/10/oshdb_plots.rds")
