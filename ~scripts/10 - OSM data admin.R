##########################################################################
# This script assigns any variables used when collecting OSM data for both SDGs:
# 1. Bounding boxes for each sdg city
#
# Exports: 
# 1. 
# 
# To-do:
#
##########################################################################

## 1. ----
bboxes <- map(set_names(sdg_cities), # set names adds names to the list output
              ~ getbb(.x) %>% 
                opq())

## 2. ---- 






