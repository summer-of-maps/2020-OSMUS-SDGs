##########################################################################
# This script:
# 1. Adds a population density column (in square miles)
# 2. Defines a desired cell size
# 3. Turns the block groups into grid cells and
#     interpolate the population in each
#
# Exports: 
# 1. grids as 21_grids.RDS
# 
# To-do:
# 1. 
#
##########################################################################

# 1. ----
# BGs <- readRDS("~objects/10/11_BGs.RDS")
BGs <- map(BGs,
           ~ .x %>% 
             mutate(area = as.numeric(st_area(.)), # area is in square feet
                    popDens_sqMi = TotPop / 
                      conv_unit(area, # add pop density in square miles
                                from = "ft2",
                                to = "mi2")))

# tracts <- readRDS("~objects/10/11_tracts.RDS")
tracts <- map(tracts,
           ~ .x %>% 
             mutate(area = as.numeric(st_area(.)), # area is in square feet
                    popDens_sqMi = TotPop / 
                      conv_unit(area, # add pop density in square miles
                                from = "ft2",
                                to = "mi2")))

## 2. ----
cell_size <- conv_unit(0.1, # each grid is 1/10th of a square mile
                       from = "mi2",
                       to = "ft2")

# 3. ----
grids <- map2(BGs[1:6],
              bbox_sfs[1:6],
             ~ st_make_grid(.x,
                            # see notes for utility function find_hex_cellsize()
                            cellsize = find_hex_cellsize(cell_size),
                            # hex cells
                            square = FALSE) %>% 
               st_sf() %>% 
               # interpolate population by BG to each grid cell
               {st_interpolate_aw(.x["TotPop"],
                                  .,
                                  # preserve total population
                                  # extensive = FALSE would preserve the mean
                                  extensive = TRUE)} %>% 
               mutate(TotPop = round(TotPop)) %>% 
               .[.y,])

## 1. Export as RDS ----
# saveRDS(grids,
#         "~objects/20/21_grids.RDS")