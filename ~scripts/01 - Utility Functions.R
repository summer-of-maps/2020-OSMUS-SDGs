##########################################################################
# This script:
# 1. Defines utility functions for use in the analysis.
#
# Exports:
# 1. q5(): factor continuous variable in quintiles
# 2. qBr(): find quintile breaks
# 3. rename_census_cols(): rename census variable ID columns with a vector of names and 
#   optionally drop the margin of error columns
# 4. find_hex_cellsize(): for a desired hexagonal cell area, returns a cellsize for the st_make_grid() function 
# 5. st_snap_points(): snap point geometries x to nearest point in geometries y
# 6. base_map_bb(): Get a bounding box for use with the ggmap package's get_map() function
#
# To-do:
# 1. 
##########################################################################

## 1. ----
q5 <- function(variable) {as.factor(ntile(variable, 5))}

## 2. ----
qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

## 3. ----
rename_census_cols <- function(x,
                               vars,
                               names,
                               drop_MOE = TRUE # drop margin of error?
                               ){
  
  estimate <- paste(vars, "E", sep = "")
  MOE <- paste(vars, "M", sep = "")
  
  if(drop_MOE == TRUE | drop_MOE == T) {
    
    output <- x %>% 
      rename_at(vars(estimate), 
                ~ names) %>% 
      dplyr::select(-all_of(MOE))
  
  } else if (missing(drop_MOE) | drop_MOE == FALSE | drop_MOE == F) {
    
    output <- x %>% 
      rename_at(vars(estimate), 
                ~ names)
  
    }
  
  output
}

## 4. ----
find_hex_cellsize <- function(area # desired area for each hexagonal cell
                          ) {
  
  # the cellsize parameter in st_make_grid() is
  # the distance between the centroids of each hexagonal cell
  
  (area * (2/3^0.5)) ^ 0.5 
  
}

## 5. ----
st_snap_points = function(x, y, max_dist = 1000) {
  
  if (inherits(x, "sf")) n = nrow(x)
  if (inherits(x, "sfc")) n = length(x)
  
  out = do.call(c,
                lapply(seq(n), function(i) {
                  nrst = st_nearest_points(st_geometry(x)[i], y)
                  nrst_len = st_length(nrst)
                  nrst_mn = which.min(nrst_len)
                  if (as.vector(nrst_len[nrst_mn]) > max_dist) return(st_geometry(x)[i])
                  return(st_cast(nrst[nrst_mn], "POINT")[2])
                })
  )
  return(out)
}

## 6. ----
base_map_bb <- function(sf # this should be an sf object
                        ) {
  tmaptools::bb(sf, output = "matrix")
  }
