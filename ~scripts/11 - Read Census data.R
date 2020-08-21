##########################################################################
# This script:
# 1. Defines census variables for collection
# 2. Reads in census tracts and block groups for all cities
#   (a) Baltimore
#   (b) Minneapolis
#   (c) New Orleans
#   (d) Philadelphia
#   (e) Houston
#   (f) San Francisco
#   (g) Pennsylvania
# 3. Combines them
#
# Exports: 
# 1. 
#   (a) tracts as 11_tracts.RDS
#   (b) BGs as 11_BGs.RDS
# 
# To-do:
# 1. Other cities
#
##########################################################################

## 1. ----
# List of 2018 ACS variables: https://api.census.gov/data/2018/acs/acs5/variables.html
census_df <- data.frame(vars =     c("B01003_001", 
                                     "B19013_001", 
                                     "B01002_001", 
                                     "B02001_002",
                                     "B02001_003",
                                     "B03001_003",
                                     "B08014_001",
                                     "B08014_002",
                                     "B08013_001",
                                     "B08012_013",
                                     "B08301_001",
                                     "B08301_002",
                                     "B08301_010",
                                     "B25077_001",
                                     "B25064_001",
                                     "B06009_005",
                                     "B06009_001"),
                        
                        colNames = c("TotPop",
                                     "MdHHInc",
                                     "MdAge",
                                     "White_Pop",
                                     "Black_Pop",
                                     "Hisp_Pop",
                                     "Vehicle_own_pop",
                                     "No_vehicle",
                                     "Total_Travel_Time",
                                     "Num_Commuters",
                                     "Means_of_Transport_pop",
                                     "Total_cartruckvan",
                                     "Total_Public_Trans",
                                     "MedValue",
                                     "MedRent",
                                     "CollGrad",
                                     "EduPop"),
                        stringsAsFactors = FALSE)

census_vars <- census_df$vars
census_colNames <- census_df$colNames

## 2. ----
# (a) Baltimore
# tracts
balt_tracts <- get_acs(geography = "tract",
                       variables = census_vars, # population
                       year = 2018,
                       state = "Maryland",
                       county = "Baltimore city",
                       survey = "acs5",
                       output = "wide",
                       geometry = TRUE) %>% 
  st_transform(sdg_cities_list$Baltimore$proj) %>% 
  rename_census_cols(x = .,
                     vars = census_vars,
                     names = census_colNames,
                     drop_MOE = TRUE)

# block groups
balt_BGs <- get_acs(geography = "block group",
                    variables = census_vars, # population
                    year = 2018,
                    state = "Maryland",
                    county = "Baltimore city",
                    survey = "acs5",
                    output = "wide",
                    geometry = TRUE) %>% 
  st_transform(sdg_cities_list$Baltimore$proj) %>% 
  rename_census_cols(x = .,
                     vars = census_vars,
                     names = census_colNames,
                     drop_MOE = TRUE)

# (b) Minneapolis
# CDP
mpls <- places("Minnesota",
               year = 2018,
               class = "sf",
               cb = TRUE) %>% 
  filter(NAME == "Minneapolis") %>% 
  st_transform(sdg_cities_list$Minneapolis$proj)

# tracts
mpls_tracts <- get_acs(geography = "tract",
                       variables = census_vars, # population
                       year = 2018,
                       state = "Minnesota",
                       survey = "acs5",
                       output = "wide",
                       geometry = TRUE) %>% 
  st_transform(sdg_cities_list$Minneapolis$proj) %>% 
  rename_census_cols(x = .,
                     vars = census_vars,
                     names = census_colNames,
                     drop_MOE = TRUE) %>% 
  st_filter(mpls, .predicate = st_within)

# block groups
mpls_BGs <- get_acs(geography = "block group",
                    variables = census_vars, # population
                    year = 2018,
                    state = "Minnesota",
                    survey = "acs5",
                    output = "wide",
                    geometry = TRUE) %>% 
  st_transform(sdg_cities_list$Minneapolis$proj) %>% 
  rename_census_cols(x = .,
                     vars = census_vars,
                     names = census_colNames,
                     drop_MOE = TRUE) %>% 
  st_filter(mpls, .predicate = st_within)

# (c) New Orleans
# tracts
nola_tracts <- get_acs(geography = "tract",
                       variables = census_vars, # population
                       year = 2018,
                       state = "Louisiana",
                       county = "Orleans Parish",
                       survey = "acs5",
                       output = "wide",
                       geometry = TRUE) %>% 
  st_transform(sdg_cities_list$`New Orleans`$proj) %>% 
  rename_census_cols(x = .,
                     vars = census_vars,
                     names = census_colNames,
                     drop_MOE = TRUE)

# block groups
nola_BGs <- get_acs(geography = "block group",
                    variables = census_vars, # population
                    year = 2018,
                    state = "Louisiana",
                    county = "Orleans Parish",
                    survey = "acs5",
                    output = "wide",
                    geometry = TRUE) %>% 
  st_transform(sdg_cities_list$`New Orleans`$proj) %>% 
  rename_census_cols(x = .,
                     vars = census_vars,
                     names = census_colNames,
                     drop_MOE = TRUE)

# (d) Philadelphia
# tracts
phl_tracts <- get_acs(geography = "tract",
                       variables = census_vars, # population
                       year = 2018,
                       state = "Pennsylvania",
                       county = "Philadelphia",
                       survey = "acs5",
                       output = "wide",
                       geometry = TRUE) %>% 
  st_transform(sdg_cities_list$`Philadelphia, Pennsylvania`$proj) %>% 
  rename_census_cols(x = .,
                     vars = census_vars,
                     names = census_colNames,
                     drop_MOE = TRUE)

# block groups
phl_BGs <- get_acs(geography = "block group",
                    variables = census_vars, # population
                    year = 2018,
                   state = "Pennsylvania",
                   county = "Philadelphia",
                    survey = "acs5",
                    output = "wide",
                    geometry = TRUE) %>% 
  st_transform(sdg_cities_list$`Philadelphia, Pennsylvania`$proj) %>% 
  rename_census_cols(x = .,
                     vars = census_vars,
                     names = census_colNames,
                     drop_MOE = TRUE)

# (e) Houston
# tracts
hou_tracts <- get_acs(geography = "tract",
                       variables = census_vars, # population
                       year = 2018,
                       state = "Texas",
                       survey = "acs5",
                       output = "wide",
                       geometry = TRUE) %>% 
  st_transform(sdg_cities_list$`Houston, Texas`$proj) %>% 
  .[bbox_sfs$`Houston, Texas`,] %>% 
  rename_census_cols(x = .,
                     vars = census_vars,
                     names = census_colNames,
                     drop_MOE = TRUE)

# block groups
hou_BGs <- get_acs(geography = "block group",
                    variables = census_vars, # population
                    year = 2018,
                    state = "Texas",
                    county = c("Harris County", "Fort Bend", "Montgomery"),
                    survey = "acs5",
                    output = "wide",
                    geometry = TRUE) %>% 
  st_transform(sdg_cities_list$`Houston, Texas`$proj) %>% 
  .[bbox_sfs$`Houston, Texas`,] %>% 
  rename_census_cols(x = .,
                     vars = census_vars,
                     names = census_colNames,
                     drop_MOE = TRUE)

# (f) San Francisco
# tracts
sf_tracts <- get_acs(geography = "tract",
                       variables = census_vars, # population
                       year = 2018,
                       state = "California",
                       county = "San Francisco",
                       survey = "acs5",
                       output = "wide",
                       geometry = TRUE) %>% 
  st_transform(sdg_cities_list$`San Francisco, California`$proj) %>% 
  .[bbox_sfs$`San Francisco, California`,] %>% 
  rename_census_cols(x = .,
                     vars = census_vars,
                     names = census_colNames,
                     drop_MOE = TRUE)

# block groups
sf_BGs <- get_acs(geography = "block group",
                    variables = census_vars, # population
                    year = 2018,
                    state = "California",
                  county = "San Francisco",
                  survey = "acs5",
                    output = "wide",
                    geometry = TRUE) %>% 
  st_transform(sdg_cities_list$`San Francisco, California`$proj) %>% 
  .[bbox_sfs$`San Francisco, California`,] %>% 
  rename_census_cols(x = .,
                     vars = census_vars,
                     names = census_colNames,
                     drop_MOE = TRUE)

# (d) Pennsylvania
# tracts
PA_tracts <- get_acs(geography = "tract",
                       variables = census_vars, # population
                       year = 2018,
                       state = "Pennsylvania",
                       survey = "acs5",
                       output = "wide",
                       geometry = TRUE) %>% 
  st_transform(sdg_cities_list$Pennsylvania$proj) %>% 
  rename_census_cols(x = .,
                     vars = census_vars,
                     names = census_colNames,
                     drop_MOE = TRUE)

# block groups
PA_BGs <- get_acs(geography = "block group",
                    variables = census_vars, # population
                    year = 2018,
                    state = "Pennsylvania",
                    survey = "acs5",
                    output = "wide",
                    geometry = TRUE) %>% 
  st_transform(sdg_cities_list$Pennsylvania$proj) %>% 
  rename_census_cols(x = .,
                     vars = census_vars,
                     names = census_colNames,
                     drop_MOE = TRUE) %>% 
  .[!st_is_empty(.), , drop = FALSE]

## 3. ----
tracts <- list("Baltimore" = balt_tracts,
               "Minneapolis" = mpls_tracts,
               "New Orleans" = nola_tracts,
               "Philadelphia" = phl_tracts,
               "Houston" = hou_tracts,
               "San Francisco" = sf_tracts,
               "Pennsylvania" = PA_tracts)

BGs <- list("Baltimore" = balt_BGs,
            "Minneapolis" = mpls_BGs,
            "New Orleans" = nola_BGs,
            "Philadelphia" = phl_BGs,
            "Houston" = hou_BGs,
            "San Francisco" = sf_BGs,
            "Pennsylvania" = PA_BGs)


## 1. Export as RDS ----
# saveRDS(tracts,
#         file = "~objects/10/11_tracts.RDS")
# saveRDS(BGs,
#         file = "~objects/10/11_BGs.RDS")
