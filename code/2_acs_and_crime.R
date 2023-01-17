get_acs_data <- function(city_state, city_county, epsg = 4326){
  library(sf)
  library(data.table)
  library(lubridate)
  library(units)
  library(tidyverse)
  library(tidycensus)
  library(data.table)
  library(readxl)
  library(geojsonsf)
  library(sfheaders)
  library(rvest)
  library(spdep)
  
  # Download ACS data
  options(tigris_use_cache = TRUE)
  acs_vars <- load_variables(2021, "acs5", cache = TRUE)
  acs_vars_selection <- read_excel("data/acs_variables.xlsx")
  acs_raw <- get_acs(state = city_state, county = city_county, geography = "tract", 
                     variables = acs_vars_selection$acs_variable,
                     year = 2021, survey = "acs5",
                     geometry = TRUE,cache_table = T,
                     output = "wide") %>% 
    st_transform(epsg = epsg)
  
  
  if (is.null(city_county)) { city_county = ""}
  if ("Cook" %in% city_county) {
    census_tracts <- read.csv('data/chicago_census_tracts.csv')
    acs_raw <- acs_raw[substr(acs_raw$GEOID,1,11) %in% as.character(census_tracts$GEOID10),]
  }
  
  if ("Los Angeles" %in% city_county) {
    la_shape <- sf::read_sf('data/la_city_boundaries/city_boundary/') %>% st_transform(crs = st_crs(acs_raw))
    #census_tracts <- read.csv('data/la_census_tracts.csv')
    #acs_raw <- acs_raw[gsub(".* Tract (.*), Los.*", "\\1", acs_raw$NAME) %in% as.character(census_tracts$LABEL),]
    acs_raw <- st_intersection(acs_raw,la_shape)
    acs_raw$OBJECTID <- NULL
    acs_raw$CITY <- NULL
  }
  
  acs <- acs_raw[,grep(".*(?<!M)$", names(acs_raw),perl = TRUE )]
  names(acs)[3:(ncol(acs)-1)] <- acs_vars_selection$variable_desc
  
  # Calculate socioeconomic variables 
  acs_statistics <- acs %>%
    as.data.table() %>% 
    group_by(GEOID) %>% 
    summarise(total_pop = total_pop,
              median_household_income = median_household_income,
              pct_black = total_pop_Black / total_pop,
              pct_hispanic = total_pop_Hispanic / total_pop,
              pct_hshlds_ownerocc = total_households_owner_occupied / total_households,
              pct_recently_moved = (total_moved_into_2017_owner_occupied + total_moved_into_2017_renter_occupied) / total_households,
              pct_poor = total_pop_below_poverty / total_pop,
              pct_unemployed = total_civilian_labor_force_unemployed / total_civilian_labor_force,
              pct_single_mothers = `total_households_female-headed` / total_households,
              pct_young_males = (total_pop_males_10_to_24_1 + 
                                   total_pop_males_10_to_24_2 +
                                   total_pop_males_10_to_24_3 + 
                                   total_pop_males_10_to_24_4 +
                                   total_pop_males_10_to_24_5 +
                                   total_pop_males_10_to_24_6) / total_pop)
  
  acs <- left_join(acs[,1:2], acs_statistics)
  
  ### Drop census block groups with zero pop
  #acs <- acs %>% filter(total_pop >0) #%>% na.omit()
  
  ### NB: Impute missing values by column medians
  acs <- acs %>% st_drop_geometry() %>% 
    mutate_all(~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)) %>% 
    st_set_geometry(., acs$geometry)
  
  return(acs)
}

# Aggregate shoootings over gridcells and pre2020/2020
prepare_shootings_data <- function(path_to_shootings_dataset, grid, epsg = 4326){
  
  # Read city-level shootings
  crime <- read.csv(path_to_shootings_dataset) %>% 
    select(lng,lat,year, shooting_type) %>% 
    st_as_sf(., coords = c("lng", "lat"),
             crs = 4326, agr = "constant") %>% 
    st_transform(crs = st_crs(city_acs))
  
  grid_crime <- st_join(crime, grid, join = st_within, left = T) 
  
  # Aggregate shootings by cells and years
  sh_aggr <- grid_crime %>% 
    as.data.table() %>% 
    #mutate(year = year(datetime)) %>% 
    count(GEOID, year)
  
  sh_aggr <- pivot_wider(sh_aggr, names_from = "year", values_from = "n",names_sort = T,names_prefix = "y")
  sh_aggr <- left_join(data.frame(GEOID = grid$GEOID), sh_aggr)
  sh_aggr[is.na(sh_aggr)] <- 0
  
  #sh_aggr$shootings_average_pre2020 <- rowMeans(sh_aggr[,2:(ncol(sh_aggr)-1)], na.rm = T)
  #sh_aggr$shootings_average_pre2020 <- rowMeans(sh_aggr[,c("2018","2019")], na.rm = T)
  
  #sh_aggr$shootings_total_pre2020 <- rowSums(sh_aggr[,c("2018","2019")], na.rm = T)
  
  
  #sh_aggr$shootings_total_post2020 <- rowSums(sh_aggr[,c("2020","2021")], na.rm = T)
  
  sh_aggr$shootings_total <- rowSums(sh_aggr[,c("y2015","y2016","y2017","y2018","y2019","y2020","y2021")], na.rm = T)
  #sh_aggr$shootings_average_post2020 <- rowMeans(sh_aggr[,c("2020","2021")], na.rm = T)
  
  # NB I round up averages and medians for the sake of Poisson
  #sh_aggr$shootings_average_pre2020 <- ceiling(sh_aggr$shootings_average_pre2020)
  #sh_aggr$shootings_average_post2020 <- ceiling(sh_aggr$shootings_average_post2020)
  
  #sh_aggr$shootings_change_pre_to_post <- sh_aggr$shootings_total_pre2020 / sh_aggr$shootings_total_post2020
  
  #sh_aggr <- sh_aggr[,c("cell_id",
  #                      "shootings_total_pre2020",
  #                      "shootings_total_post2020",
  #                      "shootings_total",
  #                      "shootings_change_pre_to_post")]
  
  
  
  return(left_join(grid, sh_aggr, by = "GEOID"))
}

# Main function
prepare_dataset <- function(city_state, city_county = "",
                            city_shootings_dataset_path,
                            city_name, nice_city_name,
                            epsg = 4326){
  # Download and save ACS data
  city_acs <- get_acs_data(city_state, city_county, epsg)
  
  # Map shootings onto the grid
  grid_crime <- prepare_shootings_data(city_shootings_dataset_path,
                                       city_acs, epsg)
  
  grid_crime$name <- nice_city_name
  grid_crime$area <- st_area(grid_crime)
  grid_crime$total_rate_per_square_block <- as.numeric(grid_crime$shootings_total / 7 / grid_crime$area * 22000)
  
  saveRDS(grid_crime,paste0("data/datasets/",city_name,".rds"))
}


prepare_dataset(city_state = "NY",
                city_county = c("New York","Bronx","Queens","Kings", "Richmond"),
                city_shootings_dataset_path = "data/shootings_csv/New York.csv",
                city_name = "nyc",
                nice_city_name = "New York City, NY")


prepare_dataset(city_state = "PA",
                city_county = c("Philadelphia"),
                city_shootings_dataset_path = "data/shootings_csv/Philadelphia.csv",
                city_name = "philadelphia",
                nice_city_name = "Philadelphia, PA")


prepare_dataset(city_state = "IL",
                city_county = c("Cook"),
                city_shootings_dataset_path = "data/shootings_csv/Chicago.csv",
                city_name = "chicago",
                nice_city_name = "Chicago, IL")

