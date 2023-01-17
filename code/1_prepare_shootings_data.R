prepare_shootings_data <- function(years = 2015:2022) {
  library(stringr)
  message("--- Download and prepare shootings data...")
  
  ### Download datasets
  ## Philadelphia
  # Info page: https://www.opendataphilly.org/dataset/shooting-victims
  philadelphia_shootings_url <- "https://phl.carto.com/api/v2/sql?q=SELECT+*,+ST_Y(the_geom)+AS+lat,+ST_X(the_geom)+AS+lng+FROM+shootings&filename=shootings&format=csv&skipfields=cartodb_id"
  if (!file.exists("raw_data/philadelphia_shootings.csv")){
    download.file(philadelphia_shootings_url, "raw_data/philadelphia_shootings.csv")
  }
  
  
  ## Chicago
  # Info page: https://data.cityofchicago.org/Public-Safety/Violence-Reduction-Victims-of-Homicides-and-Non-Fa/gumc-mgzr
  chicago_shootings_url <- "https://data.cityofchicago.org/api/views/gumc-mgzr/rows.csv?accessType=DOWNLOAD"
  
  if (!file.exists("raw_data/chicago_shootings.csv")){
    download.file(chicago_shootings_url, "raw_data/chicago_shootings.csv")
  }
  
  ## NYC
  # Info page: https://data.cityofnewyork.us/Public-Safety/NYPD-Shooting-Incident-Data-Historic-/833y-fsy8
  nyc_shootings_url <- "https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv?accessType=DOWNLOAD"
  if (!file.exists("raw_data/nyc_shootings.csv")){
    download.file(nyc_shootings_url, "raw_data/nyc_shootings.csv")
  }
  
  ## LA
  # Info page: https://data.lacity.org/Public-Safety/Crime-Data-from-2020-to-Present/2nrs-mtv8
  # NB: crime data, 126 MB
  la_crime_url <- "https://data.lacity.org/api/views/2nrs-mtv8/rows.csv?accessType=DOWNLOAD"
  if (!file.exists("raw_data/la_crime.csv")){
    download.file(la_crime_url, "raw_data/la_crime.csv")
  }
  
  
  ### Philadelphia
  city_crime <- fread("raw_data/philadelphia_shootings.csv")
  city_crime$datetime <- ymd_hms(paste0(city_crime$date_, " ", city_crime$time),
                                 quiet = T
  )
  city_crime$year <- year(city_crime$datetime)
  city_crime <- city_crime[
    year %in% years
  ]
  city_crime <- city_crime %>%
    mutate(shooting_type = ifelse(fatal == 1, "lethal", "nonlethal")) %>%
    # drop wrongly geocoded incidents
    filter(substr(lat, 1, 2) != 28) %>%
    select(-one_of(c("age", "fatal"))) %>%
    select(datetime, year, shooting_type, lat, lng) %>%
    na.omit()
  
  fwrite(city_crime, "data/shootings_csv/Philadelphia.csv")
  
  ### New York City
  city_crime <- fread("raw_data/nyc_shootings.csv")
  
  city_crime$datetime <- mdy_hms(paste(city_crime$OCCUR_DATE, city_crime$OCCUR_TIME))
  
  city_crime <- city_crime %>%
    filter(year(datetime) %in% years) %>%
    mutate(year = year(datetime),
           shooting_type = ifelse(STATISTICAL_MURDER_FLAG == T,
                                  "lethal", "nonlethal")) %>% 
    select(datetime, year, Longitude, Latitude, shooting_type) %>% 
    na.omit()
  
  # Rename
  names(city_crime) <- c("datetime", "year", "lng", "lat", "shooting_type")
  
  
  fwrite(city_crime, "data/shootings_csv/New York.csv")
  
  ### Chicago
  city_crime <- fread("raw_data/chicago_shootings.csv")
  
  # Date
  city_crime$datetime <- parse_date_time(city_crime$DATE, "m/d/y IMS p")
  city_crime$year <- year(city_crime$date)
  
  # Subset years
  city_crime <- city_crime[year %in% years]
  
  # Subset city_crime resulted in injury
  city_crime <- city_crime[GUNSHOT_INJURY_I == "YES"]
  
  city_crime <- city_crime %>%
    mutate(shooting_type = recode(INCIDENT_PRIMARY,
                                  "HOMICIDE" = "lethal", .default = "nonlethal"),
           year = year(datetime)) %>%
    select(datetime, year, shooting_type, LATITUDE, LONGITUDE) %>%
    na.omit()
  
  # Rename
  names(city_crime) <- c("datetime", "year", "shooting_type", "lat", "lng")
  
  fwrite(city_crime, "data/shootings_csv/Chicago.csv")
  
  ### Los Angeles
  city_crime <- fread("raw_data/la_crime.csv")
  
  # Parse date and time
  city_crime$datetime <- parse_date_time(
    paste(
      substring(city_crime$`DATE OCC`, 0, 11),
      str_pad(city_crime$`TIME OCC`, 4, pad = "0")
    ),
    "%m/%d/%Y %H%M"
  )
  
  city_crime$year <- year(city_crime$datetime)
  
  # Subset years
  city_crime <- city_crime[year %in% years, ]
  
  # MO Code == 1430 means victims shot
  city_crime <- city_crime[grepl("0430", Mocodes, fixed = T)]
  
  # Generate variables for data quality analysis
  city_crime$shooting_type <- dplyr::recode(city_crime$`Crm Cd Desc`,
                                            "CRIMINAL HOMICIDE" = "lethal",
                                            "MANSLAUGHTER, NEGLIGENT" = "lethal",
                                            .default = "nonlethal"
  )
  

  
  city_crime <- city_crime %>%
    select(datetime, year, shooting_type, LAT, LON) %>%
    filter(LAT > 0) %>%
    na.omit()
  
  names(city_crime) <- c("datetime","year","shooting_type", "lat", "lng")
  fwrite(city_crime, "data/shootings_csv/Los Angeles.csv")
}

prepare_shootings_data()