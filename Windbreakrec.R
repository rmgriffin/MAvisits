# Intro -------------------------------------------------------------------
rm(list=ls()) # Clears workspace

# install.packages("renv") # Install/call libraries
# renv::init()

PKG<-c("googledrive","sf","tidyverse","httpuv","R.utils","httr","jsonlite","geojsonsf","lwgeom","furrr","arrow","stringr","sandwich","lmtest","fixest")

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p, type = "binary")
    require(p,character.only = TRUE)}
}

renv::snapshot()
rm(p,PKG)
options(scipen = 999) # Prevent scientific notation

# Download data -----------------------------------------------------------
dir.create(file.path('Data'), recursive = TRUE)
folder_url<-"https://drive.google.com/open?id=15amwG3br43cpU9MS8gghNcYhljHoi52I"
folder<-drive_get(as_id(folder_url))
files<-drive_ls(folder)
dl<-function(files){
  walk(files, ~ drive_download(as_id(.x), overwrite = TRUE))
}
setwd("./Data")
system.time(map(files$id,dl))
system.time(unzip("Data.zip", exdir = "."))
file.remove("Data.zip")
setwd("..")
rm(files, folder, folder_url, dl)

# Creating study area of interest ------------------------------------------
# ESI<-st_read("Data/MV_N_ESIL.gpkg") # Layer that defines the coastline in line GIS vector format
# ESI<-st_transform(ESI,crs = 32619) # Project to UTM 19N (in meters)
# 
# bbox<-st_bbox(ESI) # Bounding box of ESI layer
# hex_grid<-st_make_grid(ESI, cellsize = 50, square = FALSE,crs = st_crs(ESI)) %>% # Making a regularized hexagonal tessellation over the area 
#   st_as_sf() %>%
#   mutate(id = row_number())
#   
# system.time(hex_grid<-st_filter(hex_grid, ESI)) # Grid hexagons that overlay ESI lines
# 
# hex_grid$loc<-ifelse(st_coordinates(st_centroid(hex_grid))[, 1]> 385000, "Nantucket","Martha's Vineyard") # Naming areas 
# 
# hullN<-st_buffer(st_cast(st_convex_hull(st_union(hex_grid[hex_grid$loc=="Nantucket",])),"MULTILINESTRING"),50) # 100 meter buffered convex hull around Nantucket
# 
# hullMV<-st_buffer(st_cast(st_concave_hull(st_union(hex_grid[hex_grid$loc=="Martha's Vineyard",]),ratio = 0.01),"MULTILINESTRING"),100) # 100 meter buffered concave hull around MV, messed with the ratio parameter visually to calibrate to outcome
# 
# hex_grid$affected<-ifelse(st_intersects(hex_grid, hullN, sparse = FALSE), "affected", # Convex hull Nantucket
#                           ifelse(st_coordinates(st_centroid(hex_grid))[, 2] < 4567000, "affected", # Southern indent convex hull Nantucket
#                                  ifelse(st_intersects(hex_grid, hullMV, sparse = FALSE), "affected", "unaffected"))) # Concave hull MV 
# 
# hex_grid$affected<-ifelse(st_coordinates(st_centroid(hex_grid))[, 1] < 351700, "unaffected", hex_grid$affected)
# hex_grid$affected<-ifelse(st_coordinates(st_centroid(hex_grid))[, 2] > 4578995 & st_coordinates(st_centroid(hex_grid))[, 1] < 378600, "unaffected", hex_grid$affected)
# hex_grid$affected<-ifelse(hex_grid$loc=="Nantucket" & st_coordinates(st_centroid(hex_grid))[, 2] < 4567600, "affected", hex_grid$affected)
# hex_grid$affected<-ifelse(hex_grid$loc=="Nantucket" & st_coordinates(st_centroid(hex_grid))[, 2] > 4572950 & st_coordinates(st_centroid(hex_grid))[, 1] > 416500, "affected", hex_grid$affected)
# hex_grid$affected<-ifelse(st_coordinates(st_centroid(hex_grid))[, 1] < 396500 & hex_grid$loc=="Nantucket", "unaffected", hex_grid$affected) # Removing Tuckernuck and Muskeget Islands from analysis
# 
# #hex_grid$affected<-ifelse(hex_grid$id %in% c(30489,25366),"affected",hex_grid$affected) # Few manual fixes @ 250m hexes
# hex_grid$affected<-ifelse(hex_grid$id %in% c(707010,706046,705082,704118,699619,698655,695763,694799,693835,692871,692228,691907,691264,690300,689336,688372, # Manual fixes using 50m hexes
#                                              687408,686444,685480,684837,684516,683873,683552,682909,682588,681945,681624,680981,680660,680017,679053,678089,
#                                              677446,677125,676482,676161,675518,674554,673590,672626,671662,670698,670055,669091,668127,667163,666199,662343,
#                                              661379,661058,660415,660737,660094,659773,659130,658809,658487,658166,657523,657202,656559,655595,655274,654631,
#                                              654310,653346,652382,651418,651097,650454,650133,649490,649812,649169,649491,905465,905144,904823,904502,904181,
#                                              903860,903539,903218,902897,902576,902255,901934,900972,900651,900330,900009,899688,899367,899046,898725,898404,
#                                              896479,896158,895837,895516,895195,894874,894553,891986,891665,891344,864054,863733,937235,936914),"affected",hex_grid$affected) # Few manual fixes @ 50m resolution hexes
# 
# ## Add a handful of ids on MV that are unaffected to above line, if pursuing analysis there
# 
# ggplot() + # Checking layers
#   #geom_sf(data = hex_grid[hex_grid$loc=="Nantucket"&hex_grid$affected=="affected",], fill = "lightblue", color = "black", alpha = 0.5) +  # Hexagonal grid
#   geom_sf(data = hex_grid[hex_grid$affected=="affected",], fill = "lightblue", color = "black", alpha = 0.5) +
#   #geom_sf(data = ESI, color = "red", size = 1.5) +  # Line feature
#   #geom_sf(data = hullMV, color = "green", alpha = 0.5) +
#   theme_minimal()
# 
# # st_write(hex_grid,"hex_grid50.gpkg")
# 
# hex_union<-hex_grid %>% # Union based on two grouping variables
#   group_by(loc, affected) %>%
#   summarise(geometry = st_union(x), .groups = "drop")
# 
# # st_write(st_transform(hex_union, crs = 4326),"hex_union.gpkg")

# Downloading cell data ---------------------------------------------------
df<-st_read("Data/GSNantucketSBeaches.gpkg") # AOI for Nantucket from satellite imagery
dg<-st_simplify(st_read("Data/nantucket_terminal_osm_extracted.gpkg"),dTolerance = 0.00001)
dg<-st_read("Data/nantucket_terminal_osm_extracted.gpkg")
## Going to want higher res cell data home locations
api_key<-read.csv(file = "APIkey.csv", header = FALSE)
api_key<-api_key$V1
headers<-add_headers(Authorization = api_key, `Content-Type` = "application/json") # Set up the headers including the API key
url<-"https://api.gravyanalytics.com/v1.1/areas/tradeareas" # API URL to query
if (!dir.exists("tData")) { # Create cell data directory if it doesn't exist
  dir.create("tData", recursive = TRUE)
}
#df<-hex_union %>% filter(affected == "affected") # Only interested in affected areas
#df$id<-seq(1,nrow(df),1) # API requires a variable named "id" to pass through the id to the files that are returned, named "searchobjectid" in the file
df<-st_transform(df, crs = 4326) # Needs to be projected in 4326 to work with lat long conventions of the API

batchapi<-function(dft,s,e,fname){ # Function converts sf object to json, passes to api, gets returned data, and merges back with sf object

  dft$startDateTimeEpochMS<-s # 1704067200000 These work as query variables
  dft$endDateTimeEpochMS<-e # 1706831999000
  dft$excludeFlags<-25216 # Corresponds to guidance for visitation from venntel
  # dft$startDateTimeEpochMS<-1656633600000
  # dft$endDateTimeEpochMS<-1659311999999
  #dft<-dft %>% select(-PUD_YR_AVG) # Need more than the geometry column to create a feature collection using sf_geojson. Also, there is a limit of 20 features per request (even if it doesn't return results for 20 features).
  dftj<-sf_geojson(dft,atomise = FALSE) # Convert sf object to GeoJSON

  dftj<-fromJSON(dftj) # Doesn't seem to like geojson formatting, switching to json
  dftj<-toJSON(dftj, auto_unbox = TRUE, digits = 15)

  # Export query (asynchronous)
  system.time(response<-POST(url, headers, body = dftj, encode = "json", query = list(
    # includeHeaders = FALSE, # Remove headers - potentially useful for batching
    # returnDeviceCountByGeoHash = TRUE, # "If true, the geoHashDeviceCount and geoHashWidthHeights fields are populated per feature" - don't see this. It does return "searchobjectid" in the response psv that corresponds to a given "id" in the json properties
    #decisionLocationTypes = list(c("LATLNG","CBG")),
    decisionLocationTypes = "CBG",
    includeAdditionalCbgInfo = TRUE,
    startTimeOfDay = "09:00:00Z", # Need to adjust for timezone when binning "daily visits" - this adjusts UTC ("Z") to Eastern Time
    endTimeOfDay = "23:59:59Z",
    #includeGeometryWithCbgInfo = TRUE, # Geometry of CBG for GIS
    exportSchema = "EVENING_COMMON_CLUSTERS",
    compressOutputFiles = FALSE, # Compressed outputs?
    responseType = "EXPORT"  # Requesting an export response
  )))

  requestID<-response$headers$requestid
  status_url<-paste0("https://api.gravyanalytics.com/v1.1/requestStatus/", requestID)
  export_complete<-FALSE

  # Function that pings the API to see if the export request is done every 1 seconds and returns either of {files ready, still waiting, failed}
  while (!export_complete) {
    Sys.sleep(1)  # Wait for 1 seconds before polling again
    status_response<-GET(status_url, add_headers(Authorization = api_key))
    status_content<-content(status_response, "parsed")

    if (status_content$status == "DONE") {
      export_complete<-TRUE
      aws_s3_link<-as.character(status_content$presignedUrlsByDataType$tradeAreas)
      base::cat("Your files are ready")
    } else if (status_content$status == "FAILED") {
      stop("Export request failed. Please try again.")
    } else {
      base::cat("Export is still in progress. Status:", round(status_content$requestDurationSeconds/60,2),"m", "\n",sep = c(" ","","",""))
    }
  }

  # Loading export results into workspace -----------------------------------

  file_name<-sub("\\?.*", "", basename(aws_s3_link)) # Extracting the file name

  downloaded_files<-lapply(seq_along(aws_s3_link), function(i) { # Batch downloading all links returned by the API call. Mode = "wb" is important.
    file_path<-file.path("tData", file_name[i]) # Construct full path
    download.file(aws_s3_link[i], destfile = file_path, mode = "wb") # Download file
    return(file_path) # Return the file path
  })
  downloaded_files<-unlist(downloaded_files)

  xp<-do.call(rbind, # Row bind files into a dataframe
              lapply( # Apply over all elements in a list
                file.path("tData/", sub("\\.gz$", "", file_name)), # Elements in a list that are named based on the API call
                function(file) {read.csv(file, sep = "|", header = TRUE)})) # Reading files in

  invisible(unlink(downloaded_files)) # Deleting downloaded psv files

  if (is.null(xp) || nrow(xp) == 0) { # Handles no data situations where there are no observations in the provided polygon(s)
    warning("No data returned from API for this batch. Skipping...")
    return(NULL)  # Return NULL to avoid stopping execution
  }

  #xp<-merge(xp,dft, by.x = "FEATUREID", by.y = "id") # %>% dplyr::select(FEATUREID,DEVICEID,DAY_IN_FEATURE,EARLIEST_OBSERVATION_OF_DAY,LATEST_OBSERVATION_OF_DAY,LATITUDE,LONGITUDE,CENSUS_BLOCK_GROUP_ID,startDateTimeEpochMS,endDateTimeEpochMS,DEVICES_WITH_DECISION_IN_CBG_COUNT,TOTAL_POPULATION)
  xp<-xp %>% dplyr::select(FEATUREID,DEVICEID,DAY_IN_FEATURE,EARLIEST_OBSERVATION_OF_DAY,LATEST_OBSERVATION_OF_DAY,CENSUS_BLOCK_GROUP_ID,DEVICES_WITH_DECISION_IN_CBG_COUNT,TOTAL_POPULATION)

  write_parquet(xp, paste0("tData/",fname,".parquet")) # Write to parquet file to save space, versus csv
}

batchapi(df, s = as.numeric(as.POSIXct("2022-06-15 00:00:00.000", tz = "America/New_York")) * 1000,
         e = as.numeric(as.POSIXct("2022-08-15 23:59:59.999", tz = "America/New_York")) * 1000, fname = "JunAug2022daylight50m") # Jun - Aug 2022
batchapi(df, s = as.numeric(as.POSIXct("2023-06-15 00:00:00.000", tz = "America/New_York")) * 1000,
         e = as.numeric(as.POSIXct("2023-08-15 23:59:59.999", tz = "America/New_York")) * 1000, fname = "JunAug2023daylight50m") # Jun - Aug 2023
batchapi(df, s = as.numeric(as.POSIXct("2024-06-15 00:00:00.000", tz = "America/New_York")) * 1000,
         e = as.numeric(as.POSIXct("2024-08-15 23:59:59.999", tz = "America/New_York")) * 1000, fname = "JunAug2024daylight50m") # Jun - Aug 2024

dfs<-map_df(list.files("tData/", pattern = "\\.parquet$", full.names = TRUE), read_parquet)

url<-"https://api.gravyanalytics.com/v1.1/areas/devices/trends"

batchapi<-function(dft,s,e,fname){ # Function converts sf object to json, passes to api, gets returned data, and merges back with sf object
  
  dft$startDateTimeEpochMS<-s # 1704067200000 These work as query variables
  dft$endDateTimeEpochMS<-e # 1706831999000
  dft$excludeFlags<-25216 # Corresponds to guidance for visitation from venntel
  # dft$startDateTimeEpochMS<-1656633600000
  # dft$endDateTimeEpochMS<-1659311999999
  # dft$returnDeviceCountByGeoHash<-TRUE
  #dft<-dft %>% select(-PUD_YR_AVG) # Need more than the geometry column to create a feature collection using sf_geojson. Also, there is a limit of 20 features per request (even if it doesn't return results for 20 features).
  dftj<-sf_geojson(dft,atomise = FALSE) # Convert sf object to GeoJSON
  
  dftj<-fromJSON(dftj) # Doesn't seem to like geojson formatting, switching to json
  dftj<-toJSON(dftj, auto_unbox = TRUE,digits = 15) # Will truncate digits if not specified which causes issues with the API as an invalid polygons
  
  # Export query (asynchronous)
  system.time(response<-POST(url, headers, body = dftj, encode = "json", query = list(
    # includeHeaders = FALSE, # Remove headers - potentially useful for batching
    # returnDeviceCountByGeoHash = TRUE, # "If true, the geoHashDeviceCount and geoHashWidthHeights fields are populated per feature" - don't see this. It does return "searchobjectid" in the response psv that corresponds to a given "id" in the json properties
    #decisionLocationTypes = list(c("LATLNG","CBG")),
    #decisionLocationTypes = "CBG",
    #includeAdditionalCbgInfo = TRUE,
    #startTimeOfDay = "09:00:00Z", # Need to adjust for timezone when binning "daily visits" - this adjusts UTC ("Z") to Eastern Time
    #endTimeOfDay = "23:59:59Z",
    #includeGeometryWithCbgInfo = TRUE, # Geometry of CBG for GIS
    #exportSchema = "EVENING_COMMON_CLUSTERS",
    compressOutputFiles = FALSE, # Compressed outputs?
    responseType = "EXPORT"  # Requesting an export response
  )))
  
  requestID<-response$headers$requestid
  status_url<-paste0("https://api.gravyanalytics.com/v1.1/requestStatus/", requestID)
  export_complete<-FALSE
  
  # Function that pings the API to see if the export request is done every 1 seconds and returns either of {files ready, still waiting, failed}
  while (!export_complete) {
    Sys.sleep(1)  # Wait for 1 seconds before polling again
    status_response<-GET(status_url, add_headers(Authorization = api_key))
    status_content<-content(status_response, "parsed")
    
    if (status_content$status == "DONE") {
      export_complete<-TRUE
      aws_s3_link<-as.character(status_content$presignedUrlsByDataType$deviceTrends)
      base::cat("Your files are ready")
    } else if (status_content$status == "FAILED") {
      stop("Export request failed. Please try again.")
    } else {
      base::cat("Export is still in progress. Status:", round(status_content$requestDurationSeconds/60,2),"m", "\n",sep = c(" ","","",""))
    }
  }
  
  # Loading export results into workspace -----------------------------------
  
  file_name<-sub("\\?.*", "", basename(aws_s3_link)) # Extracting the file name
  
  downloaded_files<-lapply(seq_along(aws_s3_link), function(i) { # Batch downloading all links returned by the API call. Mode = "wb" is important.
    file_path<-file.path("tData", file_name[i]) # Construct full path
    download.file(aws_s3_link[i], destfile = file_path, mode = "wb") # Download file
    return(file_path) # Return the file path
  })
  downloaded_files<-unlist(downloaded_files)
  
  xp<-do.call(rbind, # Row bind files into a dataframe
              lapply( # Apply over all elements in a list
                file.path("tData/", sub("\\.gz$", "", file_name)), # Elements in a list that are named based on the API call
                function(file) {read.csv(file, sep = "|", header = TRUE)})) # Reading files in
  
  invisible(unlink(downloaded_files)) # Deleting downloaded psv files
  
  if (is.null(xp) || nrow(xp) == 0) { # Handles no data situations where there are no observations in the provided polygon(s)
    warning("No data returned from API for this batch. Skipping...")
    return(NULL)  # Return NULL to avoid stopping execution
  }
  
  #xp<-merge(xp,dft, by.x = "FEATUREID", by.y = "id") # %>% dplyr::select(FEATUREID,DEVICEID,DAY_IN_FEATURE,EARLIEST_OBSERVATION_OF_DAY,LATEST_OBSERVATION_OF_DAY,LATITUDE,LONGITUDE,CENSUS_BLOCK_GROUP_ID,startDateTimeEpochMS,endDateTimeEpochMS,DEVICES_WITH_DECISION_IN_CBG_COUNT,TOTAL_POPULATION)
  #xp<-xp %>% dplyr::select(FEATUREID,DEVICEID,DAY_IN_FEATURE,EARLIEST_OBSERVATION_OF_DAY,LATEST_OBSERVATION_OF_DAY,CENSUS_BLOCK_GROUP_ID,DEVICES_WITH_DECISION_IN_CBG_COUNT,TOTAL_POPULATION)
  
  write_parquet(xp, paste0("tData/",fname,".parquet")) # Write to parquet file to save space, versus csv
}

batchapi(dg, s = as.numeric(as.POSIXct("2022-07-01 00:00:00.000", tz = "America/New_York")) * 1000,
         e = as.numeric(as.POSIXct("2025-05-31 23:59:59.999", tz = "America/New_York")) * 1000, fname = "Airportvisittrends20220701_20250531") # Jul 2022 - May 2025

dgs<-read_parquet("tData/Airportvisittrends20220701_20250531.parquet")

# Cell data tasks -------------------------------------------------------
#dfs$FEATUREID<-ifelse(dfs$FEATUREID==1,"Martha's Vineyard","Nantucket") # Location labels

dfs$EARLIEST_OBSERVATION_OF_DAY<-with_tz(as.POSIXct(dfs$EARLIEST_OBSERVATION_OF_DAY, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"), tzone = "America/New_York") # Set format to POSIXct in native UTC time zone, and convert to eastern time
dfs$LATEST_OBSERVATION_OF_DAY<-with_tz(as.POSIXct(dfs$LATEST_OBSERVATION_OF_DAY, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"), tzone = "America/New_York")

dfs$year<-year(dfs$DAY_IN_FEATURE)
dfs$instant<-ifelse(dfs$EARLIEST_OBSERVATION_OF_DAY == dfs$LATEST_OBSERVATION_OF_DAY,1,0) # Some observations have duration of stay of zero
table(dfs$year, dfs$instant, dfs$FEATUREID) # How many observations per year, how many appear to be instantaneous, by island?
dfs<-dfs %>% filter(instant == 0) # Dropping observations with no stay duration

dfs$duration_min<-as.numeric(difftime(dfs$LATEST_OBSERVATION_OF_DAY,dfs$EARLIEST_OBSERVATION_OF_DAY,units = "secs"))/60
dfs<-dfs %>% filter(duration_min>5) # 25% of observations are less than 5 minutes observed in the area, dropping those
#dfs$vd<-as.numeric(dfs$TOTAL_POPULATION)/as.numeric(dfs$DEVICES_WITH_DECISION_IN_CBG_COUNT) # Population normalized visits
dfs$vd<-1 # Each device as a visit

dfs %>% # Multiple decision locations for devices?
  group_by(DEVICEID) %>% # group_by(DEVICEID,year)
  summarize(has_variation = n_distinct(CENSUS_BLOCK_GROUP_ID), .groups = "drop") %>% 
  count(has_variation)

# Visitation model --------------------------------------------------------
df<-dfs %>% 
  group_by(DAY_IN_FEATURE) %>% 
  summarise(visits = sum(vd,na.rm = TRUE)) %>% 
  mutate(year = as.factor(year(DAY_IN_FEATURE)), dayofmonth = format(as.Date(DAY_IN_FEATURE),"%m-%d")) 

ggplot(df, aes(x = dayofmonth, y = visits, color = year, group = year)) +
  geom_line() +
  labs(x = "Day", y = "Visits", color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# df$weekendholiday<-ifelse(df$year==2022 & df$dayofmonth %in% c(2,3,4,9,10,16,17,23,24,30,31),1,
#                           ifelse(df$year==2023 & df$dayofmonth %in% c(1,2,4,8,9,15,16,22,23,29,30),1,
#                                  ifelse(df$year==2024 & df$dayofmonth %in% c(4,6,7,13,14,20,21,27,28),1,0)))

# wth<-read.csv("Data/NantucketAirportWeatherJuly2022-2024.csv") # Weather data NWS
# wth$datetime<-as.POSIXct(paste0(wth$Date..time,wth$Year), format = "%b %d, %I:%M %p %Y", tz = "America/New_York")
# wth<-wth %>% filter(format(datetime, "%H:%M:%S") >= "05:00:00", format(datetime, "%H:%M:%S") <= "20:00:00") # Only retaining weather observations that match our daily visitation window
# wth$date<-as.Date(wth$datetime)
# wth$Viskm<-as.numeric(ifelse(wth$Viskm == "< 0.4",0, ifelse(wth$Viskm == "",NA,wth$Viskm))) # Visibility in km, making numeric and replacing values
# wth$time<-format(wth$datetime, "%H:%M:%S")
# wth$X1hrprecmm<-ifelse(wth$time>"12:00:00",NA,wth$X1hrprecmm) # Only care about rain in the morning (5am - 12pm) for visitation purposes (Coombes et al. 2011)
# 
# wth<-wth %>% group_by(date) %>% # Summarizing by date
#   summarise(maxTC = max(TempC,na.rm = TRUE), minTC = min(TempC,na.rm = TRUE), raintot = sum(X1hrprecmm,na.rm = TRUE), meanviskm = mean(Viskm,na.rm = TRUE))
# 
# wth$rain<-ifelse(wth$raintot>0,1,0) # Rain indicator variable instead of amount, which has addition problems
# wth$raintot<-NULL

wth<-read.csv("Data/NANTUCKET MEMORIAL AIRPORT, MA US (USW00014756).csv") # Weather data NOAA NCEI
wth$Date<-as.Date(wth$Date,format = "%m/%d/%Y")
wth<-wth %>% filter(lubridate::month(Date) %in% c(6,7,8), lubridate::year(Date) %in% c(2022, 2023, 2024)) %>% rename(tempmaxF = TMAX..Degrees.Fahrenheit., precIn = PRCP..Inches.,date = Date) %>% 
  select(date,precIn,tempmaxF)

df<-df %>% mutate(date = as.Date(DAY_IN_FEATURE)) %>% ungroup() %>% select(!DAY_IN_FEATURE) %>%  # Ungroup is needed because DAY_IN_FEATURE was previously a grouping variable
  left_join(wth, by = "date")
rm(wth)

nt<-df #%>% filter(FEATUREID == "Nantucket")

nt<-nt %>% mutate(
    post = (date >= "2024-07-16"),
    treated_day = as.integer(post), # Indicator for treated/untreated
    day_of_week = weekdays(as.Date(date))) %>% # Day of week
  dplyr::select(-post)

nt<-nt %>% 
  mutate(temp_bin = cut(tempmaxF, breaks = c(60, 70, 80, 90, 100)))

nt$hot<-ifelse(nt$tempmaxF>80,1,0)

nt<-nt %>% 
  mutate(
    treat_day0 = as.integer(date == "2024-07-16"), # Short treatment window indicators
    treat_day1 = as.integer(date == "2024-07-17"),
    treat_day2 = as.integer(date == "2024-07-18"),
    treat_day3 = as.integer(date == "2024-07-19"),
    treat_day4 = as.integer(date == "2024-07-20"),
    treat_day5 = as.integer(date == "2024-07-21"),
    treat_day6 = as.integer(date == "2024-07-22"),
    treat_day7 = as.integer(date == "2024-07-23"),
    treat_day8 = as.integer(date == "2024-07-24"),
    treat_day9 = as.integer(date == "2024-07-25"),
    treat_day10 = as.integer(date == "2024-07-26"),
    treat_day11 = as.integer(date == "2024-07-27"),
    treat_day12 = as.integer(date == "2024-07-28"),
    treat_day13 = as.integer(date == "2024-07-29"),
    treat_day14 = as.integer(date == "2024-07-30")
  )

nt<-nt %>% 
  mutate(
    pollution_mday = "07-16",  # month-day of real event
    pollution_date = as.Date(paste0(year, "-", pollution_mday)),
    event_day = as.integer(date - pollution_date)
  ) %>% 
  dplyr::select(-c(pollution_date,pollution_mday))

# summary(model<-lm(visits ~ precIn + temp_bin, data = nt %>% filter(year == 2023)))
# coeftest(model,vcov = vcovHC,type = "HC1") # Robust standard errors

# model<-feols(visits ~ treated_day + precIn + hot | year + dayofmonth + day_of_week, vcov = "hetero", data = nt %>% filter(year %in% c("2023","2024"))) # cluster = ~ group
# 
# model<-feols(visits ~ i(event_day, ref = -1) + precIn | year + day_of_week, vcov = "hetero", data = nt %>% filter(year %in% c("2023","2024"))) # cluster = ~ group

model<-feols(visits ~ treat_day0 + treat_day1 + treat_day2 + treat_day3 + treat_day4 + treat_day5 + treat_day6 + treat_day7 + treat_day8 + treat_day9 + treat_day10 + treat_day11 + treat_day12 + treat_day13 + treat_day14 + precIn | year + dayofmonth + day_of_week, vcov = "hetero", data = nt %>% filter(year %in% c("2023","2024"))) # cluster = ~ group

summary(model)

coefs<-broom::tidy(model) %>%
  filter(str_detect(term, "^treat_day\\d+$")) %>%  # Only keep treat_day* terms
  mutate(
    day = as.integer(str_remove(term, "treat_day")),
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

ggplot(coefs, aes(x = day, y = estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Estimated Daily Effect of Pollution Event on Visitation",
    x = "Days After Pollution Event",
    y = "Estimated Effect on Visits (vs. Untreated Days)"
  ) +
  theme_minimal()
