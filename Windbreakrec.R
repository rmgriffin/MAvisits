# Intro -------------------------------------------------------------------
rm(list=ls()) # Clears workspace

# install.packages("renv") # Install/call libraries
# renv::init()

PKG<-c("googledrive","sf","tidyverse","httpuv","R.utils","httr","jsonlite","geojsonsf","lwgeom","furrr","arrow","stringr","sandwich","lmtest","fixest","digest","geosphere")

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
df<-st_simplify(df,dTolerance = 0.00001)
dg<-st_read("Data/nantucket_terminal_osm_extracted.gpkg") %>% dplyr::filter(name == "Nantucket Terminal 2")
lcw<-st_read("Data/LCWestport.gpkg") # AOI for Nantucket from satellite imagery
lcw<-st_simplify(lcw,dTolerance = 0.00001)
bi<-st_read("Data/BlockIsland.gpkg") # AOI for Nantucket from satellite imagery
bi<-st_simplify(bi,dTolerance = 0.00001)

## Want higher res cell data home locations?
api_key<-read.csv(file = "APIkey.csv", header = FALSE)
api_key<-api_key$V1
headers<-add_headers(Authorization = api_key, `Content-Type` = "application/json") # Set up the headers including the API key
url<-"https://api.gravyanalytics.com/v1.1/areas/tradeareas" # API URL to query
if (!dir.exists("tData")) { # Create cell data directory if it doesn't exist
  dir.create("tData", recursive = TRUE)
}
#df<-hex_union %>% filter(affected == "affected") # Only interested in affected areas
df<-df %>% # Union of areas for effect status
  mutate(geom = st_make_valid(geom)) %>%
  group_by(Affected) %>%
  summarise(geom = st_union(geom), .groups = "drop") %>%
  st_as_sf()
df$id<-seq(1,nrow(df),1) # API requires a variable named "id" to pass through the id to the files that are returned, named "searchobjectid" in the file
dfi<-df %>% # Union of all beach areas on island
  mutate(geom = st_make_valid(geom)) %>%
  summarise(geom = st_union(geom), .groups = "drop") %>%
  st_as_sf()
bi<-bi %>% # Union of all beach areas
  mutate(geom = st_make_valid(geom)) %>%
  summarise(geom = st_union(geom), .groups = "drop") %>%
  st_as_sf()
lcw<-lcw %>% # Union of all beach areas
  mutate(geom = st_make_valid(geom)) %>%
  summarise(geom = st_union(geom), .groups = "drop") %>%
  st_as_sf()

df<-st_transform(df, crs = 4326) # Needs to be projected in 4326 to work with lat long conventions of the API
dfi<-st_transform(dfi, crs = 4326)
dg<-st_transform(dg, crs = 4326)
lcw<-st_transform(lcw, crs = 4326)
bi<-st_transform(bi, crs = 4326)

batchapi<-function(dft,s,e,fname){ # Function converts sf object to json, passes to api, gets returned data, and merges back with sf object

  dft$startDateTimeEpochMS<-s # These work as query variables
  dft$endDateTimeEpochMS<-e
  dft$excludeFlags<-25216 # Corresponds to guidance for visitation from venntel
  # dft$startDateTimeEpochMS<-as.numeric(as.POSIXct("2023-06-15 00:00:00.000", tz = "America/New_York")) * 1000
  # dft$endDateTimeEpochMS<-as.numeric(as.POSIXct("2023-08-15 23:59:59.999", tz = "America/New_York")) * 1000
  #dft<-dft %>% select(-PUD_YR_AVG) # Need more than the geometry column to create a feature collection using sf_geojson. Also, there is a limit of 20 features per request (even if it doesn't return results for 20 features).
  dftj<-sf_geojson(dft,atomise = FALSE) # Convert sf object to GeoJSON

  dftj<-fromJSON(dftj) # Doesn't seem to like geojson formatting, switching to json
  dftj<-toJSON(dftj, auto_unbox = TRUE, digits = 15)
  g1<-st_geometry(dft)
  g2<-st_geometry(st_read(dftj, quiet = TRUE))
  suppressWarnings(st_crs(g2) <- st_crs(g1))
  
  if (all(lengths(st_equals(g1, g2))>0)) {
    message("Geometry conversion to JSON consistent")
  } else {
    warning("Geometry conversion to JSON inconsistent")
  }

  # Export query (asynchronous)
  response<-POST(url, headers, body = dftj, encode = "json", query = list(
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
  ))

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

batchapi(lcw, s = as.numeric(as.POSIXct("2023-06-15 00:00:00.000", tz = "America/New_York")) * 1000,
         e = as.numeric(as.POSIXct("2023-08-15 23:59:59.999", tz = "America/New_York")) * 1000, fname = "JunAug2023daylight_lcw") # Jun - Aug 2023
batchapi(lcw, s = as.numeric(as.POSIXct("2024-06-15 00:00:00.000", tz = "America/New_York")) * 1000,
         e = as.numeric(as.POSIXct("2024-08-15 23:59:59.999", tz = "America/New_York")) * 1000, fname = "JunAug2024daylight_lcw") # Jun - Aug 2024
batchapi(bi, s = as.numeric(as.POSIXct("2023-06-15 00:00:00.000", tz = "America/New_York")) * 1000,
         e = as.numeric(as.POSIXct("2023-08-15 23:59:59.999", tz = "America/New_York")) * 1000, fname = "JunAug2023daylight_bi") # Jun - Aug 2023
batchapi(bi, s = as.numeric(as.POSIXct("2024-06-15 00:00:00.000", tz = "America/New_York")) * 1000,
         e = as.numeric(as.POSIXct("2024-08-15 23:59:59.999", tz = "America/New_York")) * 1000, fname = "JunAug2024daylight_bi") # Jun - Aug 2024
batchapi(df, s = as.numeric(as.POSIXct("2023-06-15 00:00:00.000", tz = "America/New_York")) * 1000,
         e = as.numeric(as.POSIXct("2023-08-15 23:59:59.999", tz = "America/New_York")) * 1000, fname = "JunAug2023daylight_df") # Jun - Aug 2023
batchapi(df, s = as.numeric(as.POSIXct("2024-06-15 00:00:00.000", tz = "America/New_York")) * 1000,
         e = as.numeric(as.POSIXct("2024-08-15 23:59:59.999", tz = "America/New_York")) * 1000, fname = "JunAug2024daylight_df") # Jun - Aug 2024
batchapi(dfi, s = as.numeric(as.POSIXct("2023-06-15 00:00:00.000", tz = "America/New_York")) * 1000,
         e = as.numeric(as.POSIXct("2023-08-15 23:59:59.999", tz = "America/New_York")) * 1000, fname = "JunAug2023daylight_dfi") # Jun - Aug 2023
batchapi(dfi, s = as.numeric(as.POSIXct("2024-06-15 00:00:00.000", tz = "America/New_York")) * 1000,
         e = as.numeric(as.POSIXct("2024-08-15 23:59:59.999", tz = "America/New_York")) * 1000, fname = "JunAug2024daylight_dfi") # Jun - Aug 2024

dfs <- list.files("tData/", pattern = "\\.parquet$", full.names = TRUE) %>%
  map_dfr(function(f) {
    d.f <- read_parquet(f)
    f.name <- basename(f)
    
    # Extract suffix
    match <- str_match(f.name, "JunAug(\\d{4})daylight_(\\w+)\\.parquet")
    source <- match[3]
    
    d.f %>%
      mutate(FEATUREID = as.character(FEATUREID),
             source = source)
  })

# Calibration model -------------------------------------------------------
url<-"https://api.gravyanalytics.com/v1.1/observations/geo/search" 

batchapi<-function(dft,s,e,fname){ # Function converts sf object to json, passes to api, gets returned data, and merges back with sf object
  
  dft$startDateTimeEpochMS<-s  
  dft$endDateTimeEpochMS<-e
  dft$excludeFlags<-25216 # Corresponds to guidance for visitation from venntel
  # dft$startDateTimeEpochMS<-as.numeric(as.POSIXct("2022-08-01 00:00:00.000", tz = "America/New_York")) * 1000
  # dft$endDateTimeEpochMS<-as.numeric(as.POSIXct("2025-05-31 00:00:00.000", tz = "America/New_York")) * 1000
  dftj<-sf_geojson(dft,atomise = FALSE) # Convert sf object to GeoJSON
  
  dftj<-fromJSON(dftj) # Doesn't seem to like geojson formatting, switching to json
  dftj<-toJSON(dftj, auto_unbox = TRUE,digits = 15) # Will truncate digits if not specified which causes issues with the API as an invalid polygons
  g1<-st_geometry(dft)
  g2<-st_geometry(st_read(dftj, quiet = TRUE))
  suppressWarnings(st_crs(g2) <- st_crs(g1))
  
  if (all(lengths(st_equals(g1, g2))>0)) {
    message("Geometry conversion to JSON consistent")
  } else {
    warning("Geometry conversion to JSON inconsistent")
  }
  
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
      aws_s3_link<-as.character(status_content$presignedUrlsByDataType$`observations-geo`)
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

batchapi(dg, s = as.numeric(as.POSIXct("2022-08-01 00:00:00.000", tz = "America/New_York")) * 1000,
         e = as.numeric(as.POSIXct("2025-05-31 23:59:59.999", tz = "America/New_York")) * 1000, fname = "Airportvisittrends20220801_20250531") # Aug 2022 - May 2025

dgs<-read_parquet("tData/Airportvisittrends20220801_20250531.parquet")


url<-"https://api.gravyanalytics.com/v1.1/observations/registrationID/search" 

ids<-as.list(unique(dgs$REGISTRATION_ID))

polapi<-function(ids,s,e,fpath,fname_prefix = "batch_"){
  request_body<-list(registrationIDs = ids)
  
  json_data<-toJSON(request_body, auto_unbox = TRUE, pretty = TRUE) # JSON validator https://geojsonlint.com/ 
  
  response <- POST(url, headers, body = json_data, encode = "json", query = list(
    responseType = "EXPORT",  # Requesting an export response
    # startDateTimeEpochMS = as.numeric(as.POSIXct("2022-08-01 00:00:00.000", tz = "America/New_York")) * 1000,
    # endDateTimeEpochMS = as.numeric(as.POSIXct("2025-05-31 00:00:00.000", tz = "America/New_York")) * 1000,
    startDateTimeEpochMS = s,
    endDateTimeEpochMS = e,
    returnObservations = TRUE,
    compressOutputFiles = FALSE, # Compressed outputs?
    observationLocationTypes = "LATLNG" # Uses a geohash approach for returning results. Default is 3 decimal places (~110m resolution) 
  ))
  
  if (status_code(response) != 200) {
    if (length(ids) > 1) { # Batch failed: break into individual calls
      message("Batch failed with status ", status_code(response), ". Retrying individually...")
      for (i in seq_along(ids)) {
        single_id <- ids[[i]]
        polapi(single_id, s, e, fpath = fpath, fname_prefix = paste0("retry_", fname_prefix))
      }
    } else { # Single item failed: log and skip
      id_val <- as.character(ids[[1]])
      message(id_val, " failed with status ", status_code(response),
              " and request id ", response$headers$requestid, ". Skipping.")
    }
    return(invisible(NULL))
  }
  
  requestID<-response$headers$requestid
  status_url<-paste0("https://api.gravyanalytics.com/v1.1/requestStatus/", requestID)
  export_complete<-FALSE
  
  # Ping the API periodically and report status
  while (!export_complete) {
    Sys.sleep(10)  # Wait for 10 seconds before polling again
    status_response <- GET(status_url, add_headers(Authorization = api_key))
    status_content <- content(status_response, "parsed")
    
    if (status_content$status == "DONE") {
      export_complete <- TRUE
      if (!is.null(status_content$message) && status_content$message == "No files were exported") {
        message(paste(ids, collapse = ", "),
                " API query completed (status ", status_code(response),
                ") but no files were exported")
        return(invisible(NULL))
      }
      aws_s3_link <- as.character(status_content$presignedUrlsByDataType$`observations-id`)
      base::cat("Your files are ready.\n")} 
    
    else {
      base::cat("Export is still in progress. Status:",
                round(status_content$requestDurationSeconds / 60, 2), "m\n")
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
  
  # Construct output filename
  if (length(ids) == 1) {
    fname<-paste0(fname_prefix, as.character(ids[[1]]))
  } else {
    id_hash<-digest::digest(paste0(sort(unique(as.character(ids))), collapse = "_"))
    fname<-paste0(fname_prefix, id_hash)
  }
  
  write_parquet(xp, paste0(fpath,fname,".parquet")) # Write to parquet file to save space, versus csv
}

split_ids<-unname(split(ids, ceiling(seq_along(ids)/1000))) # Pattern of life api can only handle 1000 registration ids per query

plan(sequential)
plan(multisession, workers = 2) # Initializing parallel processing, API can only handle two concurrent connections
set.seed(12)

# polapi(split_ids[[1]],s = as.numeric(as.POSIXct("2022-08-01 00:00:00.000", tz = "America/New_York")) * 1000,e = as.numeric(as.POSIXct("2025-05-31 23:59:59.999", tz = "America/New_York")) * 1000,fpath = "tData/")

system.time(future_imap(
  split_ids,
  function(data, index) {
    cat("Processing index:", index, "\n")
    polapi(
      data,
      s = as.numeric(as.POSIXct("2022-08-01 00:00:00.000", tz = "America/New_York")) * 1000,
      e = as.numeric(as.POSIXct("2025-05-31 23:59:59.999", tz = "America/New_York")) * 1000,
      fpath = "tData/" # Filepath of output
    )
  },
  .options = furrr_options(
    packages = c("R.utils", "httr", "tidyverse", "jsonlite", "sf", "geojsonsf", "lwgeom", "furrr", "arrow","digest"),
    seed = TRUE
  ),
  .progress = TRUE
))

dds<-map_df(list.files("tData/", pattern = "^batch_.*\\.parquet$", full.names = TRUE), read_parquet)


dgs$date<-as.Date(as.POSIXct(dgs$TIMESTAMP_EPOCH_MS / 1000, origin = "1970-01-01", tz = "UTC"), tz = "America/New_York")

dds$date<-as.Date(as.POSIXct(dds$TIMESTAMP_EPOCH_MS / 1000, origin = "1970-01-01", tz = "UTC"), tz = "America/New_York")

movement_summary <- dgs %>%
  group_by(REGISTRATION_ID, date) %>%
  slice_min(order_by = TIMESTAMP_EPOCH_MS, n = 1, with_ties = FALSE) %>%
  rename(
    LAT_REF = LATITUDE,
    LON_REF = LONGITUDE,
    FIRST_TIME_MS = TIMESTAMP_EPOCH_MS
  ) %>%
  select(REGISTRATION_ID, date, LAT_REF, LON_REF, FIRST_TIME_MS) %>%
  inner_join(dds, by = c("REGISTRATION_ID", "date")) %>%
  filter(TIMESTAMP_EPOCH_MS > FIRST_TIME_MS) %>%
  mutate(dist_from_ref_km = distHaversine(
    cbind(LONGITUDE, LATITUDE),
    cbind(LON_REF, LAT_REF)
  )/1000) %>%
  group_by(REGISTRATION_ID, date) %>%
  summarize(
    moved_far = any(dist_from_ref_km > 40),
    max_dist_m = max(dist_from_ref_km),
    .groups = "drop"
  )

ggplot(movement_summary, aes(x = max_dist_m, fill = moved_far)) +
  geom_histogram(position = "identity", bins = 50) +
  scale_x_log10() +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +
  labs(
    title = "Histogram of maximum intra-day distance (log scale)",
    x = "Max distance (km, log10)",
    y = "Count",
    fill = "> 40km"
  ) +
  theme_minimal()

ms_enplanements<-movement_summary %>% # Monthly counts of enplaned devices
  filter(moved_far == TRUE) %>%
  mutate(month_year = format(date, "%Y-%m")) %>%
  distinct(date, REGISTRATION_ID, month_year) %>%
  count(month_year, name = "counts_enplaned")

ms<-dgs %>% # Counts of unique device days in the airport based on total devices, and devices seen >40km from airport after an airport visit on the same day
  mutate(month_year = format(date, "%Y-%m")) %>%
  distinct(date, REGISTRATION_ID, month_year) %>%
  count(month_year, name = "counts_all") %>% 
  left_join(ms_enplanements, by = "month_year") %>% 
  mutate(ratio_alldiven = counts_all/counts_enplaned)

pl<-read.csv("Data/Planements.csv")

# pl %>% # Planements by year
#   group_by(Year,Total.Route) %>% 
#   summarise(Enplanements = sum(Enplanements,na.rm = TRUE),Deplanements = sum(Deplanements,na.rm = TRUE)) %>% 
#   mutate(RatioEnDe = Enplanements/Deplanements) 
# 
# pl %>% # Planements overall
#   group_by(Total.Route) %>% 
#   summarise(Enplanements = sum(Enplanements,na.rm = TRUE),Deplanements = sum(Deplanements,na.rm = TRUE)) %>% 
#   mutate(RatioEnDe = Enplanements/Deplanements) 

ms<-ms %>% 
  left_join(pl %>%
              mutate(month_year = sprintf("%d-%02d", Year, Month)) %>% filter(Total.Route == "T") %>% select(month_year,Enplanements), by = "month_year") %>% 
  mutate(ratio_enplaned = Enplanements/counts_enplaned,
         ratio_all = Enplanements/counts_all, 
         year = as.integer(substr(month_year, 1, 4))) %>% 
  filter(year != 2025)

r2_labels <- ms %>%
  group_by(year) %>%
  do({
    model <- lm(Enplanements ~ counts_enplaned, data = .)
    data.frame(
      r2 = summary(model)$r.squared,
      x = max(.$Enplanements, na.rm = TRUE),
      y = max(.$counts_enplaned, na.rm = TRUE)
    )
  })

ggplot(ms, aes(x = Enplanements, y = counts_enplaned)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(
    data = r2_labels,
    aes(x = x, y = y, label = paste0("R² = ", round(r2, 2))),
    inherit.aes = FALSE,
    hjust = 1, vjust = 1
  ) +
  facet_wrap(~ year) +
  theme_minimal() 

cf<-ms %>% filter(month_year == "2024-07") %>% select(ratio_enplaned) %>% as.numeric() # Conversion factor for July 2024

# Visitation model --------------------------------------------------------

#dfs$FEATUREID<-ifelse(dfs$FEATUREID==1,"Martha's Vineyard","Nantucket") # Location labels

dfs$EARLIEST_OBSERVATION_OF_DAY<-with_tz(as.POSIXct(dfs$EARLIEST_OBSERVATION_OF_DAY, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"), tzone = "America/New_York") # Set format to POSIXct in native UTC time zone, and convert to eastern time
dfs$LATEST_OBSERVATION_OF_DAY<-with_tz(as.POSIXct(dfs$LATEST_OBSERVATION_OF_DAY, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"), tzone = "America/New_York")

dfs$year<-year(dfs$EARLIEST_OBSERVATION_OF_DAY)
dfs$instant<-ifelse(dfs$EARLIEST_OBSERVATION_OF_DAY == dfs$LATEST_OBSERVATION_OF_DAY,1,0) # Some observations have duration of stay of zero
table(dfs$year, dfs$instant, dfs$FEATUREID) # How many observations per year, how many appear to be instantaneous, by island?
dfs<-dfs %>% filter(instant == 0) # Dropping observations with no stay duration

dfs$duration_min<-as.numeric(difftime(dfs$LATEST_OBSERVATION_OF_DAY,dfs$EARLIEST_OBSERVATION_OF_DAY,units = "secs"))/60
dfs<-dfs %>% filter(duration_min>5) # 25% of observations are less than 5 minutes observed in the area, dropping those
#dfs$vd<-as.numeric(dfs$TOTAL_POPULATION)/as.numeric(dfs$DEVICES_WITH_DECISION_IN_CBG_COUNT) # Population normalized visits
dfs$vd<-1 # Each device as a visitor day

dfs$source<-ifelse(
  dfs$source == "bi", "Block Island",
  ifelse(dfs$source == "lcw","Little Compton and Westport",
         ifelse(dfs$source %in% c( "df","dfi"),"Nantucket",dfs$source)))
dfs$FEATUREID<-ifelse(dfs$FEATUREID %in% c("1","2","3"),dfs$FEATUREID,NA)

dfs %>% # Multiple decision locations for devices?
  filter(is.na(FEATUREID)) %>%
  group_by(DEVICEID) %>% # group_by(DEVICEID,year)
  summarize(distinct_home_census_blocks = n_distinct(CENSUS_BLOCK_GROUP_ID), .groups = "drop") %>% 
  count(distinct_home_census_blocks)

dfs<-dfs %>%
  left_join(
    dfs %>%
      filter(is.na(FEATUREID)) %>% 
      distinct(DEVICEID, source) %>%
      group_by(DEVICEID) %>%
      summarise(groups_seen_in = paste(sort(unique(source)), collapse = ", "), .groups = "drop"),
    by = "DEVICEID"
  )

dfs %>% filter(is.na(FEATUREID)) %>% count(groups_seen_in) # Summary of overlap in devices across areas


# Visitation model --------------------------------------------------------
df<-dfs %>% 
  filter(is.na(FEATUREID)) %>% # FeatureID {3 - Affected areas, 2 - Unaffected areas, 1 - Maybe affected areas (inland marsh areas adjacent to affected areas)}
  group_by(DAY_IN_FEATURE,source) %>% 
  summarise(visits = sum(vd,na.rm = TRUE)) %>% 
  mutate(year = as.factor(year(DAY_IN_FEATURE)), dayofmonth = format(as.Date(DAY_IN_FEATURE),"%m-%d")) 

ggplot(df, aes(x = dayofmonth, y = visits, color = year, group = year)) +
  geom_line() +
  labs(x = "Day", y = "Visits", color = "Year") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  geom_vline(xintercept = "07-16", linetype = "dashed", color = "black") +
  theme_minimal() +
  scale_x_discrete(breaks = unique(df$dayofmonth)[seq(1, length(unique(df$dayofmonth)), by = 14)]) +
  facet_wrap(~source)

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

wth<-list.files("Data/", pattern = "AIRPORT.*\\.csv$", full.names = TRUE) %>% # Weather data NOAA NCEI
  map_dfr(function(f) {
    read_csv(f, show_col_types = FALSE) %>%
      mutate(station = str_remove(basename(f), "\\.csv$"))
  })

wth$Date<-as.Date(wth$Date,format = "%m/%d/%Y")
wth<-wth %>% filter(lubridate::month(Date) %in% c(6,7,8), lubridate::year(Date) %in% c(2023, 2024)) %>% rename(tempmaxF = `TMAX (Degrees Fahrenheit)`, precIn = `PRCP (Inches)`,date = Date) %>% 
  select(date,precIn,tempmaxF,station)

# wth %>% # Missing data check
#   group_by(source) %>%
#   summarise(across(everything(), ~sum(is.na(.)), .names = "na_{.col}"))

df<-df %>% mutate(date = as.Date(DAY_IN_FEATURE), 
                   station = case_when(source %in% c("Block Island", "Little Compton and Westport") ~ "NEWPORT STATE AIRPORT, RI US (USW00014787)",
                                       source == "Nantucket" ~ "NANTUCKET MEMORIAL AIRPORT, MA US (USW00014756)",
                                       TRUE ~ NA)) %>% 
  ungroup() %>% # Ungroup is needed because DAY_IN_FEATURE was previously a grouping variable
  select(!DAY_IN_FEATURE) %>%
  left_join(wth, by = c("date","station"))
rm(wth)

df<-df %>%
  filter(format(date, "%m") == "07") %>% # Only July dates to avoid contamination that began in LC and Westport starting 8/1
  mutate(
    post = date >= as.Date("2024-07-16") , # TRUE if on or after July 16
    treated = source == "Nantucket", # TRUE only for Nantucket
    treat_post = post * treated, # DiD interaction term
    temp_bin = factor(cut(tempmaxF, breaks = c(60, 70, 80, 90, 100), right = TRUE)),
    day_of_week = weekdays(as.Date(date))
  )

model<- feols(
  visits ~ treat_post + temp_bin + precIn | source + dayofmonth + day_of_week + year,
  data = df,
  vcov = "hetero"
)

summary(model)

total_effect<-as.numeric(coef(model)["treat_post"] * 16 * cf)


# Robustness tests --------------------------------------------------------
# Pre-trends
# ggplot(df %>% filter(post == FALSE), aes(x = date, y = visits, color = source)) + # Plot
#   geom_line() +
#   facet_wrap(~ year, scales = "free_x") +
#   labs(
#     x = "Date",
#     y = "Visits",
#     color = "Beach",
#     title = "Pre-Treatment trends by location and year"
#   ) +
#   theme_minimal()

## Need a residual version of the above

pre_df <- df %>%
  filter(
    format(date, "%m-%d") >= "07-01" & format(date, "%m-%d") <= "07-15",
    year %in% c(2023, 2024)
  ) %>% 
  mutate(time = as.numeric(date - min(date) + 1))

pretrend_model <- feols(
  visits ~ time + treated:time + temp_bin + precIn | day_of_week + dayofmonth + year,
  data = pre_df,
  vcov = "hetero"
)

summary(pretrend_model)

# Placebo test


