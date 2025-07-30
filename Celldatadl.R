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

# # Download data -----------------------------------------------------------
# dir.create(file.path('Data'), recursive = TRUE)
# folder_url<-"https://drive.google.com/open?id=15amwG3br43cpU9MS8gghNcYhljHoi52I"
# folder<-drive_get(as_id(folder_url))
# files<-drive_ls(folder)
# dl<-function(files){
#   walk(files, ~ drive_download(as_id(.x), overwrite = TRUE))
# }
# setwd("./Data")
# system.time(map(files$id,dl))
# system.time(unzip("Data.zip", exdir = "."))
# file.remove("Data.zip")
# setwd("..")
# rm(files, folder, folder_url, dl)


# Load data into workspace ---------------------------------------------------
nt<-st_transform(st_read("Data/GSNantucketSBeaches.gpkg"), crs = 4326) %>% select(!Affected) # AOI for Nantucket from satellite imagery, projected in 4326 to work with conventions of the API
dg<-st_transform(st_read("Data/nantucket_terminal_osm_extracted.gpkg"), crs = 4326) %>% dplyr::filter(name == "Nantucket Terminal 2")
dgnt<-st_transform(st_read("Data/MAmunicipalities.gpkg"), crs = 4326) %>% dplyr::filter(TOWN == "NANTUCKET") %>% rename(City = TOWN) %>% dplyr::select(City) %>% mutate(City = "Nantucket")
lcw<-st_transform(st_read("Data/LCWestport.gpkg"), crs = 4326) # AOI for Little Compton and Westport from satellite imagery
bi<-st_transform(st_read("Data/BlockIsland.gpkg"), crs = 4326) # AOI for Block Island from satellite imagery
ai<-st_transform(st_read("Data/AquidneckIsland.gpkg"), crs = 4326) # AOI for Aquidneck Island from satellite imagery
jw<-st_transform(st_read("Data/JamestownWesternRI.gpkg"), crs = 4326) # AOI for Jamestown and Western Rhode Island from satellite imagery
mv<-st_transform(st_read("Data/MV.gpkg") %>% dplyr::select(Name, City, State, geom), crs = 4326) # AOI for Martha's Vineyard from satellite imagery

al<-bind_rows(lcw,bi,ai,jw,mv,nt)
al$id<-seq(1,nrow(al),1) # API requires a variable named "id" to pass through the id to the files that are returned, named "searchobjectid" in the file, also need more than one column

al<-st_simplify(st_make_valid(al),dTolerance = 0.00001)

dg$id<-seq(1,nrow(dg),1)
dgnt$id<-seq(1,nrow(dgnt),1)

rm(ai,bi,jw,lcw,mv,nt)


# API preparation ---------------------------------------------------------
## Want higher res cell data home locations?
api_key<-read.csv(file = "APIkey.csv", header = FALSE)
api_key<-api_key$V1
headers<-add_headers(Authorization = api_key, `Content-Type` = "application/json") # Set up the headers including the API key
url<-"https://api.gravyanalytics.com/v1.1/areas/tradeareas" # API URL to query
if (!dir.exists("tData")) { # Create cell data directory if it doesn't exist
  dir.create("tData", recursive = TRUE)
}


# Trade areas api query ---------------------------------------------------
tradeapi<-function(dft,s,e,fpath,fname_prefix = "batch_"){ # Function converts sf object to json, passes to api, gets returned data, and merges back with sf object
  
  dft_name<-deparse(substitute(dft)) # Extract the name of the object passed to the function for naming
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
  
  if (status_code(response) != 200) {
    if (nrow(dft) > 1) {
      # Batch failed: break into individual calls
      message("Batch failed with status ", status_code(response), ". Retrying rows individually...")
      for (i in seq_len(nrow(dft))) {
        single_row<-dft[i, , drop = FALSE]
        tradeapi(single_row, s, e, fpath = fpath, fname_prefix = paste0("retry_", fname_prefix))
      }
    } else {
      # Individual row failed: log and skip
      message(dft$id, " failed with status ", status_code(response), "and request id ", response$headers$requestid, ". Skipping.")
      tryCatch({ # If failing, check to see if input sf object is valid, if so check to see if derived JSON is valid
        if (!st_is_valid(dft)) {
          message("Geometry is invalid for id ", dft$id, " from original sf object")
        } else {
          tryCatch({
            sf_from_json <- st_read(dftj, quiet = TRUE)
            if (!st_is_valid(sf_from_json)) {
              message("Geometry is invalid for id ", dft$id, " after GeoJSON conversion")
            }
          }, error = function(e) {
            # Silent fail
          })
        }
      })
    }
    return(invisible(NULL))
  }
  
  requestID<-response$headers$requestid
  status_url<-paste0("https://api.gravyanalytics.com/v1.1/requestStatus/", requestID)
  export_complete<-FALSE
  
  # Function that pings the API to see if the export request is done every 1 seconds and returns either of {files ready, still waiting}
  while (!export_complete) {
    Sys.sleep(10)  # Wait for 10 seconds before polling again
    status_response <- GET(status_url, add_headers(Authorization = api_key))
    status_content <- content(status_response, "parsed")
    
    if (status_content$status == "DONE") {
      export_complete <- TRUE
      if (!is.null(status_content$message) && status_content$message == "No files were exported") {
        message(dft$id," API query completed (status ",status_code(response), ") but no files were exported")
        return(invisible(NULL))
      }
      aws_s3_link <- as.character(status_content$presignedUrlsByDataType$tradeAreas)
      base::cat("Your files are ready.\n")} 
    
    else {
      base::cat("Export is still in progress. Status:",
                round(status_content$requestDurationSeconds / 60, 2), "m\n")
    }
  }
  
  # Loading export results into workspace -----------------------------------
  file_name<-sub("\\?.*", "", basename(aws_s3_link)) # Extracting the file name
  
  downloaded_files<-lapply(seq_along(aws_s3_link), function(i) { # Batch downloading all links returned by the API call. Mode = "wb" is important.
    file_path<-file.path(fpath, file_name[i]) # Construct full path
    download.file(aws_s3_link[i], destfile = file_path, mode = "wb") # Download file
    return(file_path) # Return the file path
  })
  downloaded_files<-unlist(downloaded_files)
  
  xp<-do.call(rbind, # Row bind files into a dataframe
              lapply( # Apply over all elements in a list
                file.path(fpath, sub("\\.gz$", "", file_name)), # Elements in a list that are named based on the API call
                function(file) {read.csv(file, sep = "|", header = TRUE)})) # Reading files in
  
  invisible(unlink(downloaded_files)) # Deleting downloaded psv files
  
  if (is.null(xp) || nrow(xp) == 0) { # Handles no data situations where there are no observations in the provided polygon(s)
    warning("No data returned from API for this batch. Skipping...")
    return(NULL)  # Return NULL to avoid stopping execution
  }
  
  #xp<-merge(xp,dft, by.x = "FEATUREID", by.y = "id") # %>% dplyr::select(FEATUREID,DEVICEID,DAY_IN_FEATURE,EARLIEST_OBSERVATION_OF_DAY,LATEST_OBSERVATION_OF_DAY,LATITUDE,LONGITUDE,CENSUS_BLOCK_GROUP_ID,startDateTimeEpochMS,endDateTimeEpochMS,DEVICES_WITH_DECISION_IN_CBG_COUNT,TOTAL_POPULATION)
  xp<-xp %>% dplyr::select(FEATUREID,DEVICEID,DAY_IN_FEATURE,EARLIEST_OBSERVATION_OF_DAY,LATEST_OBSERVATION_OF_DAY,CENSUS_BLOCK_GROUP_ID)
  
  fname<-paste0(fname_prefix, dft_name, "_", paste0(unique(dft$id), collapse = "_"))
  write_parquet(xp, paste0(fpath,fname,".parquet")) # Write to parquet file to save space, versus csv
}

# tradeapi(dft = dgnt,s = as.numeric(as.POSIXct("2022-08-01 00:00:00.000", tz = "America/New_York")) * 1000,
#          e = as.numeric(as.POSIXct("2025-05-31 23:59:59.999", tz = "America/New_York")) * 1000, fpath = "tData/",fname_prefix = "dgnt")

dgnt<-read_parquet("tData/dgntdgnt_1.parquet")

# split_al<-split(al, ceiling(seq_len(nrow(al))/20)) # api returns 404 error if even one polygon in the batch has a problem
# 
# plan(sequential)
# plan(multisession, workers = 2) # Initializing parallel processing, API can only handle two concurrent connections
# set.seed(12)
# 
# system.time(future_imap(
#   split_al,
#   function(data, index) {
#     cat("Processing index:", index, "\n")
#     tradeapi(
#       data,
#       s = as.numeric(as.POSIXct("2023-06-15 00:00:00.000", tz = "America/New_York")) * 1000,
#       e = as.numeric(as.POSIXct("2023-08-15 23:59:59.999", tz = "America/New_York")) * 1000,
#       fpath = "tData/", # Filepath of output
#       fname_prefix = 2023
#     )
#   },
#   .options = furrr_options(
#     packages = c("R.utils", "httr", "tidyverse", "jsonlite", "sf", "geojsonsf", "lwgeom", "furrr", "arrow"),
#     seed = TRUE
#   ),
#   .progress = TRUE
# ))
# 
# system.time(future_imap(
#   split_al,
#   function(data, index) {
#     cat("Processing index:", index, "\n")
#     tradeapi(
#       data,
#       s = as.numeric(as.POSIXct("2024-06-15 00:00:00.000", tz = "America/New_York")) * 1000,
#       e = as.numeric(as.POSIXct("2024-08-15 23:59:59.999", tz = "America/New_York")) * 1000,
#       fpath = "tData/", # Filepath of output
#       fname_prefix = 2024
#     )
#   },
#   .options = furrr_options(
#     packages = c("R.utils", "httr", "tidyverse", "jsonlite", "sf", "geojsonsf", "lwgeom", "furrr", "arrow"),
#     seed = TRUE
#   ),
#   .progress = TRUE
# ))

dfs <- list.files("tData/", pattern = "^(2023data|2024data).*\\.parquet$", full.names = TRUE) %>% 
  map_dfr(function(f) {
    d.f <- read_parquet(f)
    f.name <- basename(f)
    
    # Extract prefix without year
    match <- str_match(f.name, "^.{4}([^_]+)_")
    
    d.f %>%
      mutate(FEATUREID = as.character(FEATUREID),
             CENSUS_BLOCK_GROUP_ID = as.character(CENSUS_BLOCK_GROUP_ID))
  })

dfs$FEATUREID<-as.numeric(dfs$FEATUREID)
dfs<-dfs %>% rename(id = FEATUREID)

dfs<-dfs %>%
  left_join(al, by = "id") %>% 
  mutate()


# Calibration model api queries -------------------------------------------------------
# Identifying device ids within AOI of Nantucket Airport terminal
url<-"https://api.gravyanalytics.com/v1.1/observations/geo/search" 

geosearchapi<-function(dft,s,e,fname){ # Function converts sf object to json, passes to api, gets returned data, and merges back with sf object
  
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

# geosearchapi(dg, s = as.numeric(as.POSIXct("2022-08-01 00:00:00.000", tz = "America/New_York")) * 1000,
#          e = as.numeric(as.POSIXct("2025-05-31 23:59:59.999", tz = "America/New_York")) * 1000, fname = "Airportvisittrends20220801_20250531") # Aug 2022 - May 2025

dgs<-read_parquet("tData/Airportvisittrends20220801_20250531.parquet")

# Identifying all pings for ids observed in the airport terminal

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

# split_ids<-unname(split(ids, ceiling(seq_along(ids)/1000))) # Pattern of life api can only handle 1000 registration ids per query
# 
# plan(sequential)
# plan(multisession, workers = 2) # Initializing parallel processing, API can only handle two concurrent connections
# set.seed(12)
# 
# # polapi(split_ids[[1]],s = as.numeric(as.POSIXct("2022-08-01 00:00:00.000", tz = "America/New_York")) * 1000,e = as.numeric(as.POSIXct("2025-05-31 23:59:59.999", tz = "America/New_York")) * 1000,fpath = "tData/")
# 
# system.time(future_imap(
#   split_ids,
#   function(data, index) {
#     cat("Processing index:", index, "\n")
#     polapi(
#       data,
#       s = as.numeric(as.POSIXct("2022-08-01 00:00:00.000", tz = "America/New_York")) * 1000,
#       e = as.numeric(as.POSIXct("2025-05-31 23:59:59.999", tz = "America/New_York")) * 1000,
#       fpath = "tData/" # Filepath of output
#     )
#   },
#   .options = furrr_options(
#     packages = c("R.utils", "httr", "tidyverse", "jsonlite", "sf", "geojsonsf", "lwgeom", "furrr", "arrow","digest"),
#     seed = TRUE
#   ),
#   .progress = TRUE
# ))

dds<-map_df(list.files("tData/", pattern = "^batch_.*\\.parquet$", full.names = TRUE), read_parquet)

rm(ids,api_key,url,dg,geosearchapi,polapi,tradeapi,headers)
