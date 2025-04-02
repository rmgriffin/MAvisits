# Intro -------------------------------------------------------------------
rm(list=ls()) # Clears workspace

# install.packages("renv") # Install/call libraries
# renv::init()

PKG <- c("googledrive","sf","tidyverse","httpuv","R.utils","httr","jsonlite","geojsonsf","lwgeom","furrr","arrow","stringr")

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p)
    require(p,character.only = TRUE)}
}

renv::snapshot()
rm(p,PKG)

# Load data -----------------------------------------------------------
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

# Analysis -----------------------------------------------------------
ESI<-st_read("Data/MV_N_ESIL.gpkg")
ESI<-st_transform(ESI,crs = 32619) # Project to UTM 19N (in meters)

bbox<-st_bbox(ESI)
hex_grid<-st_make_grid(ESI, cellsize = 250, square = FALSE,crs = st_crs(ESI)) %>%
  st_as_sf() %>%
  mutate(id = row_number())
  
system.time(hex_grid<-st_filter(hex_grid, ESI))

hex_grid$loc<-ifelse(st_coordinates(st_centroid(hex_grid))[, 1]> 385000, "Nantucket","Martha's Vineyard")  

hullN<-st_buffer(st_cast(st_convex_hull(st_union(hex_grid[hex_grid$loc=="Nantucket",])),"MULTILINESTRING"),250) # 250 meter buffered convex hull around Nantucket

hullMV<-st_buffer(st_cast(st_concave_hull(st_union(hex_grid[hex_grid$loc=="Martha's Vineyard",]),ratio = 0.01),"MULTILINESTRING"),250) # 250 meter buffered concave hull around MV, messed with the ratio parameter visually to calibrate to outcome

hex_grid$affected<-ifelse(st_intersects(hex_grid, hullN, sparse = FALSE), "affected", # Convex hull Nantucket
                          ifelse(st_coordinates(st_centroid(hex_grid))[, 2] < 4567000, "affected", # Southern indent convex hull Nantucket
                                 ifelse(st_intersects(hex_grid, hullMV, sparse = FALSE), "affected", "unaffected"))) # Concave hull MV 
                                        

hex_grid$affected<-ifelse(st_coordinates(st_centroid(hex_grid))[, 1] < 351700, "unaffected", hex_grid$affected)
hex_grid$affected<-ifelse(st_coordinates(st_centroid(hex_grid))[, 2] > 4578995 & st_coordinates(st_centroid(hex_grid))[, 1] < 378600, "unaffected", hex_grid$affected)

hex_grid$affected<-ifelse(hex_grid$id %in% c(30489,25366),"affected",hex_grid$affected) # Few manual fixes

ggplot() + # Checking layers
  #geom_sf(data = hex_grid[hex_grid$loc=="Nantucket"&hex_grid$affected=="affected",], fill = "lightblue", color = "black", alpha = 0.5) +  # Hexagonal grid
  geom_sf(data = hex_grid[hex_grid$affected=="affected",], fill = "lightblue", color = "black", alpha = 0.5) +
  #geom_sf(data = ESI, color = "red", size = 1.5) +  # Line feature
  #geom_sf(data = hullMV, color = "green", alpha = 0.5) +
  theme_minimal()

# st_write(hex_grid,"hex_grid.gpkg")

hex_union<-hex_grid %>% # Union based on two grouping variables
  group_by(loc, affected) %>%
  summarise(geometry = st_union(x), .groups = "drop")

# st_write(hex_union,"hex_union.gpkg")


# Downloading cell data ---------------------------------------------------

## Going to want higher res cell data home locations

api_key<-read.csv(file = "APIkey.csv", header = FALSE) 
api_key<-api_key$V1
headers<-add_headers(Authorization = api_key, `Content-Type` = "application/json") # Set up the headers including the API key
url<-"https://api.gravyanalytics.com/v1.1/areas/tradeareas" # API URL to query
if (!dir.exists("tData")) { # Create cell data directory if it doesn't exist
  dir.create("tData", recursive = TRUE)
}
df<-hex_union %>% filter(affected == "affected")
df$id<-seq(1,nrow(df),1)
df<-st_transform(df, crs = 4326) # Needs to be projected in 4326 to work with lat long conventions of the API

batchapi<-function(dft,s,e,fname){ # Function converts sf object to json, passes to api, gets returned data, and merges back with sf object
  
  dft$startDateTimeEpochMS<-s # 1704067200000 These don't seem to work as query variables
  dft$endDateTimeEpochMS<-e # 1706831999000 Says endDateTimeEpochMS must be within 90 days from startDateTimeEpochMS disregarding time of day, but this doesn't seem true. Any date is possible.
  dft$excludeFlags<-25216 # Corresponds to removing
  # dft$startDateTimeEpochMS<-1656633600000
  # dft$endDateTimeEpochMS<-1659311999999
  #dft<-dft %>% select(-PUD_YR_AVG) # Need more than the geometry column to create a feature collection using sf_geojson. Also, there is a limit of 20 features per request (even if it doesn't return results for 20 features).
  dftj<-sf_geojson(dft,atomise = FALSE) # Convert sf object to GeoJSON 
  
  dftj<-fromJSON(dftj) # Doesn't seem to like geojson formatting, switching to json
  dftj<-toJSON(dftj, auto_unbox = TRUE)
  
  # Export query (asynchronous)
  system.time(response<-POST(url, headers, body = dftj, encode = "json", query = list(
    # includeHeaders = FALSE, # Remove headers - potentially useful for batching
    # returnDeviceCountByGeoHash = TRUE, # "If true, the geoHashDeviceCount and geoHashWidthHeights fields are populated per feature" - don't see this. It does return "searchobjectid" in the response psv that corresponds to a given "id" in the json properties
    #decisionLocationTypes = list(c("LATLNG","CBG")),
    decisionLocationTypes = "CBG",
    includeAdditionalCbgInfo = TRUE,
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
  
  write_parquet(xp, paste0("tData/",fname,".parquet"))
}

batchapi(df, s = 1656633600000, e = 1659311999999, fname = "July2022") # July 2022
batchapi(df, s = 1688169600000, e = 1690847999999, fname = "July2023") # July 2023
batchapi(df, s = 1720003200000, e = 1722681599999, fname = "July2024") # July 2024

dfs<-map_df(list.files("tData/", pattern = "\\.parquet$", full.names = TRUE), read_parquet)



