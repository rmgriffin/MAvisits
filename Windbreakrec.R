# Intro -------------------------------------------------------------------
rm(list=ls()) # Clears workspace

# install.packages("renv") # Install/call libraries
# renv::init()

PKG <- c("googledrive","sf","tidyverse","httpuv")

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

st_write(hex_grid,"hex_grid.gpkg")
