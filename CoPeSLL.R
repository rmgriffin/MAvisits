## Scripts in support of NSF CoPe application and SLL project

# Setup -------------------------------------------------------------------
rm(list=ls()) # Clears workspace

#system("sudo apt install make g++ gfortran libgeos-dev libproj-dev libgdal-dev libudunits2-dev libcurl4-openssl-dev -y") # Install linux geospatial dependencies 

# Install/call libraries
install.packages("renv")
#renv::init()

PKG <- c("googledrive","tidyr","purrr", "sf", "tmap", "raster", "patchwork",
         "rgdal", "exactextractr","ggplot2","gargle", "s2","dplyr","RColorBrewer")

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p)
    require(p,character.only = TRUE)}
}

renv::snapshot()
rm(p,PKG)

# Data download
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

## ESI
ESI<-st_read("Data/ESILRIMA.gpkg")
ESI<-ESI[,c("ESI","geom")]
ESI$length<-st_length(ESI)
ESI<-separate(ESI,ESI,c("I1","I2","I3"),"/",convert = TRUE)
ESI$x1a<-ifelse(ESI$I1 == "1A",ESI$length, ifelse(ESI$I2 == "1A",ESI$length, ifelse(ESI$I3 == "1A",ESI$length,0))) # Create variables for all classes and populate them with relevant length
ESI$x1b<-ifelse(ESI$I1 == "1B",ESI$length, ifelse(ESI$I2 == "1B",ESI$length, ifelse(ESI$I3 == "1B",ESI$length,0)))
ESI$x2a<-ifelse(ESI$I1 == "2A",ESI$length, ifelse(ESI$I2 == "2A",ESI$length, ifelse(ESI$I3 == "2A",ESI$length,0)))
ESI$x3a<-ifelse(ESI$I1 == "3A",ESI$length, ifelse(ESI$I2 == "3A",ESI$length, ifelse(ESI$I3 == "3A",ESI$length,0)))
ESI$x3b<-ifelse(ESI$I1 == "3B",ESI$length, ifelse(ESI$I2 == "3B",ESI$length, ifelse(ESI$I3 == "3B",ESI$length,0)))
ESI$x4<-ifelse(ESI$I1 == "4",ESI$length, ifelse(ESI$I2 == "4", 1,ifelse(ESI$I3 == "4",ESI$length,0)))
ESI$x5<-ifelse(ESI$I1 == "5",ESI$length, ifelse(ESI$I2 == "5", 1,ifelse(ESI$I3 == "5",ESI$length,0)))
ESI$x6a<-ifelse(ESI$I1 == "6A",ESI$length, ifelse(ESI$I2 == "6A",ESI$length, ifelse(ESI$I3 == "6A",ESI$length,0)))
ESI$x7<-ifelse(ESI$I1 == "7",ESI$length, ifelse(ESI$I2 == "7", 1,ifelse(ESI$I3 == "7",ESI$length,0)))
ESI$x6b<-ifelse(ESI$I1 == "6B",ESI$length, ifelse(ESI$I2 == "6B",ESI$length, ifelse(ESI$I3 == "6B",ESI$length,0)))
ESI$x8a<-ifelse(ESI$I1 == "8A",ESI$length, ifelse(ESI$I2 == "8A",ESI$length, ifelse(ESI$I3 == "8A",ESI$length,0)))
ESI$x8b<-ifelse(ESI$I1 == "8B",ESI$length, ifelse(ESI$I2 == "8B",ESI$length, ifelse(ESI$I3 == "8B",ESI$length,0)))
ESI$x8c<-ifelse(ESI$I1 == "8C",ESI$length, ifelse(ESI$I2 == "8C",ESI$length, ifelse(ESI$I3 == "8C",ESI$length,0)))
ESI$x9a<-ifelse(ESI$I1 == "9A",ESI$length, ifelse(ESI$I2 == "9A",ESI$length, ifelse(ESI$I3 == "9A",ESI$length,0)))
ESI$x9b<-ifelse(ESI$I1 == "9B",ESI$length, ifelse(ESI$I2 == "9B",ESI$length, ifelse(ESI$I3 == "9B",ESI$length,0)))
ESI$x10a<-ifelse(ESI$I1 == "10A",ESI$length, ifelse(ESI$I2 == "10A",ESI$length, ifelse(ESI$I3 == "10A",ESI$length,0)))
ESI$xU<-ifelse(ESI$I1 == "U",ESI$length, ifelse(ESI$I2 == "U",ESI$length, ifelse(ESI$I3 == "U",ESI$length,0)))
ESI[is.na(ESI)]<-0 # Replace NAs with zeros

ESI$RockyShore<-ESI$x1a + ESI$x2a + ESI$x6a +ESI$x8a
ESI$SandyBeach<-ESI$x3a + ESI$x3b + ESI$x4 + ESI$x5
ESI$Marshes<-ESI$x9b + ESI$x10a
ESI$Armored<-ESI$x1b + ESI$x6b + ESI$x8b + ESI$x8c
ESI$TidalFlat<-ESI$x7 + ESI$x9a

ESI<-ESI[,c("RockyShore","SandyBeach","Marshes","Armored","TidalFlat","geom")]

muni<-st_read("Data/MAmunicipalities.gpkg")
muni<-st_transform(muni, st_crs(ESI))
muni<-muni[,c("TOWN","geom")]
muni<-slice(muni,-c(17,30,38,111,191)) # "Evaluation error: Found 5 features with invalid spherical geometry."
#muni<-muni %>% dplyr::filter(TOWN %in% c("BOSTON","GLOUCESTER","NEW BEDFORD","NANTUCKET"))

ESI<-st_join(muni,ESI)

ESI$TOWN<-as.factor(ESI$TOWN)

ESIsum<-ESI %>% st_drop_geometry() %>% # If you don't drop geometry, takes forever 
  group_by(TOWN) %>% 
  summarise(sum(RockyShore),sum(SandyBeach),sum(Marshes),sum(Armored),sum(TidalFlat)) # Sum length (meters)

ESIsum$total<-ESIsum$`sum(RockyShore)`+ESIsum$`sum(SandyBeach)`+ESIsum$`sum(Marshes)`+ESIsum$`sum(Armored)`+ESIsum$`sum(TidalFlat)`

ESIp<-ESIsum

ESIp$RockyShore<-round((ESIp$`sum(RockyShore)`/ESIp$total)*100,1)
ESIp$SandyBeach<-round((ESIp$`sum(SandyBeach)`/ESIp$total)*100,1)
ESIp$Marshes<-round((ESIp$`sum(Marshes)`/ESIp$total)*100,1)
ESIp$Armored<-round((ESIp$`sum(Armored)`/ESIp$total)*100,1)
ESIp$TidalFlat<-round((ESIp$`sum(TidalFlat)`/ESIp$total)*100,1)

ESIp$totalkm<-round(ESIp$total/1000,1) # Total length kilometers

#ESIp$geom<-NULL
ESIp<-as.data.frame(ESIp)

write.csv(ESIp,"ESIp.csv")

## Boston example
bos<-muni %>% filter(TOWN=="BOSTON")
dc<-st_read("Data/DC2019ESILRIMA0_002DD.gpkg")
ESI2<-st_read("Data/ESILRIMA.gpkg")
ESI2<-separate(ESI2,ESI,c("I1","I2","I3"),"/",convert = TRUE)
ESI2$I1<-ifelse(ESI2$I1 %in% c("1A","2A","2B","6A","8A","8D"),"Rocky Shore",
                ifelse(ESI2$I1 %in% c("3A","3B","4","5"),"Sandy Beach",
                       ifelse(ESI2$I1 %in% c("9B","10A","10B","10C","10D"),"Marsh",
                              ifelse(ESI2$I1 %in% c("1B","6B","8B","8C"),"Armored",
                                     ifelse(ESI2$I1 %in% c("7","9A"),"Tidal Flat",NA)))))
ESI2$I2<-ifelse(ESI2$I2 %in% c("1A","2A","2B","6A","8A","8D"),"Rocky Shore",
                ifelse(ESI2$I2 %in% c("3A","3B","4","5"),"Sandy Beach",
                       ifelse(ESI2$I2 %in% c("9B","10A","10B","10C","10D"),"Marsh",
                              ifelse(ESI2$I2 %in% c("1B","6B","8B","8C"),"Armored",
                                     ifelse(ESI2$I2 %in% c("7","9A"),"Tidal Flat",NA)))))
ESI2$I3<-ifelse(ESI2$I3 %in% c("1A","2A","2B","6A","8A","8D"),"Rocky Shore",
                ifelse(ESI2$I3 %in% c("3A","3B","4","5"),"Sandy Beach",
                       ifelse(ESI2$I3 %in% c("9B","10A","10B","10C","10D"),"Marsh",
                              ifelse(ESI2$I3 %in% c("1B","6B","8B","8C"),"Armored",
                                     ifelse(ESI2$I3 %in% c("7","9A"),"Tidal Flat",NA)))))


ESI2<-st_transform(ESI2,st_crs(muni))
dc<-st_transform(dc,st_crs(muni))

bos_c<-st_bbox(bos)
bos_c_sf<-st_as_sfc(bos_c) # Bounding box to sf object

dc2<-st_intersection(dc,bos_c_sf) # Extracting only cell data in the plot domain for quantile id

# ESI shoreline types
cols1 <- c("Armored" = "black", "Rocky Shore" = "gray", "Marsh" = "darkgreen", "Sandy Beach" = "tan", "Tidal Flat" = "lightblue") # Consistent colors across I1, I2, I3
a<-ggplot() +
  #geom_sf(data = muni, color = "grey",fill = "white") +
  geom_sf(data = ESI2, aes(color = I1), linewidth=1) +
  scale_color_manual(values = cols1) +
  geom_sf(data = ESI2 %>% drop_na(I2), aes(color = I2), linewidth=1, linetype = "dotted") +
  scale_color_manual(values = cols1, name = "Shoreline Type") +
  geom_sf_label(data = muni, aes(label = TOWN)) +
  coord_sf(xlim = c(bos_c[1],bos_c[3]), ylim = c(bos_c[2],bos_c[4])) + 
  theme_minimal()

# Cell phone density
getPalette<-colorRampPalette(brewer.pal(9, "Blues")) # function, change if different palette is desired
cols<-getPalette(5) 
scales::show_col(cols)
#labs<-expression(paste("10"^"9"),paste("10"^"8"),paste("10"^"7"),paste("-10"^"7"),paste("-10"^"8"),paste("-10"^"9"),paste("-10"^"10"))
colscale<-unname(quantile(dc2$DC_YR, probs = seq(0, 1, 1/5)))
labs<-paste(paste(seq(0, 100, 20),"%",sep=""), paste(seq(20, 120, 20),"%",sep=""), sep=" - ")

dc$DC_YR_f<-cut(dc$DC_YR,breaks = colscale)

#cols[4]<-cols[1]<-"#FFFFFF" # Manually adding another bin for labels at the ends

b<-ggplot() +
  #geom_sf(data = muni, color = "grey",fill = "white") +
  # scale_color_manual(values = cols) +
  # geom_sf(data = ESI2 %>% drop_na(I2), aes(color = I2), linewidth=1, linetype = "dotted") +
  # scale_color_manual(values = cols, name = "Shoreline Type") +
  geom_sf(data = dc %>% drop_na(), aes(color=DC_YR_f), alpha = 0.5) +
  scale_color_manual(values = cols, drop = TRUE, labels = labs, name = "Quintiles") +
  geom_sf(data = ESI2, linewidth=1) +
  geom_sf_label(data = muni, aes(label = TOWN)) +
  coord_sf(xlim = c(bos_c[1],bos_c[3]), ylim = c(bos_c[2],bos_c[4])) + 
  theme_minimal()

a + b + plot_layout(ncol = 1)

dir.create(file.path('Figures'), recursive = TRUE)

ggsave("./Figures/Fig1.png", device = "png", width = 8.5, height = 11, units = "in", dpi = 300)

## Proportions of shoreline type and cellphone data density (using data summarized by MA towns from above)
# Shoreline type
ESIp %>% summarise("Rocky Shore (km)" = sum(`sum(RockyShore)`/1000, na.rm = TRUE),
                   "Sandy Beach (km)" = sum(`sum(SandyBeach)`/1000, na.rm = TRUE),
                   "Marshes (km)" = sum(`sum(Marshes)`/1000, na.rm = TRUE),
                   "Armored (km)" = sum(`sum(Armored)`/1000, na.rm = TRUE),
                   "Tidal Flat (km)" = sum(`sum(TidalFlat)`/1000, na.rm = TRUE)) %>% 
  mutate("Total Length (km)" = `Rocky Shore (km)` + `Sandy Beach (km)` + `Marshes (km)` + `Armored (km)` + `Tidal Flat (km)`) %>% 
  mutate("Rocky Shore (%)" = round(`Rocky Shore (km)`/`Total Length (km)`,3)*100,
         "Sandy Beach (%)" = round(`Sandy Beach (km)`/`Total Length (km)`,3)*100,
         "Marshes (%)" = round(`Marshes (km)`/`Total Length (km)`,3)*100,
         "Armored (%)" = round(`Armored (km)`/`Total Length (km)`,3)*100,
         "Tidal Flat (%)" = round(`Tidal Flat (km)`/`Total Length (km)`,3)*100)
# Cellphone data density
