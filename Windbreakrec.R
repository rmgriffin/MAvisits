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


# Runs code to download and load data from api and google drive -----------
# Uncomment google drive section or api calls in this script if data is not already in your project folders
source("Celldatadl.R")


# Calibration model -------------------------------------------------------
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

df <- df %>%
  mutate(
    source = factor(source),
    dayofmonth = factor(dayofmonth),
    day_of_week = factor(day_of_week)
  )

model<- feols(
  visits ~ treat_post + temp_bin + precIn | source + dayofmonth + day_of_week + year,
  data = df,
  vcov = cluster ~ source
)

summary(model)

print(bt_result<-boottest(model, param = "treat_post", clustid = "source", B = 9999,type = "webb"))

total_effect<-as.numeric(coef(model)["treat_post"] * 16 * cf)


# Robustness tests --------------------------------------------------------
# Pre-trends
pre_df <- df %>%
  filter(
    (format(date, "%m-%d") >= "07-01" & format(date, "%m-%d") <= "07-15" & year %in% c(2023, 2024)) |
      (format(date, "%m-%d") >= "07-16" & format(date, "%m-%d") <= "07-31" & year == 2023)
  ) %>%
  mutate(
    time1 = as.numeric(date - min(date) + 1),  # Start counter at 1
    time2 = as.integer(format(date, "%d"))     # For plotting
  )

pretrend_model <- feols(
  visits ~ time1 + treated:time1 + temp_bin + precIn | day_of_week + dayofmonth + year,
  data = pre_df,
  vcov = "hetero"
)

summary(pretrend_model)

# ggplot(df %>% filter(post == FALSE), aes(x = date, y = visits, color = source)) + # Plot of raw visits
#   geom_line() +
#   facet_wrap(~ year, scales = "free_x") +
#   labs(
#     x = "Date",
#     y = "Visits",
#     color = "Beach",
#     title = "Pre-Treatment trends by location and year"
#   ) +
#   theme_minimal()

pre_df$fitted <- predict(pretrend_model)

ggplot(pre_df %>% filter(source != "Block Island"), aes(x = time2, y = fitted, color = treated)) +
  geom_line(size = 1.2) +
  facet_wrap(~ year, scales = "free_x") +
  labs(x = "Time", y = "Covariate-adjusted visits", color = "Treated") +
  theme_minimal()

# Placebo test
placebo_df <- df %>%
  filter(format(date, "%Y") == "2023" & format(date, "%m") == "07") %>%
  mutate(
    placebo_post = date >= as.Date("2023-07-16"),  # fake treatment date
    placebo_treat_post = placebo_post * treated
  )

placebo_model <- feols(
  visits ~ placebo_treat_post + temp_bin + precIn |
    source + dayofmonth + day_of_week,
  data = placebo_df,
  vcov = "hetero"
)

summary(placebo_model)

# Drop controls
df_wo_LCW <- df %>% filter(source != "Little Compton and Westport")

model_wo_LCW <- feols(
  visits ~ treat_post + temp_bin + precIn |
    source + dayofmonth + day_of_week + year,
  data = df_wo_LCW,
  vcov = "hetero"
)

summary(model_wo_LCW)

df_wo_BI <- df %>% filter(source != "Block Island")

model_wo_BI <- feols(
  visits ~ treat_post + temp_bin + precIn |
    source + dayofmonth + day_of_week + year,
  data = df_wo_BI,
  vcov = "hetero"
)

summary(model_wo_BI)



