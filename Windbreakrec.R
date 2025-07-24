# Intro -------------------------------------------------------------------
rm(list=ls()) # Clears workspace

# install.packages("renv") # Install/call libraries
# renv::init()

PKG<-c("googledrive","sf","tidyverse","httpuv","R.utils","httr","jsonlite","geojsonsf","lwgeom","furrr","arrow","stringr","sandwich","lmtest","fixest","digest","geosphere","broom","fwildclusterboot")

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

rm(r2_labels)

# Visitation model data preparation --------------------------------------------------------
dfs$EARLIEST_OBSERVATION_OF_DAY<-with_tz(as.POSIXct(dfs$EARLIEST_OBSERVATION_OF_DAY, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"), tzone = "America/New_York") # Set format to POSIXct in native UTC time zone, and convert to eastern time
dfs$LATEST_OBSERVATION_OF_DAY<-with_tz(as.POSIXct(dfs$LATEST_OBSERVATION_OF_DAY, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"), tzone = "America/New_York")

dfs$year<-year(dfs$EARLIEST_OBSERVATION_OF_DAY)
dfs$instant<-ifelse(dfs$EARLIEST_OBSERVATION_OF_DAY == dfs$LATEST_OBSERVATION_OF_DAY,1,0) # Some observations have duration of stay of zero
table(dfs$year, dfs$instant, dfs$id) # How many observations per year, how many appear to be instantaneous, by island?
dfs<-dfs %>% filter(instant == 0) # Dropping observations with no stay duration

dfs$duration_min<-as.numeric(difftime(dfs$LATEST_OBSERVATION_OF_DAY,dfs$EARLIEST_OBSERVATION_OF_DAY,units = "secs"))/60
dfs<-dfs %>% filter(duration_min>5) # 25% of observations are less than 5 minutes observed in the area, dropping those
#dfs$vd<-as.numeric(dfs$TOTAL_POPULATION)/as.numeric(dfs$DEVICES_WITH_DECISION_IN_CBG_COUNT) # Population normalized visits
dfs$vd<-1 # Each device as a visitor day

# dfs %>% # Multiple decision locations for devices?
#   group_by(DEVICEID) %>% # group_by(DEVICEID,year)
#   summarize(distinct_home_census_blocks = n_distinct(CENSUS_BLOCK_GROUP_ID), .groups = "drop") %>%
#   count(distinct_home_census_blocks)

df<-dfs %>% 
  group_by(DAY_IN_FEATURE,id,Name,City,State) %>% 
  summarise(visits = sum(vd,na.rm = TRUE),
            visitorhours = sum(duration_min,na.rm = TRUE)/60) %>% 
  mutate(year = as.factor(year(DAY_IN_FEATURE)), dayofmonth = format(as.Date(DAY_IN_FEATURE),"%m-%d")) 

# Filling in zeros for dates without any visits
df<-df %>%
  mutate(DAY_IN_FEATURE = as.Date(DAY_IN_FEATURE))

valid_dates <- c(
  seq(as.Date("2023-06-15"), as.Date("2023-08-15"), by = "day"),
  seq(as.Date("2024-06-15"), as.Date("2024-08-15"), by = "day")
) %>% tibble(DAY_IN_FEATURE = .)

meta <- df %>% # Extract unique id + metadata
  ungroup() %>% 
  select(id, Name, City, State) %>% 
  distinct()

full_grid <- crossing(meta, valid_dates)

df <- full_grid %>%
  left_join(df, by = c("id", "Name", "City", "State", "DAY_IN_FEATURE")) %>%
  mutate(visits = replace_na(visits, 0),
         visitorhours = replace_na(visitorhours,0),
         year = year(DAY_IN_FEATURE),
         dayofmonth = format(DAY_IN_FEATURE, "%m-%d"),
         DAY_IN_FEATURE = as.character(DAY_IN_FEATURE),
         year = as.factor(year(DAY_IN_FEATURE)))

# ggplot(df, aes(x = dayofmonth, y = visits, color = year, group = year)) + # Cell visits by day and year for each id
#   geom_line() +
#   labs(x = "Day", y = "Visits", color = "Year") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
#   geom_vline(xintercept = "07-16", linetype = "dashed", color = "black") +
#   theme_minimal() +
#   scale_x_discrete(breaks = unique(df$dayofmonth)[seq(1, length(unique(df$dayofmonth)), by = 14)]) +
#   facet_wrap(~id)

df %>% group_by(year, id) %>% # Scatterplot of visitor hours per beach, 2023 vs 2024
  summarize(visitdurationhrs = sum(visitorhours, na.rm = TRUE) / sum(visits, na.rm = TRUE), .groups = 'drop') %>%
  drop_na(visitdurationhrs) %>%
  pivot_wider(
    names_from = year,
    values_from = visitdurationhrs,
    names_prefix = "visit_dur_"
  ) %>% drop_na(visit_dur_2023, visit_dur_2024) %>%
  {
    mod <- lm(visit_dur_2024 ~ visit_dur_2023, data = .)
    r2 <- summary(mod)$r.squared
    label <- paste0("R² = ", round(r2, 3))
    
    ggplot(., aes(x = visit_dur_2023, y = visit_dur_2024)) +
      geom_point(alpha = 0.7, size = 3) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      annotate("text", x = Inf, y = -Inf, hjust = 1.1, vjust = -1.1,
               label = label, size = 4) +
      labs(
        title = "Visit Duration Hours Per Visit: 2023 vs. 2024",
        x = "Visit Duration Hours Per Visit (2023)",
        y = "Visit Duration Hours Per Visit (2024)"
      ) +
      theme_minimal() +
      coord_fixed()
  }

# df %>% # Total cell visits and visitor hours by beach id
#   #filter(City == "Nantucket") %>%
#   filter(year == 2024) %>%
#   group_by(id,Name,State,City,year) %>%
#   summarise(sumvisits = sum(visits), sumvisitorhours = sum(visitorhours)) %>%
#   arrange(desc(sumvisits)) %>%
#   print(n = nrow(.))

nv<-62 # Threshold under which to exclude beaches
`%ni%`<- Negate(`%in%`)

low_visit_ids<-df %>%
  filter(year == 2023) %>%
  group_by(id) %>%
  summarise(sumvisits_2023 = sum(visits)) %>%
  filter(sumvisits_2023 < nv) %>%
  pull(id)

cvf<-df %>% # Conversion factor from sampled visits to total cell visits for Nantucket
  filter(City == "Nantucket") %>% filter(year == 2024) %>% summarise(sumvisits = sum(visits)) %>% as.numeric() /
  df %>% filter(City == "Nantucket" & id %ni% low_visit_ids) %>% filter(year == 2024) %>% summarise(sumvisits = sum(visits)) %>% as.numeric()

cvhf<-df %>% # Conversion factor from sampled visits to total cell visithours for Nantucket 
  filter(City == "Nantucket") %>% filter(year == 2024) %>% summarise(sumvh = sum(visitorhours)) %>% as.numeric() /
  df %>% filter(City == "Nantucket" & id %ni% low_visit_ids) %>% filter(year == 2024) %>% summarise(sumvh = sum(visitorhours)) %>% as.numeric()

df<-df %>% # Filtering out low visit beaches 
  filter(id %ni% low_visit_ids)

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
                   station = case_when(State == "RI" ~ "NEWPORT STATE AIRPORT, RI US (USW00014787)",
                                       State == "MA" & City != "Westport" ~ "NANTUCKET MEMORIAL AIRPORT, MA US (USW00014756)",
                                       State == "MA" & City == "Westport" ~ "NEWPORT STATE AIRPORT, RI US (USW00014787)",
                                       TRUE ~ NA)) %>% 
  ungroup() %>% # Ungroup is needed because DAY_IN_FEATURE was previously a grouping variable
  select(!DAY_IN_FEATURE) %>%
  left_join(wth, by = c("date","station"))

al_treat_groups<-al %>% filter(id %in% unique(df$id) & City != "Nantucket") # Only visited and non-treated beaches

dist_matrix<-matrix(as.numeric(st_distance(st_transform(al_treat_groups, 26919))), nrow = nrow(al_treat_groups))

al_treat_groups<-al_treat_groups %>% mutate(group = map(seq_len(n()), ~ al_treat_groups$id[which(dist_matrix[.x, ] <= 23000)])) # Identifying groups of beaches within 23km of each other

al_treat_groups %>% 
  mutate(group_str = map_chr(seq_along(group), ~ paste0(sort(group[[.x]]), collapse = "-"))) %>% 
  count(group_str, name = "group_frequency") %>% 
  arrange(desc(group_frequency)) %>% 
  st_drop_geometry()

rm(wth,full_grid,valid_dates,meta)


# Visitation model --------------------------------------------------------
DiD_ri <- function(df, # Input dataframe
                   al_treat_groups, # Potential groups of affected beaches for randomization inference
                   mode = c("window", "daily"), # Estimate treatment effect for a window of time, or a series of days
                   dates, # Dates for "daily" (seq(as.Date("2024-07-15"), as.Date("2024-07-31"), by = "day")) or "window" (as.Date(c("2024-07-16", "2024-07-27")))
                   model_formula, # Formula for model written as FEOLS model 
                   perm_unit = "spatial_cluster", # Choice of grouping for randomization permutation. Options {"spatial_cluster", "City", "id"}
                   n_perm = 1000, # Number of permutations for randomization inference
                   treated_city = "Nantucket", # City name for treated beaches
                   return_plot = FALSE, # Return plot of permutation distribution and treatment effect
                   seed = 123) { # Seed for randomization
  
  mode <- match.arg(mode)
  set.seed(seed)
  
  results <- list()
  all_permutations <- list()
  
  if (mode == "window") {
    date_list <- list(dates)
    label_list <- paste0(min(dates), "_to_", max(dates))
  } else {
    date_list <- as.list(dates)
    label_list <- as.character(dates)
  }
  
  for (i in seq_along(date_list)) {
    current_date <- date_list[[i]]
    date_label <- label_list[i]
    
    dfavg <- df %>%
      filter(date >= as.Date("2024-06-15") & date < as.Date("2024-08-15")) %>% # Avoids contamination that began in LC and Westport starting 8/1
      mutate(
        post = if (mode == "window") {
          date >= current_date[1] & date <= current_date[length(current_date)]
        } else {
          date == current_date
        },
        treated = City == treated_city,
        treat_post = post * treated,
        temp_bin = factor(cut(tempmaxF, breaks = c(60, 70, 80, 90, 100), right = TRUE)),
        day_of_week = factor(weekdays(date)),
        id = factor(id),
        dayofmonth = factor(dayofmonth)
      )
    
    model <- feols(model_formula, data = dfavg) # Fit original model
    # print(bt_result<-boottest(model, param = "treat_post", clustid = "City", B = 9999,type = "webb")) Wild clustered standard errors for treatment effect
    point_estimate <- coef(model)["treat_post"]
    
    if (perm_unit == "spatial_cluster") { # Identify permutation units
      valid_placebo_clusters <- al_treat_groups %>%
        mutate(group_str = map_chr(group, ~ paste(sort(.x), collapse = "-"))) %>%
        distinct(group_str, group)
      n_treated <- 1
    } else {
      if (!perm_unit %in% c("id", "City")) {
        stop(glue::glue("perm_unit '{perm_unit}' is not an acceptable unit of permutation"))
      }
      n_treated <- dfavg %>% filter(treated) %>% distinct(.data[[perm_unit]]) %>% nrow()
      controls <- dfavg %>% filter(!treated) %>% distinct(.data[[perm_unit]]) %>% pull()
    }
    
    # Prepare model formula strings
    model_str <- paste(deparse(model_formula), collapse = "")
    parts <- strsplit(model_str, "\\|", fixed = TRUE)[[1]]
    rhs_formula <- trimws(parts[1])  # "visitorhours ~ treat_post + ..."
    fe_formula <- if (length(parts) > 1) trimws(parts[2]) else NULL
    
    rhs_placebo <- gsub("\\btreat_post\\b", "placebo_treat_post", rhs_formula)
    placebo_str <- if (!is.null(fe_formula)) {
      paste(rhs_placebo, "|", fe_formula)
    } else {
      rhs_placebo
    }
    
    perm_estimates <- numeric(n_perm)
    for (j in 1:n_perm) { # Permutation loop
      dfavg_perm <- if (perm_unit == "spatial_cluster") {
        placebo_cluster_ids <- sample(valid_placebo_clusters$group, size = 1)[[1]]
        dfavg %>%
          mutate(
            placebo_treated = id %in% placebo_cluster_ids,
            placebo_treat_post = placebo_treated * post
          )
      } else {
        placebo <- sample(controls, size = n_treated, replace = FALSE)
        dfavg %>%
          mutate(
            placebo_treated = .data[[perm_unit]] %in% placebo,
            placebo_treat_post = placebo_treated * post
          )
      }
      
      mod_perm <- feols(as.formula(placebo_str), data = dfavg_perm)
      perm_estimates[j] <- tryCatch(coef(mod_perm)["placebo_treat_post"], error = function(e) NA)
    }
    
    p_val <- mean(abs(perm_estimates) >= abs(point_estimate), na.rm = TRUE)
    
    results[[i]] <- tibble(
      label = date_label,
      estimate = point_estimate,
      p_val = p_val
    )
    
    perm_df <- tibble(label = date_label, estimate = perm_estimates)
    all_permutations[[i]] <- perm_df
  }
  
  out <- list(
    summary = bind_rows(results),
    permutations = bind_rows(all_permutations)
  )
  
  if (return_plot && mode == "window") {
    # Single plot for one window
    p <- out$summary$p_val[1]
    out$plot <- ggplot(out$permutations, aes(x = estimate)) +
      geom_histogram(fill = "gray80", color = "black", bins = 30) +
      geom_vline(xintercept = out$summary$estimate[1], color = "red", size = 1.2, linetype = "dashed") +
      labs(
        title = "Permutation Distribution of Placebo Treatment Effects",
        subtitle = paste("True effect shown in red | p-value =", formatC(p, digits = 2, format = "f")),
        x = "Estimated Effect",
        y = "Frequency"
      ) +
      theme_minimal(base_size = 14)
    
  } else if (return_plot && mode == "daily") {

    plot_df <- out$permutations %>%
      left_join(out$summary %>% rename(true_effect = estimate), by = "label") %>%
      mutate(label_with_p = paste0(label, " (p = ", formatC(p_val, digits = 2, format = "f"), ")"))
    
    out$facet_plot <- ggplot(plot_df, aes(x = estimate)) +
      geom_histogram(fill = "gray80", color = "black", bins = 30) +
      geom_vline(aes(xintercept = true_effect),
                 color = "red", linetype = "dashed", size = 1.2) +
      facet_wrap(~ label_with_p, scales = "free") +
      labs(
        title = "Permutation Distribution of Placebo Treatment Effects by Day",
        x = "Estimated Effect",
        y = "Frequency"
      ) +
      theme_minimal(base_size = 14)
  }
  
  return(out)
}

# Might flag beach closures on 8/3 that seem to have a negative impact on visitation

DiD_ri(df = df, al_treat_groups = al_treat_groups,
       mode = "daily", dates = seq(as.Date("2024-07-09"), as.Date("2024-08-05"), by = "day"),
       model_formula = visitorhours ~ treat_post | id + date,
       perm_unit = "spatial_cluster", n_perm = 100, treated_city = "Nantucket", seed = 123, return_plot = TRUE)

DiD_ri(df = df, al_treat_groups = al_treat_groups,
       mode = "daily", dates = seq(as.Date("2024-07-09"), as.Date("2024-08-05"), by = "day"),
       model_formula = visits ~ treat_post | id + date,
       perm_unit = "spatial_cluster", n_perm = 100, treated_city = "Nantucket", seed = 123, return_plot = TRUE)

out<-DiD_ri(df = df, al_treat_groups = al_treat_groups,
       mode = "window", dates = as.Date(c("2024-07-16", "2024-07-17")),
       model_formula = visits ~ treat_post | id + date,
       perm_unit = "spatial_cluster", n_perm = 100, treated_city = "Nantucket", seed = 123, return_plot = TRUE)


total_effect<-round(as.numeric(out$summary$estimate)*2*length(unique(df %>% filter(City == "Nantucket") %>% pull(id)))*cf*cvf,0)

# # # Average impact 7-16 to 7-31 (normalized cell visit counts, by annual mean)
# dfavgr <- df %>%
#   filter(date >= as.Date("2023-06-15") & date < as.Date("2024-07-23")) %>% # Avoids contamination that began in LC and Westport starting 8/1
#   mutate(
#     year = as.character(year),  # Temporarily convert to character for group_by
#     date = as.Date(date),
#     post = date >= as.Date("2024-07-16"),
#     treated = City == "Nantucket",
#     treat_post = post * treated,
#     temp_bin = factor(cut(tempmaxF, breaks = c(60, 70, 80, 90, 100), right = TRUE)),
#     day_of_week = weekdays(date)
#   ) %>%
#   group_by(id, year) %>%
#   mutate(
#     baseline_mean = mean(visits[date < as.Date("2024-07-16")], na.rm = TRUE),
#     visits_rel = visits / baseline_mean
#   ) %>%
#   ungroup()
# 
# dfavgr <- dfavgr %>%
#   mutate(
#     id = factor(id),
#     year = factor(year),
#     dayofmonth = factor(dayofmonth),
#     day_of_week = factor(day_of_week)
#   ) %>%
#   filter(is.finite(visits_rel))  # Drop if baseline_mean was 0 or NA
# 
# model <- feols(
#   visits_rel ~ treat_post + temp_bin + precIn | id + dayofmonth + day_of_week + year,
#   data = dfavgr,
#   vcov = cluster ~ id
# )
# 
# summary(model)
# 
# print(bt_result<-boottest(model, param = "treat_post", clustid = "City", B = 9999,type = "webb"))

# Event study with discrete bins of days for pre-post 
nbins<-1 # Number of days binned together
df_esn <- df %>%
  filter(date >= as.Date("2024-07-01") & date < as.Date("2024-08-01")) %>% 
  mutate(
    rel_day = as.integer(date - as.Date("2024-07-16")),
    event_bin = floor(rel_day / nbins),  # numeric for clean ordering
    treated = City == "Nantucket",
    temp_bin = factor(cut(tempmaxF, breaks = c(60, 70, 80, 90, 100), right = TRUE)),
    day_of_week = weekdays(date)
  )

model_esn <- feols(
  visits ~ i(event_bin, treated, ref = -1) + temp_bin + precIn | id + dayofmonth + day_of_week,
  data = df_esn,
  cluster = ~City
)

tidy_esn <- tibble::tibble(
  term = names(coef(model_esn)),
  estimate = unname(coef(model_esn)),
  conf.low = confint(model_esn)[, 1],
  conf.high = confint(model_esn)[, 2]
) %>%
  filter(grepl("^event_bin::[-0-9]+:treated$", term)) %>%
  mutate(
    bin = as.numeric(gsub("^event_bin::([-0-9]+):treated$", "\\1", term))
  ) %>%
  arrange(bin)

ggplot(tidy_esn, aes(x = bin, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    x = paste0(nbins,"-day bins (relative to event)"),
    y = "Effect on visits",
    title = paste0("Event Study:",nbins,"-Day Bins (Nantucket Treatment)")
  ) +
  theme_minimal()

# Event study with all pre-event days binned together, and post binned flexibly into n days
nbins <- 3  # Number of days binned together

df_esn <- df %>%
  filter(date >= as.Date("2024-07-01") & date < as.Date("2024-08-01")) %>% 
  mutate(
    rel_day = as.integer(date - as.Date("2024-07-16")),
    event_bin = case_when(
      rel_day < 0 ~ "pre",
      TRUE ~ as.character(floor(rel_day / nbins))
    ),
    treated = City == "Nantucket",
    temp_bin = factor(cut(tempmaxF, breaks = c(60, 70, 80, 90, 100), right = TRUE)),
    day_of_week = weekdays(date)
  )

df_esn <- df_esn %>%
  mutate(
    id = factor(id),
    dayofmonth = factor(dayofmonth),
    day_of_week = factor(day_of_week)
  )

model_esn <- feols(
  visits ~ i(event_bin, treated, ref = "pre") + temp_bin + precIn | id + dayofmonth + day_of_week,
  data = df_esn,
  cluster = ~id
)

print(bt_result<-boottest(model_esn, param = "event_bin::2:treated", clustid = "id", B = 9999,type = "webb"))

tidy_esn <- tibble::tibble(
  term = names(coef(model_esn)),
  estimate = unname(coef(model_esn)),
  conf.low = confint(model_esn)[, 1],
  conf.high = confint(model_esn)[, 2]
) %>%
  filter(grepl("^event_bin::.*:treated$", term)) %>%
  mutate(
    bin = as.numeric(gsub("^event_bin::(.*):treated$", "\\1", term))
  ) %>%
  arrange(bin)

ggplot(tidy_esn, aes(x = bin, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    x = paste0(nbins, "-day bins (relative to event)"),
    y = "Effect on visits",
    title = paste0("Event Study: ", nbins, "-Day Bins (Nantucket Treatment)")
  ) +
  theme_minimal()


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


# Displacement

# dfs<-dfs %>% # Old code, needs updating
#   left_join(
#     dfs %>%
#       filter(is.na(FEATUREID)) %>% 
#       distinct(DEVICEID, source) %>%
#       group_by(DEVICEID) %>%
#       summarise(groups_seen_in = paste(sort(unique(source)), collapse = ", "), .groups = "drop"),
#     by = "DEVICEID"
#   )
# 
# dfs %>% filter(is.na(FEATUREID)) %>% count(groups_seen_in) # Summary of overlap in devices across areas

