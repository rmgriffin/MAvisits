# Intro -------------------------------------------------------------------
rm(list=ls()) # Clears workspace

# install.packages("renv") # Install/call libraries
# renv::init()

remotes::install_github("bcallaway11/did")
library(did) 

PKG<-c("googledrive","sf","tidyverse","httpuv","R.utils","httr","jsonlite","geojsonsf","lwgeom","furrr","arrow","stringr","sandwich","lmtest","fixest","digest","geosphere","broom","fwildclusterboot","gmapsdistance")

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

cfj<-ms %>% filter(month_year == "2024-07") %>% select(ratio_enplaned) %>% as.numeric() # Conversion factor for July 2024
cfa<-ms %>% filter(month_year == "2024-08") %>% select(ratio_enplaned) %>% as.numeric() # Conversion factor for August 2024

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

al_treat_groups<-al %>% filter(id %in% unique(df$id) & City != "Nantucket") # Unique groups of non-treated high-visitation beaches based on 23km clustering of Nantucket pollution

dist_matrix<-matrix(as.numeric(st_distance(st_transform(al_treat_groups, 26919))), nrow = nrow(al_treat_groups))

al_treat_groups<-al_treat_groups %>% mutate(group = map(seq_len(n()), ~ al_treat_groups$id[which(dist_matrix[.x, ] <= 23000)])) # Identifying groups of beaches within 23km of each other

al_treat_groups %>% 
  mutate(group_str = map_chr(seq_along(group), ~ paste0(sort(group[[.x]]), collapse = "-"))) %>% 
  count(group_str, name = "group_frequency") %>% 
  arrange(desc(group_frequency)) %>% 
  st_drop_geometry()

df$hourspervisit<-df$visitorhours/df$visits # Hours per visit

rm(wth,full_grid,valid_dates,meta)


# DiD visitation model, Nantucket main impact --------------------------------------------------------
DiD_ri <- function(df, # Input dataframe
                   al_treat_groups, # Potential groups of affected beaches for randomization inference, only used if perm_unit == "spatial_cluster"
                   mode = c("window", "daily"), # Estimate treatment effect for a window of time, or a series of days
                   dates, # Treatment window dates for "daily" (seq(as.Date("2024-07-15"), as.Date("2024-07-31"), by = "day")) or "window" (as.Date(c("2024-07-16", "2024-07-27")))
                   model_formula, # Formula for model written as FEOLS model 
                   perm_unit = c("spatial_cluster","id"), # Choice of grouping for randomization permutation. Options {"spatial_cluster", "id"}
                   n_perm = 1000, # Number of permutations for randomization inference
                   treated_ids, # Ids for treated beaches
                   return_plot = FALSE, # Return plot of permutation distribution and treatment effect
                   date_range, # Time range over which observations are included in regression, formatted as: as.Date(c("2024-06-15", "2024-08-15"))
                   pretrend_formula = NULL, # Formula for pre-trend model written as FEOLS model (visits ~ time + time:placebo | id). Returns test of parallel pre-trends for period before "dates" using permutation inference (grouping specified via perm_unit)
                   seed = 123) { # Seed for randomization
  
  mode <- match.arg(mode)
  perm_unit <- match.arg(perm_unit)
  set.seed(seed)
  
  if (missing(treated_ids) || is.null(treated_ids) || length(treated_ids) == 0)
    stop("Please pass a non-empty vector 'treated_ids' (treatment is defined only by ids).")
  
  mf_str <- paste(deparse(model_formula), collapse = "")
  
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
      filter(date >= date_range[1] & date <= date_range[2]) %>%
      mutate(
        post = if (mode == "window") date >= current_date[1] & date <= current_date[length(current_date)] else date == current_date,
        id = factor(id),
        treated = as.character(id) %in% as.character(treated_ids),
        treat_post = post * treated,
        temp_bin = factor(cut(tempmaxF, breaks = c(60, 70, 80, 90, 100), right = TRUE)),
        day_of_week = factor(weekdays(date)),
        dayofmonth = factor(dayofmonth)
      )
    
    model <- feols(model_formula, data = dfavg) # Fit original model
    # print(bt_result<-boottest(model, param = "treat_post", clustid = "City", B = 9999,type = "webb")) Wild clustered standard errors for treatment effect
    point_estimate <- tryCatch(stats::coef(model)[["treat_post"]], error = function(e) NA_real_)
    
    if (perm_unit == "spatial_cluster") { # Identify permutation units
      valid_placebo_clusters <- al_treat_groups %>%
        mutate(group_str = map_chr(group, ~ paste(sort(.x), collapse = "-"))) %>%
        distinct(group_str, group)
      n_treated <- 1
    } else {
      n_treated <- dfavg %>% filter(treated) %>% distinct(id) %>% nrow()
      controls <- dfavg %>% filter(!treated) %>% distinct(id) %>% pull() %>% as.character()
    }
    
    # Prepare placebo model formula strings
    parts <- strsplit(mf_str, "\\|", fixed = FALSE)[[1]]
    rhs_formula <- trimws(parts[1])
    fe_formula  <- if (length(parts) > 1) trimws(parts[2]) else NULL
    rhs_placebo <- gsub("\\btreat_post\\b", "placebo_treat_post", rhs_formula)
    placebo_str <- if (!is.null(fe_formula)) paste(rhs_placebo, "|", fe_formula) else rhs_placebo
    perm_estimates <- numeric(n_perm)
    
    for (j in 1:n_perm) { # Permutation loop
      dfavg_perm <- if (perm_unit == "spatial_cluster") {
        placebo_cluster_ids <- sample(valid_placebo_clusters$group, size = 1)[[1]]
        dfavg %>%
          mutate(
            placebo_treated = id %in% placebo_cluster_ids,
            placebo_treat_post = as.integer(post) * as.integer(placebo_treated)
          )
      } else {
        if (length(controls) < n_treated) { perm_estimates[j] <- NA_real_; next }
        placebo_ids <- sample(controls, size = n_treated, replace = FALSE)
        dfavg %>%
          mutate(
            placebo_treated    = as.character(id) %in% as.character(placebo_ids),
            placebo_treat_post = as.integer(post) * as.integer(placebo_treated)
          )
      }
      
      mod_perm <- tryCatch(
        fixest::feols(stats::as.formula(placebo_str), data = dfavg_perm),
        error = function(e) NULL
      )
      perm_estimates[j] <- if (!is.null(mod_perm)) {
        tryCatch(stats::coef(mod_perm)[["placebo_treat_post"]], error = function(e) NA_real_)
      } else NA_real_
    }
    
    p_val <- mean(abs(perm_estimates) >= abs(point_estimate), na.rm = TRUE)
    
    results[[i]] <- tibble(label = date_label, estimate = point_estimate, p_val = p_val)
    
    all_permutations[[i]] <- tibble(label = date_label, estimate = perm_estimates)
  }
  
  out <- list(
    summary = bind_rows(results),
    permutations = bind_rows(all_permutations)
  )
  
  # Optional pre-trend test
  if (!is.null(pretrend_formula) && mode == "window") {
    pre_df <- df %>%
      filter(date < min(dates)) %>%
      mutate(
        time = as.integer(date - min(date)),
        temp_bin = factor(cut(tempmaxF, breaks = c(60, 70, 80, 90, 100), right = TRUE)),
        day_of_week = factor(weekdays(date)),
        id = factor(id),
        dayofmonth = factor(dayofmonth),
        treated = as.character(id) %in% as.character(treated_ids)
      )
    
    pre_model <- feols(pretrend_formula, data = pre_df)
    coef_names <- names(coef(pre_model))
    real_idx <- grep("[:.]treated", coef_names) 
    # real_idx <- grep("^time:treated|^treated:time", coef_names)
    real_slope <- if (length(real_idx) == 1) coef(pre_model)[real_idx] else NA_real_
    
    
    pf_str <- paste(deparse(pretrend_formula), collapse = "") # Build placebo pretrend formula by replacing 'treated' -> 'placebo' in the RHS part only
    pf_parts <- strsplit(pf_str, "\\|")[[1]]
    pf_main  <- trimws(pf_parts[1])  # LHS ~ RHS
    pf_fe    <- if (length(pf_parts) > 1) paste0("|", trimws(pf_parts[2])) else ""
    
    lhs_rhs <- strsplit(pf_main, "~")[[1]] # replace treated -> placebo in RHS of the main part
    lhs_pt  <- lhs_rhs[1]
    rhs_pt  <- gsub("\\btreated\\b", "placebo", lhs_rhs[2])
    placebo_pretrend_str <- paste0(lhs_pt, "~", rhs_pt, if (pf_fe != "") paste0(" ", pf_fe) else "")
    
    n_treated_pre <- pre_df %>% dplyr::filter(treated) %>% dplyr::distinct(id) %>% nrow()

    slope_perm <- replicate(n_perm, {
      pre_df_perm <- if (perm_unit == "spatial_cluster") {
        if (nrow(al_treat_groups) < 1) return(NA_real_)
        placebo_cluster_ids <- sample(al_treat_groups$group, size = 1)[[1]]
        pre_df %>% mutate(placebo = id %in% placebo_cluster_ids)
      } else { # id
        all_ids_pre <- pre_df %>% distinct(id) %>% pull() %>% as.character()
        if (length(all_ids_pre) < n_treated_pre) return(NA_real_)
        placebo_ids <- sample(all_ids_pre, size = n_treated_pre, replace = FALSE)
        pre_df %>% mutate(placebo = as.character(id) %in% placebo_ids)
      }
      
      #placebo_formula <- visits ~ time + time:placebo | id
      
      mod <- tryCatch(fixest::feols(stats::as.formula(placebo_pretrend_str), data = pre_df_perm), error = function(e) NULL)
      if (is.null(mod)) return(NA_real_)
      
      # pick the coefficient that involves ':placebo'
      nm <- names(stats::coef(mod))
      idx <- grep("[:.]placebo", nm)
      if (length(idx) >= 1) as.numeric(stats::coef(mod)[idx[1]]) else NA_real_
    })
    
    out$pretrend_slope <- tibble(
      estimate = real_slope,
      p_val = mean(abs(slope_perm) >= abs(real_slope), na.rm = TRUE)
    )
    
    out$pretrend_plot <- ggplot(data.frame(estimate = slope_perm), aes(x = estimate)) +
      geom_histogram(fill = "gray80", color = "black", bins = 30) +
      geom_vline(xintercept = real_slope, color = "red", linetype = "dashed", linewidth = 1.2) +
      labs(
        title = "Permutation Distribution of Placebo Pretrend Slopes",
        subtitle = paste("Observed slope shown in red | p-value =", formatC(out$pretrend_slope$p_val, digits = 2, format = "f")),
        x = "Estimated Pretrend Slope", y = "Frequency"
      ) +
      theme_minimal(base_size = 14)
  }
  
  
  if (return_plot && mode == "window") {
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

DiD_ri(df = df, al_treat_groups = al_treat_groups, # Note beach closures on 8/3 that seem to have a negative impact on visitation
       mode = "daily", dates = seq(as.Date("2024-07-09"), as.Date("2024-08-05"), by = "day"), date_range = as.Date(c("2024-06-15", "2024-08-15")),
       model_formula = visits ~ treat_post | id + date,
       perm_unit = "spatial_cluster", n_perm = 100, treated_ids = df %>% filter(City == "Nantucket") %>% distinct(id) %>% pull(), seed = 123, return_plot = TRUE)

outv<-DiD_ri(df = df, al_treat_groups = al_treat_groups,
       mode = "window", dates = as.Date(c("2024-07-16", "2024-07-17")), date_range = as.Date(c("2024-06-15", "2024-07-30")),
       model_formula = visits ~ treat_post | id + date,
       perm_unit = "spatial_cluster", n_perm = 500, treated_ids = df %>% filter(City == "Nantucket") %>% distinct(id) %>% pull(), seed = 123, return_plot = TRUE)

total_effectv<-round(as.numeric(outv$summary$estimate)*2*length(unique(df %>% filter(City == "Nantucket") %>% pull(id)))*cfj*cvf,0) # 2 is for two treated days

outvh<-DiD_ri(df = df, al_treat_groups = al_treat_groups,
             mode = "window", dates = as.Date(c("2024-07-16", "2024-07-17")), date_range = as.Date(c("2024-06-15", "2024-07-30")),
             model_formula = visitorhours ~ treat_post | id + date,
             perm_unit = "spatial_cluster", n_perm = 500, treated_ids = df %>% filter(City == "Nantucket") %>% distinct(id) %>% pull(), seed = 123, return_plot = TRUE)

total_effectvh<-round(as.numeric(outvh$summary$estimate)*2*length(unique(df %>% filter(City == "Nantucket") %>% pull(id)))*cfj*cvhf,0) # 2 is for two treated days

# DiD_ri(df = df %>% filter(!is.na(hourspervisit)), al_treat_groups = al_treat_groups, # Hours per visit did not appear to change on the pollution date and shortly thereafter
#        mode = "daily", dates = seq(as.Date("2024-07-09"), as.Date("2024-07-29"), by = "day"), date_range = as.Date(c("2024-06-15", "2024-07-30")),
#        model_formula = hourspervisit ~ treat_post | id + date,
#        perm_unit = "spatial_cluster", n_perm = 100, treated_ids = df %>% filter(City == "Nantucket") %>% distinct(id) %>% pull(), seed = 123, return_plot = TRUE)


# Robustness tests --------------------------------------------------------
# Event study daily pre-trends (note date_range is automatically constrained by min(dates) in the pre-trend analysis using "window" mode)
ptoutvd<-DiD_ri(df = df, al_treat_groups = al_treat_groups, # Note beach closures on 8/3 that seem to have a negative impact on visitation
               mode = "daily", dates = seq(as.Date("2024-06-15"), as.Date("2024-07-15"), by = "day"), date_range = as.Date(c("2024-06-15", "2024-07-15")), 
               model_formula = visits ~ treat_post | id + date,
               perm_unit = "spatial_cluster", n_perm = 500, treated_ids = df %>% filter(City == "Nantucket") %>% distinct(id) %>% pull(), seed = 123, return_plot = TRUE, 
               pretrend_formula = NULL)

ptoutvd$summary %>% filter(as.Date(label) < as.Date("2024-07-16")) %>% print(n = nrow(.)) # Daily treatment impacts for days in the pre-period 

# Placebo pre-treatment treatment window
ptoutvp<-DiD_ri(df = df, al_treat_groups = al_treat_groups, 
                mode = "window", dates = as.Date(c("2024-07-14", "2024-07-15")), date_range = as.Date(c("2024-06-15", "2024-07-30")), 
                model_formula = visits ~ treat_post | id + date,
                perm_unit = "spatial_cluster", n_perm = 500, treated_ids = df %>% filter(City == "Nantucket") %>% distinct(id) %>% pull(), seed = 123, return_plot = TRUE,
                pretrend_formula = NULL)
ptoutvp$plot

# Time interaction for parallel trends
ptoutvpt<-DiD_ri(df = df, al_treat_groups = al_treat_groups, 
                mode = "window", dates = as.Date(c("2024-07-16", "2024-07-17")), date_range = as.Date(c("2024-06-15", "2024-07-30")), 
                model_formula = visits ~ treat_post | id + date,
                perm_unit = "spatial_cluster", n_perm = 500, treated_ids = df %>% filter(City == "Nantucket") %>% distinct(id) %>% pull(), seed = 123, return_plot = TRUE, 
                pretrend_formula = visits ~ time + treated:time | id)
ptoutvpt$pretrend_slope
ptoutvpt$pretrend_plot


# DiD model August 3 closure ----------------------------------------------
closed_ids_803<-c(201,195,199,193)
out_closure <- DiD_ri(
  df = df,
  al_treat_groups = NULL,                  # not used (perm_unit = "id")
  mode = "window",                         # use "window" so the pretrend block can run
  dates = as.Date("2024-08-03"),           # single-day window is fine
  model_formula = visits ~ treat_post | id + date,
  perm_unit = "id",
  n_perm = 1000,
  treated_ids = closed_ids_803,
  #treated_ids = df %>% filter(City == "Nantucket") %>% distinct(id) %>% pull(),
  date_range = as.Date(c("2024-08-01","2024-08-15")),  
  pretrend_formula = visits ~ time + time:treated | id, 
  return_plot = TRUE,
  seed = 123
)

total_effect803v1<-round(as.numeric(out_closure$summary$estimate)*length(unique(df %>% filter(City == "Nantucket") %>% pull(id)))*cfa*cvf,0) # One day
total_effect803v2<-round(as.numeric(out_closure$summary$estimate)*2*length(unique(df %>% filter(City == "Nantucket") %>% pull(id)))*cfa*cvf,0) # Two days



# Staggered rollout model -------------------------------------------------
al_treat_groups2<-al %>% filter(id %in% unique(df$id)) # Unique groups of high-visitation beaches based on 23km clustering of Nantucket pollution

dist_matrix<-matrix(as.numeric(st_distance(st_transform(al_treat_groups2, 26919))), nrow = nrow(al_treat_groups2))

al_treat_groups2<-al_treat_groups2 %>% mutate(group = map(seq_len(n()), ~ al_treat_groups2$id[which(dist_matrix[.x, ] <= 23000)])) # Identifying groups of beaches within 23km of each other

al_treat_groups2 %>% 
  mutate(group_str = map_chr(seq_along(group), ~ paste0(sort(group[[.x]]), collapse = "-"))) %>% 
  count(group_str, name = "group_frequency") %>% 
  arrange(desc(group_frequency)) %>% 
  st_drop_geometry()


ATTgt_rollout <- function(
    df, adoption_map, date_range,
    outcome   = "visits",
    id_var    = "id",
    time_var  = "date",
    treat_unit_var = "City",
    xformla  = ~ 1,                      # use ~1 (intercept); ~0 can break did internals
    control_group = c("notyettreated","nevertreated"),
    clustervars = NULL,
    panel = TRUE,
    allow_unbalanced_panel = TRUE,
    anticipation = 0,
    bstrap = FALSE, biters = 999,
    k_keep = NULL,
    windows = NULL,
    compute_pooled_dynamic = TRUE,
    return_plot = TRUE,
    seed = 123
){
  set.seed(seed)
  control_group <- match.arg(control_group)
  if (!inherits(adoption_map, "Date")) adoption_map <- as.Date(adoption_map)
  
  df1 <- df %>%
    dplyr::filter(.data[[time_var]] >= date_range[1], .data[[time_var]] <= date_range[2]) %>%
    dplyr::mutate(date = as.Date(.data[[time_var]]))
  
  dates <- sort(unique(df1$date))
  date_to_idx <- setNames(seq_along(dates), as.character(dates))
  idx_to_date <- setNames(dates, seq_along(dates))
  
  adopt_vec <- setNames(as.Date(adoption_map), names(adoption_map))
  g_idx_map  <- setNames(date_to_idx[as.character(adopt_vec)], names(adopt_vec))
  
  df2 <- df1 %>%
    dplyr::mutate(
      id_num = as.integer(factor(.data[[id_var]])),
      t_idx  = as.integer(date_to_idx[as.character(date)]),
      g_idx  = ifelse(.data[[treat_unit_var]] %in% names(g_idx_map),
                      as.integer(g_idx_map[.data[[treat_unit_var]]]), 0L)
    )
  
  if (is.null(clustervars)) clustervars <- treat_unit_var
  
  # ensure design matrix has ≥1 column
  if (ncol(model.matrix(xformla, df2)) == 0L) xformla <- ~ 1
  
  keep_rows <- complete.cases(df2[[outcome]]) &
    complete.cases(model.matrix(xformla, df2))
  df3 <- df2[keep_rows, , drop = FALSE]
  
  # ---- Callaway & Sant’Anna (did) ----
  att_obj <- did::att_gt(
    yname  = outcome,
    tname  = "t_idx",
    idname = "id_num",
    gname  = "g_idx",
    xformla = xformla,
    panel  = panel,
    control_group = control_group,
    clustervars = clustervars,
    data   = df3,
    bstrap = bstrap,
    biters = biters,
    allow_unbalanced_panel = allow_unbalanced_panel,
    anticipation = anticipation
  )
  
  # tidy (avoid name collision by using att_hat / se_hat)
  gt <- tibble::tibble(
    row_idx = seq_along(att_obj$att),
    g_idx   = att_obj$group,
    t_idx   = att_obj$t,
    att_hat = as.numeric(att_obj$att),
    se_hat  = as.numeric(att_obj$se)
  ) %>%
    dplyr::mutate(
      cohort = as.Date(idx_to_date[as.character(g_idx)]),
      time   = as.Date(idx_to_date[as.character(t_idx)]),
      k      = as.integer(t_idx - g_idx),
      ci_lo  = att_hat - 1.96*se_hat,
      ci_hi  = att_hat + 1.96*se_hat
    ) %>%
    dplyr::arrange(cohort, k)
  
  if (!is.null(k_keep)) gt <- dplyr::filter(gt, k %in% k_keep)
  
  pooled_dynamic <- NULL
  if (compute_pooled_dynamic) {
    dyn <- did::aggte(att_obj, type = "dynamic")
    pooled_dynamic <- tibble::tibble(
      k = as.integer(dyn$egt),
      att = as.numeric(dyn$att.egt),
      se  = as.numeric(dyn$se.egt)
    ) %>% dplyr::mutate(ci_lo = att - 1.96*se, ci_hi = att + 1.96*se)
  }
  
  # windows (robust indexing)
  window_est <- NULL
  if (!is.null(windows)) {
    V <- att_obj$V
    nV <- if (is.null(V)) 0L else nrow(V)
    
    window_est <- purrr::imap_dfr(windows, function(ks, cohort_chr){
      idx_gt <- which(gt$cohort == as.Date(cohort_chr) & gt$k %in% as.integer(ks))
      if (!length(idx_gt)) {
        return(tibble::tibble(cohort = as.Date(cohort_chr), k_window = paste(ks, collapse=","),
                              estimate = NA_real_, se = NA_real_,
                              ci_lo = NA_real_, ci_hi = NA_real_, p = NA_real_))
      }
      rows <- gt$row_idx[idx_gt]
      ok <- !is.na(rows) & rows >= 1L & (nV == 0L | rows <= nV)
      rows <- rows[ok]; idx_gt <- idx_gt[ok]
      if (!length(rows)) {
        return(tibble::tibble(cohort = as.Date(cohort_chr), k_window = paste(ks, collapse=","),
                              estimate = NA_real_, se = NA_real_,
                              ci_lo = NA_real_, ci_hi = NA_real_, p = NA_real_))
      }
      L <- rep(1/length(rows), length(rows))
      est <- sum(L * gt$att_hat[idx_gt])
      if (!is.null(V)) {
        var <- tryCatch(as.numeric(t(L) %*% V[rows, rows, drop = FALSE] %*% L),
                        error = function(e) sum((L^2) * diag(V)[rows], na.rm = TRUE))
      } else var <- NA_real_
      se  <- sqrt(var)
      tibble::tibble(
        cohort = as.Date(cohort_chr),
        k_window = paste(ks, collapse=","),
        estimate = est,
        se = se,
        ci_lo = if (is.finite(se)) est - 1.96*se else NA_real_,
        ci_hi = if (is.finite(se)) est + 1.96*se else NA_real_,
        p = if (is.finite(se) && se > 0) 2*pnorm(-abs(est/se)) else NA_real_
      )
    })
  }
  
  plot_obj <- NULL
  if (return_plot) {
    plot_obj <- ggplot2::ggplot(gt, ggplot2::aes(k, att_hat)) +
      ggplot2::geom_hline(yintercept = 0, linewidth = 0.4, alpha = .6) +
      ggplot2::geom_vline(xintercept = -1, linetype = 3, alpha = .6) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lo, ymax = ci_hi), alpha = .15) +
      ggplot2::geom_line() + ggplot2::geom_point(size = 1.2) +
      ggplot2::facet_wrap(~ cohort, scales = "free_y") +
      ggplot2::labs(title = "Cohort-specific event time ATT (Callaway–Sant’Anna)",
                    x = "Event time k = t_idx − g_idx", y = "ATT(g,t) with 95% CI") +
      ggplot2::theme_minimal(base_size = 12)
  }
  
  list(att_object = att_obj, gt = gt, windows = window_est,
       pooled_dynamic = pooled_dynamic, plot = plot_obj,
       mapping = list(date_to_idx = date_to_idx, idx_to_date = idx_to_date))
}

adoption_map <- c(
  "Nantucket"      = "2024-07-16",
  "Oak Bluffs"     = "2024-07-31",
  "Aquinnah"       = "2024-07-31",
  "Edgartown"      = "2024-07-31",
  "Chilmark"       = "2024-07-31",
  "Tisbury"        = "2024-07-31",
  "West Tisbury"   = "2024-07-31",
  "Westport"       = "2024-07-31",
  "Little Compton" = "2024-07-31"
)

res <- ATTgt_rollout(
  df = df,
  adoption_map = adoption_map,
  date_range = as.Date(c("2024-06-15","2024-08-15")),
  outcome = "visits",
  id_var = "id",
  time_var = "date",
  treat_unit_var = "City",
  xformla = ~ 1,                     # or ~ tempmaxF + precIn
  control_group = "notyettreated",
  clustervars = "City",
  anticipation = 0,
  bstrap = TRUE, biters = 999,
  k_keep = -10:10,
  windows = list("2024-07-16" = 0:1, "2024-07-31" = 0:0),
  #windows = NULL,
  compute_pooled_dynamic = TRUE,
  return_plot = TRUE
)

res$gt %>% print(n=Inf)
res$windows
res$pooled_dynamic
res$att_object
res$plot
res$mapping


# Travel cost model -------------------------------------------------------
dfs %>% filter(City == "Nantucket" & year == 2024 & CENSUS_BLOCK_GROUP_ID != "NULL") %>% distinct(CENSUS_BLOCK_GROUP_ID,id) %>% nrow() # Unique A to B pairs for dist calc

# Setting up API key for access to google distance matrix api
api_key<-read.csv(file = "GDMapikey.csv", header = FALSE)
set.api.key(api_key$V1)

