# Intro -------------------------------------------------------------------
rm(list=ls()) # Clears workspace

# install.packages("renv") # Install/call libraries
# renv::init()

PKG<-c("fixest","tidyverse","did")

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p)
    require(p,character.only = TRUE)}
}

renv::snapshot()
rm(p,PKG)

set.seed(123) # Seed for reproducibility

# Simulate Panel Data -----------------------------------------------------
n<-50000    # Number of units
t<-20     # Time periods
beta<-2   # True treatment effect

panel<-expand.grid(id = 1:n, time = 1:t) # Create panel structure

panel<-panel %>% # Assign treatment time randomly for each unit (some never treated)
  group_by(id) %>%
  mutate(treat_time = sample(c(sample(5:15, 1), NA), 1)) %>%
  ungroup()

panel <- panel %>% 
  mutate(
    treated = ifelse(!is.na(treat_time) & time >= treat_time, 1, 0),
    event_time = ifelse(!is.na(treat_time), time - treat_time, NA),
    unit_fe = rep(rnorm(n), times = t),
    time_fe = rep(rnorm(t), each = n)
  )

panel <- panel %>%
  mutate(
    post_treat_time = ifelse(event_time >= 0, event_time, NA),
    treated_effect = ifelse(event_time >= 0, beta - 0.1 * event_time, 0) # Linear decay
  )

# Generate outcome based on treatment and fixed effects
panel<-panel %>%
  mutate(
    epsilon = rnorm(n * t),   # Random error
    y = 3 + treated_effect + unit_fe + time_fe + epsilon  # Outcome equation
  )

# Event Study Model -------------------------------------------------------
# model_clustered<-feols(y ~ i(event_time, ref = -1) | id + time, data = panel, cluster = ~id) - note, fixed effects models of have issues with small and large sample sizes (seemingly only with unit fixed effects?)
# # Issue in large samples seems to be associated with differences in unit fixed effects at different event times
# panel %>%
#   group_by(event_time) %>%
#   summarize(mean_unit_fe = mean(unit_fe, na.rm = TRUE)) %>%
#   ggplot(aes(x = event_time, y = mean_unit_fe)) +
#   geom_line()

model1<-feols(y ~ i(event_time, ref = -1) | time, data = panel) # Standard event study
#model2 <- feols(y ~ i(event_time, ref = -1) + post_treat_time | time, data = panel) # Event study with linear decay
model3 <- feols(y ~ i(event_time, ref = -1) + i(event_time, ref = -1):treated | time, data = panel) # Distributed Lag Model


plot_results <- function(model, title) {
  coef_df <- data.frame(
    event_time = as.numeric(sub("event_time::", "", names(coef(model)))),
    estimate = coef(model),
    conf.low = confint(model)[,1],
    conf.high = confint(model)[,2]
  )

  post_treatment <- coef_df %>% filter(event_time >= 0)

  # Compute true expected effect based on the DGP
  post_treatment <- post_treatment %>%
    mutate(true_effect = 2 - 0.1 * event_time)  # This is our known data-generating function

  ggplot(post_treatment, aes(x = event_time, y = estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
    geom_line(aes(y = true_effect), color = "red", linetype = "dashed", size = 1) +  # True effect line
    geom_hline(yintercept = 0, linetype = "dotted") + 
    labs(title = title, x = "Event Time", y = "Estimated Effect") +
    theme_minimal()
}

# Show plots with corrected true effect
plot_results(model1, "Standard Event Study (Time FE)")
#plot_results(model2, "Event Study with Linear Decay")
plot_results(model3, "Distributed Lag Model (DLM)")


# Impact calculation ------------------------------------------------------
coef_df <- data.frame( # Extract estimated event study coefficients
  event_time = as.numeric(sub("event_time::", "", names(coef(model1)))),  # Adjust for correct model
  estimate = coef(model3)
)

panel <- panel %>% # Merge the estimated effects into the panel dataset
  left_join(coef_df, by = "event_time") %>%
  mutate(
    estimate = ifelse(event_time >= 0, estimate, 0)  # Only apply to post-treatment periods
  )

# panel$estimate<-ifelse(panel$estimate<0,0,panel$estimate) # If a linear model, will predict negative values. Use DLM to avoid this.

panel %>% # Cumulative impact by id
  group_by(id) %>% 
  summarise(treat = mode(treated), impact = sum(estimate))

panel %>% # Cumulative impact 
  summarise(impact = sum(estimate, na.rm = TRUE))


# DID (Callaway and Sant'Anna) --------------------------------------------
# The did package expects a panel dataset with:
#  - id column
#  - time column
#  - treatment variable (0/1)
#  - outcome variable
#  - a variable specifying when treatment occurs (never-treated units get NA)

# panel <- panel %>%
#   mutate(
#     D = ifelse(!is.na(treat_time) & time >= treat_time, 1, 0)  # Treatment status at each period
#   )
# 
# att_est <- att_gt(
#   yname = "y",         # Outcome variable
#   tname = "time",      # Time variable
#   idname = "id",       # Panel identifier
#   gname = "treat_time", # First treatment time
#   data = panel,
#   est_method = "dr"    # Doubly robust estimation
# )
