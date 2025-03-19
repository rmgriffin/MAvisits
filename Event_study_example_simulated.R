# Intro -------------------------------------------------------------------
rm(list=ls()) # Clears workspace

# install.packages("renv") # Install/call libraries
# renv::init()

PKG<-c("fixest","tidyverse")

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

panel<-panel %>% # Define treatment indicator (1 if treated and after treatment time)
  mutate(
    treated = ifelse(!is.na(treat_time) & time >= treat_time, 1, 0),
    unit_fe = rep(rnorm(n), times = t),    # Repeat each unit's FE across time
    time_fe = rep(rnorm(t), each = n)      # Repeat each time's FE across units
  )

# Generate outcome based on treatment and fixed effects
panel<-panel %>%
  mutate(
    epsilon = rnorm(n * t),   # Random error
    y = 3 + beta * treated + unit_fe + time_fe + epsilon  # Outcome equation
  )

# Event Study Model -------------------------------------------------------
panel<-panel %>% # Create event time relative to treatment
  mutate(event_time = ifelse(!is.na(treat_time), time - treat_time, NA))

model<-feols(y ~ i(event_time, ref = -1) | id + time, data = panel) # Estimate event study using fixest
model<-feols(y ~ i(event_time, ref = -1), data = panel) # Estimate event study using fixest

model_clustered<-feols(y ~ i(event_time, ref = -1) | id + time, data = panel, cluster = ~id)
summary(model_clustered)

# Validate against data generating process --------------------------------
coef_df<-data.frame( # Extract estimated coefficients
  event_time = as.numeric(sub("event_time::", "", names(coef(model)))),
  estimate = coef(model),
  conf.low = confint(model)[,1],
  conf.high = confint(model)[,2]
)

post_treatment<-coef_df %>% filter(event_time >= 0)

mse<-mean((post_treatment$estimate - beta)^2) # Mean squared error of post-treatment estimates versus true beta

post_treatment<-post_treatment %>% # Are estimated effects statistically different from β?
  mutate(
    t_stat = (estimate - beta) / ((conf.high - conf.low) / 3.92), # Approx. SE from 95% CI
    p_value = 2 * pt(abs(t_stat), df = n * t - length(coef(model)), lower.tail = FALSE)
  )

# Plot event study results
ggplot(post_treatment, aes(x = event_time, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = beta, linetype = "dashed", color = "red") +  # True effect
  geom_hline(yintercept = 0, linetype = "dotted") + 
  labs(title = "Event Study Estimates", x = "Event Time", y = "Estimated Effect") +
  theme_minimal()
