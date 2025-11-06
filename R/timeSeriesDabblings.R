# timeSeriesDabblings.R

# playing with time series analysis of continuous data

## load packages ####
ld_pkgs <- c("tidyverse", "tictoc","mgcv","gratia")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE);rm(ld_pkgs)

tictoc::tic.clearlog()
# DO ####
tictoc::tic("Set metadata and load/format data")

### Set metadata and load/format data####
# data folder
source("R/00_folders.R")

df_do0 <- as_tibble(read.csv(paste0(WB_DO_fol,"THAMES_PURFLEET_E_200707-DISSOLVED OXYGEN-15min-Measured.csv")))

# Convert to proper date-time class
df_do0 %>% 
  mutate(
    date_time = ymd_hms(dateTime),
    date = as_date(date_time),
    time = format(date_time, "%H:%M:%S")
  ) ->df_do

df_do <- df_do |>
  mutate(
    #convert timestamps into a continuous numeric variable (time_num) that GAMs can handle easily
    time_num  = as.numeric(difftime(date_time, min(date_time), units = "days"))  # time in days
  )

df_do <- df_do |>
  mutate(
    hour = hour(date_time) + minute(date_time) / 60,
    doy  = yday(date_time)
  )
tictoc::toc(log=TRUE)

### fit gam ####
# gam_fit <- gam(
#   value ~ 
#     s(time_num, k = 50, bs = "tp") +   # smooth long-term trend
#     s(doy, bs = "cc", k = 20) +        # yearly cycle
#     s(hour, bs = "cc", k = 24),        # daily cycle
#   data = df_do,
#   method = "REML"
#   )
# saveRDS(gam_fit, file="data/timeSeriesGAMThamesDO.Rdat")
tictoc::toc(log=TRUE)

tictoc::tic("Plots")
gam_fit <- readRDS("data/timeSeriesGAMThamesDO.Rdat")
### Quick plots ####
draw(gam_fit)
ggsave(filename = "plots/gam_fit_DO_gratia1.png")

## time series decomposition ###
df_effects <- gratia::smooth_estimates(gam_fit)

ggplot(df_effects, aes(x = time_num, y = .estimate)) +
  # geom_line() +
  geom_ribbon(aes(ymin = .estimate - 2 * .se,
                  ymax = .estimate + 2 * .se),
              alpha = 0.2) +
  facet_wrap(~ .smooth, scales = "free_x", ncol = 1) +
  theme_minimal(base_size = 13) +
  labs(
    title = "GAM Decomposition of Dissolved Oxygen time series",
    x = "Time component",
    y = "Estimated effect ± 2 SE"
  )

# reconstruct a full decomposition
df_plot <- df_do |>
  mutate(
    trend = predict(gam_fit, exclude = c("s(doy)", "s(hour)")),
    seasonality = predict(gam_fit, exclude = c("s(time_num)"))
  )

df_plot |>
  tidyr::pivot_longer(
    c(value, trend, seasonality),
    names_to = "component",
    values_to = "y"
  ) |>
  ggplot(aes(x = date_time, y = y, color = component)) +
  geom_line() +
  theme_minimal(base_size = 13) +
  labs(title = "GAM Time Series Decomposition", y = "Value", x = NULL) ->pl
ggsave(pl,filename = "plots/gam_fit_DO_decomp1.png");rm(pl)
###########


pred_terms <- as_tibble(predict(gam_fit, type = "terms"))
names(pred_terms)

trend <- pred_terms[, "s(time_num)"]
season <- pred_terms[, "s(doy)"]
day <- pred_terms[,"s(hour)"]
residuals <- residuals(gam_fit)

components <- data.frame(
  time = df_do$time,
  trend = trend,
  season = season,
  day = day,
  residuals = residuals
)

components_long <- pivot_longer(components, -time, names_to = "component", values_to = "value")

# ggplot(components_long, aes(x = time, y = value)) +
#   geom_line(color = "steelblue") +
#   facet_wrap(~component, scales = "free_y", ncol = 1) +
#   theme_minimal(base_size = 13) +
#   labs(title = "GAM-Based Time Series Decomposition",
#        x = "Date", y = "Component Value")

hourly <- components_long %>% filter(component == "s.hour.") %>% distinct() %>% mutate(time = hms(time))

daily <- components_long %>% filter(component == "s.doy.") %>% distinct()

trend <- components_long %>% filter(component == "s.time_num.") %>% distinct()

residuals <- components_long %>% filter(component == "residuals") %>% distinct()

ggplot(hourly, aes(x=time,y=value))+
  geom_line()+
  ggthemes::theme_few()+labs(title = "Hourly")
ggplot(daily, aes(x=time,y=value))+
  geom_line()+
  ggthemes::theme_few()+labs(title = "Daily")
ggplot(trend, aes(x=time,y=value))+
  geom_line()+
  ggthemes::theme_few()+labs(title = "Trend")
ggplot(residuals, aes(x=time,y=value))+
  geom_line()+
  ggthemes::theme_few()+labs(title = "Residuals")

###############
#CLAUDE_AI version ####

tictoc::tic("prep data")
# Prepare your data
df_do <- df_do %>%
  mutate(
    # Combine date and time into datetime
    datetime = ymd_hms(paste(date, time)),
    
    # Create temporal features
    # Decimal time (days since start) - for overall trend
    time_numeric = as.numeric(difftime(datetime, min(datetime), units = "days")),
    
    # Day of year (1-365/366) - for seasonal patterns
    doy = yday(datetime),
    
    # Time of day (0-24) - for diurnal patterns
    hour = hour(datetime) + minute(datetime)/60,
    
    # Year as factor for yearly variation
    year = factor(year(datetime))
  )
toc(log=TRUE)

tictoc::tic("Simple model")
# Model with smooth trend and cyclic seasonal component
### m1 ####
# m1 <- gam(value ~ 
#             s(time_numeric, k = 50, bs = "tp") +  # Overall trend
#             s(doy, bs = "cc", k = 20),             # Seasonal (cyclic)
#           data = df_do,
#           method = "REML",
#           knots = list(doy = c(0, 366)))           # Cyclic boundaries
# summary(m1)
# saveRDS(m1, file="data/timeSeriesGAMThamesDO_CAI_m1.Rdat")
m1 <- readRDS("data/timeSeriesGAMThamesDO_CAI_m1.Rdat")
toc(log=TRUE)
draw_m1 <- draw(m1)
ggsave(draw_m1, filename = "plots/gam_fit_DO_gratia1_m1.png")

tictoc::tic("Intermediate model")
# Add time-of-day effects
### m2 ####
# m2 <- gam(value ~ 
#             s(time_numeric, k = 50, bs = "tp") +
#             s(doy, bs = "cc", k = 20) +
#             s(hour, bs = "cc", k = 12),            # Diurnal pattern
#           data = df_do,
#           method = "REML",
#           knots = list(doy = c(0, 366), hour = c(0, 24)))
# 
# summary(m2)
# saveRDS(m2, file="data/timeSeriesGAMThamesDO_CAI_m2.Rdat")
m2 <- readRDS("data/timeSeriesGAMThamesDO_CAI_m2.Rdat")
toc(log=TRUE)
draw_m2 <- draw(m2)
ggsave(draw_m2, filename = "plots/gam_fit_DO_gratia1_m2.png")

tictoc::tic("Complex model")
# Allow seasonal pattern to vary over time using tensor product
### m3 ####
# m3 <- gam(value ~ 
#             s(time_numeric, k = 50, bs = "tp") +
#             te(doy, time_numeric, bs = c("cc", "tp"), k = c(20, 20)) +
#             s(hour, bs = "cc", k = 12),
#           data = df_do,
#           method = "REML",
#           knots = list(doy = c(0, 366), hour = c(0, 24)))
# 
# summary(m3)
# saveRDS(m3, file="data/timeSeriesGAMThamesDO_CAI_m3.Rdat")
m3 <- readRDS("data/timeSeriesGAMThamesDO_CAI_m3.Rdat")
toc(log=TRUE)
draw_m3 <- draw(m3)
ggsave(draw_m3, filename = "plots/gam_fit_DO_gratia1_m3.png")

### model diagnostics ####
# Check model diagnostics
appraise(m2)  # Using gratia's diagnostic plots

# Check for autocorrelation
acf(residuals(m2), main = "ACF of Residuals (m2)")
acf(residuals(m3), main = "ACF of Residuals (m3)")

# If substantial autocorrelation remains, add AR structure:
tic("m2 add AR structure")
### m2_ar ####
# m2_ar <- gam(value ~ 
#                s(time_numeric, k = 50, bs = "tp") +
#                s(doy, bs = "cc", k = 20) +
#                s(hour, bs = "cc", k = 12),
#              data = df_do,
#              method = "REML",
#              correlation = corARMA(form = ~ 1, p = 1),
#              knots = list(doy = c(0, 366), hour = c(0, 24)))
# saveRDS(m2_ar, file="data/timeSeriesGAMThamesDO_CAI_m2_ar.Rdat")
m2_ar <- readRDS("data/timeSeriesGAMThamesDO_CAI_m2_ar.Rdat")
toc(log=TRUE)
draw_m2_ar <- draw(m2_ar)
ggsave(draw_m2_ar, filename = "plots/gam_fit_DO_gratia1_m2_ar.png")

# If substantial autocorrelation remains, add AR structure:
tic("m3 add AR structure")
### m3_ar ####
# m3_ar <- gam(value ~
#             s(time_numeric, k = 50, bs = "tp") +
#             te(doy, time_numeric, bs = c("cc", "tp"), k = c(20, 20)) +
#             s(hour, bs = "cc", k = 12),
#           data = df_do,
#           method = "REML",
#           correlation = corARMA(form = ~ 1, p = 1),
#           knots = list(doy = c(0, 366), hour = c(0, 24)))
# saveRDS(m3_ar, file="data/timeSeriesGAMThamesDO_CAI_m3_ar.Rdat")
m3_ar <- readRDS("data/timeSeriesGAMThamesDO_CAI_m3_ar.Rdat")
toc(log=TRUE)
draw_m3_ar <- draw(m3_ar)
ggsave(draw_m3_ar, filename = "plots/gam_fit_DO_gratia1_m3_ar.png")

# Compare models using AIC ###

# Compare models using AIC ####
AIC(m1, m2,m2_ar, m3, m3_ar)

# Compare using R-squared ####
summary(m1)$r.sq
summary(m2)$r.sq
summary(m2_ar)$r.sq
summary(m3)$r.sq
summary(m3_ar)$r.sq

# plot trends ####
# Plot overall trend
draw(m3, select = "s(time_numeric)") +
  labs(title = "Long-term Trend in Dissolved Oxygen",
       x = "Days Since Start",
       y = "Effect on DO")

# Or with gratia's smooth_estimates
smooth_estimates(m3, "s(time_numeric)") %>%
  add_confint() %>%
  ggplot(aes(x = time_numeric, y = est)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.3) +
  geom_line() +
  labs(title = "Long-term Trend",
       x = "Days Since Start",
       y = "Partial Effect on DO (mg/L)")

# Plot seasonal pattern
draw(m2, select = "s(doy)") +
  labs(title = "Seasonal Pattern in Dissolved Oxygen",
       x = "Day of Year",
       y = "Effect on DO")

# Or create a prettier version
smooth_estimates(m2, "s(doy)") %>%
  add_confint() %>%
  mutate(month = factor(month.abb[ceiling(doy/30.5)], 
                        levels = month.abb)) %>%
  ggplot(aes(x = doy, y = est)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), 
              alpha = 0.3, fill = "steelblue") +
  geom_line(color = "steelblue", linewidth = 1) +
  scale_x_continuous(breaks = seq(1, 365, by = 30.5),
                     labels = month.abb) +
  labs(title = "Seasonal Variation in Dissolved Oxygen",
       x = "Month",
       y = "Partial Effect on DO (mg/L)") +
  theme_minimal()

# Plot diurnal pattern
draw(m2, select = "s(hour)") +
  labs(title = "Diurnal Pattern in Dissolved Oxygen",
       x = "Hour of Day",
       y = "Effect on DO")

# Plot all smooth terms together
draw(m2)

#extract and interpret effects
# Get predicted values for full dataset (fills in missing values)
df_do <- df_do %>%
  mutate(
    fitted = fitted(m2),
    residuals = residuals(m2)
  )

# Decompose into components
# Overall trend component
trend_effect <- predict(m2, type = "terms", terms = "s(time_numeric)")
# Seasonal component
seasonal_effect <- predict(m2, type = "terms", terms = "s(doy)")
# Diurnal component
diurnal_effect <- predict(m2, type = "terms", terms = "s(hour)")

# Add to dataframe
df_do <- df_do %>%
  mutate(
    trend = as.numeric(trend_effect),
    seasonal = as.numeric(seasonal_effect),
    diurnal = as.numeric(diurnal_effect)
  )