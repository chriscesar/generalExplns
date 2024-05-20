# NVZ_trends_v.04.R ####
### this time, we are converting the results to consistent units!

## load packages
ld_pkgs <- c("tidyverse","ggplot2","trend","tictoc", "rkt")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

tictoc::tic.clearlog()
tic("Import metadata and raw data")#start timer
theme_set(ggthemes::theme_few())

### load data
dfraw <- as_tibble(readxl::read_excel("data/All nutrients.xlsx",
                                      sheet="combinedNO3-N",
                                      guess_max = 500000))
# Create a folder to store the plots if it doesn't exist
if (!file.exists("plots/ver4")) {
  dir.create("plots/ver4")
}

dfraw %>% 
  janitor::clean_names(.) %>% 
  mutate(year = as.numeric(year)) %>% #change year to numeric
  select(.,-c(
    month,wb_distance,reg_distance,smpt_grid_ref, ###remove unnecessary and potentially confusing cols
    samp_id,samp_lab_ref_no,samp_sample_time,
    samp_notes,ngr_easting,ngr_northing
  )) -> df0 #clean variable names
toc(log = TRUE)

tic("Tidy data: remove 'problematic' values")
df0 %>% filter(.,flag == 0) -> df0_trim

df0_trim %>%
    select(.,region:ea_wb_id,
           year,season,nitrate_use,units_use) -> df_no3

# calculate means and remove data-limited WBs
df_no3 %>% 
  filter(.,!is.na(nitrate_use)) %>%
  group_by(water_body,year) %>%
  summarise(mean_no3=mean(nitrate_use, na.rm=TRUE)) %>%  
  group_by(water_body) %>% filter(n() >= 3) %>% ungroup() %>%
  rename(nitrate_umol_l=mean_no3)-> df_no3_mean

df_no3_mean <- df_no3_mean[order(df_no3_mean$water_body,df_no3_mean$year),]
toc(log=TRUE)


tic("Calculate MK trends for each WB")
# Create an empty data frame to store results
trend_results <- data.frame(WB_Name = character(),
                            S = numeric(),
                            p.value = numeric(),
                            n = numeric(),
                            statname = character(),
                            tau = numeric(),
                            significance = character(),
                            stringsAsFactors = FALSE)

#===============

# Loop through unique WB_Names
for(name in unique(df_no3_mean$water_body)) {
  # Subset data for current WB_Name
  wb_data <- subset(df_no3_mean, water_body == name)
  
  # Perform Mann-Kendall trend test
  mk.test <- trend::mk.test(wb_data$nitrate_umol_l)
  # mk.test <- rkt::rkt(wb_data$year,wb_data$nitrate_umol_l)
  
  # Extract S statistic, p-value, and Sen's Slope
  
  n <- samp_n <- count(wb_data)
  z <- round(mk.test$statistic, 3)
  p_value <- round(mk.test$p.value, 4)
  tau <- round(mk.test$estimates[3], 3)
  Sen <- round(sens.slope(wb_data$nitrate_umol_l)$estimates,3)
  
  # Determine significance level
  significance <- ifelse(p_value < 0.001, "***",
                         ifelse(p_value < 0.01, "**",
                                ifelse(p_value < 0.05, "*",
                                       ifelse(p_value < 0.1, "+", ""))))
  
  p_value <- ifelse(round(mk.test$p.value, 4)==0,"<0.0001",
                    ifelse(round(mk.test$p.value, 4)==1,">0.9999",
                           round(mk.test$p.value, 4)))
  
  # Compute min and max Year values within each level of WB_Name
  min_year <- min(wb_data$year)
  max_year <- max(wb_data$year)
  
  # Add results for current WB_Name to trend_results
  trend_results <- rbind(trend_results, data.frame(water_body = name, n = n,
                                                   min_year = min_year,
                                                   max_year = max_year,
                                                   # stat=statname,
                                                   z_score = z,
                                                   tau = tau,
                                                   Sen=Sen,
                                                   p.value = p_value,
                                                   sig = significance))
}

# Print the final table
print(trend_results)
write.csv(trend_results, file="plots/ver4/NO3trend_results.csv",row.names = FALSE)

toc(log=TRUE)

## create trend plot for each level of WB_Name ####
tic("Generate trend plots for each WB")
# Loop through unique WB_Names
for(name in unique(df_no3_mean$water_body)) {
  # Replace "/" slashes with another character (e.g., "_")
  sanitized_name <- gsub("/", "_", name)
  
  trend_results_section <- subset(trend_results, water_body == name)
  
  # Subset data for current WB_Name
  wb_data <- subset(df_no3_mean, water_body == name)
  
  # Perform Mann-Kendall trend test
  mk.test <- trend::mk.test(wb_data$nitrate_umol_l)
  
  # Extract S statistic and p-value
  samp_n <- count(wb_data)
  z <- round(mk.test$statistic, 3)
  
  # p_value <- round(mk.test$p.value, 4)
  p_value <- ifelse(round(mk.test$p.value, 4)==0,"<0.0001",
                    ifelse(round(mk.test$p.value, 4)==1,">0.9999",
                           round(mk.test$p.value, 4)))
  tau <- round(mk.test$estimates[3], 3)
  ymin <- min(wb_data$year);ymax <- max(wb_data$year)
  Sen <- trend_results_section$Sen
  
  # Create a plot
  p <- ggplot(wb_data, aes(x = year, y = nitrate_umol_l)) +
    geom_point() +
    # ylim(0,NA)+
    geom_smooth(method = "lm", se = FALSE) + # Add a linear trend line
    scale_x_continuous(breaks = seq(min(wb_data$year), max(wb_data$year),by=2))+
    labs(title = paste(name),
         subtitle = paste0("n = ", samp_n, ", from = ",ymin, ", to = ",ymax,",\nz-score = ", z,", Sen's slope = ",Sen,", tau = ",tau, ", p = ", p_value,trend_results_section$sig),
         caption = "Trend calculations are based on mean NO<sub>3</sub> concentrations across each calendar year.<br>Statistical outputs from a Mann-Kendal analysis of concentrations over time are presented.<br>Blue line indicates linear model trend for visualisation purposes only.",
         x = "Year",
         y = bquote(NO[3]~(umol^-l))
         )+
    theme(plot.caption = ggtext::element_markdown())
  
  # Export the plot to a file with the sanitized WB_Name value in the filename
  ggsave(paste("plots/ver4/NO3_", sanitized_name, ".png", sep = ""), plot = p, width = 8, height = 6)
  # rm(sanitized_name,trend_results_section,wb_data,mk.test,samp_n,
  #    p_value,tau,ymin,ymax,p,n,name,min_year,max_year,Sen)
}
toc(log=TRUE)

rm(df_no3_mean)

#NEXT STEPS: ####
## TRIM DATA TO INCLUDE WINTER ONLY AND GENERATE MEANS BY YEAR/WINTER
df_no3 %>% 
  filter(.,!is.na(nitrate_use)) %>%
  filter(.,season=="Winter") %>% 
  group_by(water_body,year) %>%
  summarise(mean_no3=mean(nitrate_use, na.rm=TRUE)) %>%  
  group_by(water_body) %>% filter(n() >= 3) %>% ungroup() %>%
  rename(nitrate_umol_l=mean_no3)-> df_no3_mean

df_no3_mean <- df_no3_mean[order(df_no3_mean$water_body,df_no3_mean$year),]

## RECALCULATE MK ANALYSES ####
tic("Calculate MK trends for each WB (Winter data)")
# Create an empty data frame to store results
trend_results <- data.frame(WB_Name = character(),
                            S = numeric(),
                            p.value = numeric(),
                            n = numeric(),
                            statname = character(),
                            tau = numeric(),
                            significance = character(),
                            stringsAsFactors = FALSE)

#===============

# Loop through unique WB_Names
for(name in unique(df_no3_mean$water_body)) {
  # Subset data for current WB_Name
  wb_data <- subset(df_no3_mean, water_body == name)
  
  # Perform Mann-Kendall trend test
  mk.test <- trend::mk.test(wb_data$nitrate_umol_l)
  # mk.test <- rkt::rkt(wb_data$year,wb_data$nitrate_umol_l)
  
  # Extract S statistic, p-value, and Sen's Slope
  
  n <- samp_n <- count(wb_data)
  z <- round(mk.test$statistic, 3)
  p_value <- round(mk.test$p.value, 4)
  tau <- round(mk.test$estimates[3], 3)
  Sen <- round(sens.slope(wb_data$nitrate_umol_l)$estimates,3)
  
  # Determine significance level
  significance <- ifelse(p_value < 0.001, "***",
                         ifelse(p_value < 0.01, "**",
                                ifelse(p_value < 0.05, "*",
                                       ifelse(p_value < 0.1, "+", ""))))
  
  p_value <- ifelse(round(mk.test$p.value, 4)==0,"<0.0001",
                    ifelse(round(mk.test$p.value, 4)==1,">0.9999",
                           round(mk.test$p.value, 4)))
  
  # Compute min and max Year values within each level of WB_Name
  min_year <- min(wb_data$year)
  max_year <- max(wb_data$year)
  
  # Add results for current WB_Name to trend_results
  trend_results <- rbind(trend_results, data.frame(water_body = name, n = n,
                                                   min_year = min_year,
                                                   max_year = max_year,
                                                   # stat=statname,
                                                   z_score = z,
                                                   tau = tau,
                                                   Sen=Sen,
                                                   p.value = p_value,
                                                   sig = significance))
}

# Print the final table
print(trend_results)
write.csv(trend_results, file="plots/ver4/NO3trend_results_WINTER.csv",row.names = FALSE)
toc(log = TRUE)

## PRODUCE PLOTS FOR WINTER TRENDS ####
## create trend plot for each level of WB_Name ####
tic("Generate trend plots for each WB (Winter)")
# Loop through unique WB_Names
for(name in unique(df_no3_mean$water_body)) {
  # Replace "/" slashes with another character (e.g., "_")
  sanitized_name <- gsub("/", "_", name)
  
  trend_results_section <- subset(trend_results, water_body == name)
  
  # Subset data for current WB_Name
  wb_data <- subset(df_no3_mean, water_body == name)
  
  # Perform Mann-Kendall trend test
  mk.test <- trend::mk.test(wb_data$nitrate_umol_l)
  
  # Extract S statistic and p-value
  samp_n <- count(wb_data)
  z <- round(mk.test$statistic, 3)
  
  # p_value <- round(mk.test$p.value, 4)
  p_value <- ifelse(round(mk.test$p.value, 4)==0,"<0.0001",
                    ifelse(round(mk.test$p.value, 4)==1,">0.9999",
                           round(mk.test$p.value, 4)))
  tau <- round(mk.test$estimates[3], 3)
  ymin <- min(wb_data$year);ymax <- max(wb_data$year)
  Sen <- trend_results_section$Sen
  
  # Create a plot
  p <- ggplot(wb_data, aes(x = year, y = nitrate_umol_l)) +
    geom_point() +
    # ylim(0,NA)+
    geom_smooth(method = "lm", se = FALSE) + # Add a linear trend line
    scale_x_continuous(breaks = seq(min(wb_data$year), max(wb_data$year),by=2))+
    labs(title = paste(name),
         subtitle = paste0("n = ", samp_n, ", from = ",ymin, ", to = ",ymax,",\nz-score = ", z,", Sen's slope = ",Sen,", tau = ",tau, ", p = ", p_value,trend_results_section$sig),
         caption = "Trend calculations are based on mean winter NO<sub>3</sub> concentrations across each calendar year.<br>Statistical outputs from a Mann-Kendal analysis of concentrations over time are presented.<br>Blue line indicates linear model trend for visualisation purposes only.",
         x = "Year",
         y = bquote(NO[3]~(umol^-l))
    )+
    theme(plot.caption = ggtext::element_markdown())
  
  # Export the plot to a file with the sanitized WB_Name value in the filename
  ggsave(paste("plots/ver4/Winter_NO3_", sanitized_name, ".png", sep = ""), plot = p, width = 8, height = 6)
  # rm(sanitized_name,trend_results_section,wb_data,mk.test,samp_n,
  #    p_value,tau,ymin,ymax,p,n,name,min_year,max_year,Sen)
}
toc(log=TRUE)
unlist(tictoc::tic.log())
