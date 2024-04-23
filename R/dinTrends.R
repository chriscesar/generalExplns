# dinTrends.R
## plotting/analysis of trends in DIN data

## load packages
ld_pkgs <- c("tidyverse","ggplot2","trend","tictoc")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

tic()#start timer
theme_set(ggthemes::theme_few())

### load data

df0 <- as_tibble(readxl::read_excel("data/DIN_2000_2023_umols.xlsx", sheet = "Years_combined"))

### which months are included?
table(df0$Month)

df0 %>% 
  dplyr::select(.,-c(`Easting Northing`,Date,Month)) %>% 
  group_by(WB_Name,Year) %>% 
  mutate(median_DIN = median(`DIN (umol)`),
         mean_DIN = mean(`DIN (umol)`)) %>% 
  select(.,-c(`DIN (umol)`)) %>% 
  ungroup() %>% distinct() %>% 
  group_by(WB_Name) %>% 
  filter(n() >= 3) %>% 
  arrange(WB_Name, Year) -> df_trim


# x <- df_trim[df_trim$WB_Name=="ADUR",]
# d <- mk.test(x$mean_DIN)

# ###medians
# tic()
# df_trim %>% 
#   group_by(WB_Name) %>% 
#   summarise(S=mk.test(.$median_DIN)$estimates[1],
#             varS=mk.test(.$median_DIN)$estimates[2],
#             tau=mk.test(.$median_DIN)$estimates[3],
#             n=mk.test(.$parameter),
#             p.value=mk.test(.$p.value)
#             )
# toc()
# 
# tic()
# df0 %>% 
#   group_by(WB_Name) %>% 
#   summarise(
#     tau=mk.test(.$`DIN (umol)`)$tau,
#     p_value=mk.test(.$`DIN (umol)`)$p.value
#   )
# toc()
# 
# mk_test <- function(data) {
#   data %>%
#     group_by(WB_Name) %>%
#     summarise(
#       mk_stat = mk.test(median_DIN, data = .)$statistic,
#       p_value = mk.test(median_DIN, data = .)$p.value
#     )
# }
# 
# # Apply the function to the data and summarize by WB
# trend_analysis <- data %>%
#   group_by(WB_Name) %>%
#   do(mk_test(.))
# 

# Create an empty data frame to store results
# trend_results <- data.frame(WB_Name = character(),
#                             S = numeric(),
#                             p.value = numeric(),
#                             n=numeric(),
#                             statname=character(),
#                             tau=numeric(),
#                             significance = character(),
#                             stringsAsFactors = FALSE)

# # Loop through unique WB_Names
# for(name in unique(df_trim$WB_Name)) {
#   # Subset data for current WB_Name
#   wb_data <- subset(df_trim, WB_Name == name)
#   
#   # Perform Mann-Kendall trend test
#   mk.test <- mk.test(wb_data$median_DIN)
#   
#   # Extract S statistic, p-value, and Sen's Slope
#   n <- mk.test$parameter
#   S <- round(mk.test$statistic,3)
#   p_value <- round(mk.test$p.value,4)
#   tau <- round(mk.test$estimates[3],3)
#   statname <- names(mk.test$statistic)
#   
#   # Determine significance level
#   significance <- ifelse(p_value < 0.001, "***",
#                          ifelse(p_value < 0.01, "**",
#                                 ifelse(p_value < 0.05, "*",
#                                        ifelse(p_value < 0.1, "+", ""))))
#   
#   # Compute min and max Year values within each level of WB_Name
#   min_year <- min(wb_data$Year)
#   max_year <- max(wb_data$Year)
#   
#   # Add results for current WB_Name to trend_results
#   trend_results <- rbind(trend_results, data.frame(WB_Name = name, n=n,
#                                                    min_year = min_year,
#                                                    max_year = max_year,
#                                                    # stat=statname,
#                                                    S = S,
#                                                    tau=tau,
#                                                    p.value = p_value,
#                                                    sig = significance))
# }
# 
# # Print the final table
# print(trend_results)
# 
# toc()#stop timer
# 
# #### create trend plot for each level of WB_Name
# # Create a folder to store the plots if it doesn't exist
# tic()
# if (!file.exists("plots")) {
#   dir.create("plots")
# }
# 
# # Loop through unique WB_Names
# for(name in unique(df_trim$WB_Name)) {
#   # Replace "/" slashes with another character (e.g., "_")
#   sanitized_name <- str_replace_all(name, "/", "_")
#   
#   # Subset data for current WB_Name
#   wb_data <- subset(df_trim, WB_Name == name)
#   
#   # Create a plot
#   p <- ggplot(wb_data, aes(x = Year, y = median_DIN)) +
#     geom_point() +
#     geom_smooth(method = "lm", se = FALSE) + # Add a linear trend line
#     labs(title = paste(name),
#          x = "Year",
#          y = "DIN (umol)")
#   
#   # Export the plot to a file with the sanitized WB_Name value in the filename
#   ggsave(paste("plots/", sanitized_name, ".png", sep = ""), plot = p, width = 8, height = 6)
# }
# toc()

######################
# Create an empty data frame to store results
trend_results <- data.frame(WB_Name = character(),
                            S = numeric(),
                            p.value = numeric(),
                            n = numeric(),
                            statname = character(),
                            tau = numeric(),
                            significance = character(),
                            stringsAsFactors = FALSE)

# Loop through unique WB_Names
for(name in unique(df_trim$WB_Name)) {
  # Subset data for current WB_Name
  wb_data <- subset(df_trim, WB_Name == name)
  
  # Perform Mann-Kendall trend test
  mk.test <- trend::mk.test(wb_data$median_DIN)
  
  # Extract S statistic, p-value, and Sen's Slope
  n <- mk.test$parameter
  S <- round(mk.test$statistic, 3)
  p_value <- round(mk.test$p.value, 4)
  tau <- round(mk.test$estimates[3], 3)
  statname <- names(mk.test$statistic)
  
  # Determine significance level
  significance <- ifelse(p_value < 0.001, "***",
                         ifelse(p_value < 0.01, "**",
                                ifelse(p_value < 0.05, "*",
                                       ifelse(p_value < 0.1, "+", ""))))
  
  # Compute min and max Year values within each level of WB_Name
  min_year <- min(wb_data$Year)
  max_year <- max(wb_data$Year)
  
  # Add results for current WB_Name to trend_results
  trend_results <- rbind(trend_results, data.frame(WB_Name = name, n = n,
                                                   min_year = min_year,
                                                   max_year = max_year,
                                                   # stat=statname,
                                                   S = S,
                                                   tau = tau,
                                                   p.value = p_value,
                                                   sig = significance))
}

# Print the final table
print(trend_results)

write.csv(trend_results, file="plots/trend_results.csv",row.names = FALSE)

# create trend plot for each level of WB_Name
# Create a folder to store the plots if it doesn't exist
if (!file.exists("plots")) {
  dir.create("plots")
}

# Loop through unique WB_Names
for(name in unique(df_trim$WB_Name)) {
  # Replace "/" slashes with another character (e.g., "_")
  sanitized_name <- gsub("/", "_", name)
  
  trend_results_section <- subset(trend_results, WB_Name == name)
  
  # Subset data for current WB_Name
  wb_data <- subset(df_trim, WB_Name == name)
  
  # Perform Mann-Kendall trend test
  mk.test <- trend::mk.test(wb_data$median_DIN)
  
  # Extract S statistic and p-value
  samp_n <- count(wb_data)[2]
  S <- round(mk.test$statistic, 3)
  p_value <- round(mk.test$p.value, 4)
  tau <- round(mk.test$estimates[3], 3)
  
  # Create a plot
  p <- ggplot(wb_data, aes(x = Year, y = median_DIN)) +
    geom_point() +
    ylim(0,NA)+
    geom_smooth(method = "lm", se = FALSE) + # Add a linear trend line
    labs(title = paste(name),
         subtitle = paste0("n = ", samp_n, ", S = ", S,", tau = ",tau, ", p = ", p_value,trend_results_section$sig),
         caption = "Trend calculations are based on median winter DIN concentrations for each calendar year",
         x = "Year",
         y = "DIN (umol)")
  
  # Export the plot to a file with the sanitized WB_Name value in the filename
  ggsave(paste("plots/", sanitized_name, ".png", sep = ""), plot = p, width = 8, height = 6)
}

toc()

# tidy up ####
rm(list = ls(pattern = "^df"))
