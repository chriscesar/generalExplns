# dinTrends.R
## plotting/analysis of trends in DIN data

## load packages
ld_pkgs <- c("tidyverse","ggplot2","trend","tictoc")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

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
  filter(n() >= 3) -> df_trim


x <- df_trim[df_trim$WB_Name=="ADUR",]
d <- mk.test(x$mean_DIN)

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
trend_results <- data.frame(WB_Name = character(),
                            S = numeric(),
                            p.value = numeric(),
                            n=numeric(),
                            statname=character(),
                            tau=numeric(),
                            stringsAsFactors = FALSE)

# Loop through unique WB_Names
for(name in unique(df_trim$WB_Name)) {
  # Subset data for current WB_Name
  wb_data <- subset(df_trim, WB_Name == name)
  
  # Perform Mann-Kendall trend test
  mk.test <- mk.test(wb_data$median_DIN)
  
  # Extract S statistic, p-value, and Sen's Slope
  n <- mk.test$parameter
  S <- mk.test$statistic
  p_value <- mk.test$p.value
  tau <- mk.test$estimates[3]
  statname <- names(mk.test$statistic)
  
  # Add results for current WB_Name to trend_results
  trend_results <- rbind(trend_results, data.frame(WB_Name = name, n=n,
                                                   stat=statname,
                                                   S = S,
                                                   tau=tau,
                                                   p.value = p_value))
}

# Print the final table
print(trend_results)



