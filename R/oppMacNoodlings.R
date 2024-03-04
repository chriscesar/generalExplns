# oppMacNoodlings.R ####

# analysis of OppMac data #

### load packages
library("tidyverse")
library("readxl")

ggplot2::theme_set(ggthemes::theme_few())

### import data
df0 <- as_tibble(read_xlsx("data/OppMac_data.xlsx",
                           sheet = "CCWIP"))

df0$wb_Yr <- paste0(df0$WaterbodyID,"_",df0$SurveyID)
df0$wb_Yr_patch <- paste0(df0$WaterbodyID,"_",df0$SurveyID,"_",df0$PatchID)

#calc total patch area by WB and year
df0 %>% 
  dplyr::select(.,-c(WaterbodyID,SurveyID,PatchID)) %>% 
  group_by(wb_Yr_patch) %>% 
  summarise(area_wb_Yr_patch=mean(`Patch Area`)) -> area_wb_Yr_patch

df <- df0

df <- left_join(df,area_wb_Yr_patch)

# df %>% 
#   dplyr::select(.,-c(WaterbodyID,SurveyID,PatchID)) %>% 
#   View(.)
#   group_by(wb_Yr) %>% 
#   summarise(area_wb_Yr=sum(`Patch Area`)) %>% 
#   View(.)

df %>% 
  dplyr::select(.,c(WaterbodyID,SurveyID,`Patch Area`)) %>% 
  distinct(.) %>% 
  group_by(WaterbodyID,SurveyID) %>% 
  summarise(tot_area_wb_yr=sum(`Patch Area`)) -> tot_area_wb_yr 

df <- left_join(df,tot_area_wb_yr)

# dftotcov_yr <- 
df %>% 
  dplyr::select(WaterbodyID, SurveyID, `AIH (ha)`,tot_area_wb_yr) %>%
  distinct(.) -> df_percWBYr


df_percWBYr$perc_cov <- (df_percWBYr$tot_area_wb_yr/df_percWBYr$`AIH (ha)`)*100

df_percWBYr %>% 
  ggplot(.,aes(x=SurveyID,y=perc_cov,group=WaterbodyID))+
  # geom_line()+
  geom_point()+
  geom_smooth(se = FALSE)+
  facet_wrap(~WaterbodyID)

# TIDY ####
detach("package:readxl", unload=TRUE)
detach("package:tidyverse", unload=TRUE)

rm(list = ls(pattern = "^df"))
rm(area_wb_Yr_patch,tot_area_wb_yr)
