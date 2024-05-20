### ExtractDataFromWims.R
## load packages
ld_pkgs <- c("dplyr","arrow","lubridate","tictoc", "trend","ggplot2")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

theme_set(ggthemes::theme_few())

# TIME PERIODS
# 1: pre-1990
# 2: 1/1/1990 - 31/12/2019
# 3: 1/1/2020 onwards (this is what gets updated periodically)

# Notes:
# 1. you don't need to select time periods in your query but it helps speed things up if you know you don't need
# data e.g. you can easily exclude pre-1990 data
# 2. WIMS det code list https://midas.prodds.ntnl/helpfiles/help_ncs.html
# 3. I just use the tictoc package to time queries but it's not essential to do this

tic()#start timer

# main location for TraC data
# this is a partitioned Parquet file: info on Parquet and partitioning here: https://r4ds.hadley.nz/arrow.html
wims.p <- open_dataset('C:/Users/cc000046/OneDrive - Defra/Desktop/git/WIMS_est_sea')

# check which regions are there
wims.p |> select(wims_region) |> collect() |> distinct(wims_region) 

# get data from 1990-current
# Choose det codes - uncomment to select variable
N_dets <- c(
  # 5282, #Ammonia : Steam distilled : Dry Wt (as N)_mg/kg
  # 3656, #Ammonia Unionised : Saline as N_mg/l
  # 9150, #Ammonia un-ionised : Saline PML as N_mg/l
  # 6496, #Ammonia un-ionised : at Fish Gill as N_mg/l
  # 119, #Ammonia un-ionised as N_mg/l
  # 6382, #Ammonia un-ionised, Freshwater as N_mg/l
  # 1077, #Ammoniacal Nitrogen : Dry Wt as N_mg/kg
  # 1167, #Ammoniacal Nitrogen : Dry Wt as N_g/kg
  # 7677, #Ammoniacal Nitrogen : Dry Wt as N_%
  # 7004, #Ammoniacal Nitrogen : Dynamic load as N_t
  # 7404, #Ammoniacal Nitrogen : Gas as N_mg/kg
  # 9259, #Ammoniacal Nitrogen : Grant YSI 3800 : In Situ_mg/l
  # 9260, #Ammoniacal Nitrogen : Grant YSI 55 : In Situ_mg/l
  # 3970, #Ammoniacal Nitrogen : In Situ_mg/l
  # 4027, #Ammoniacal Nitrogen : In Situ Palin test_mg/l
  # 7532, #Ammoniacal Nitrogen : Load_t/d
  # 3561, #Ammoniacal Nitrogen : Load_MG/S
  # 6003, #Ammoniacal Nitrogen : Load as N_t/ANN
  # 8508, #Ammoniacal Nitrogen : Qualitative_PRES/NF
  # 110, #Ammoniacal Nitrogen : Total load_kg/d
  # 9720, #Ammoniacal Nitrogen : Total moving 7 day emissions as N_t/ANN
  # 7403, #Ammoniacal Nitrogen : Wet Wt as N_mg/kg
  # 9397, #Ammoniacal Nitrogen as N_ug/l
  # 6021, #Ammoniacal Nitrogen as N_umol/l
  # 5092, #Ammoniacal Nitrogen as N_kg/Wk
  # 111, #Ammoniacal Nitrogen as N_mg/l
  # 9875, #Ammoniacal Nitrogen as NH4_mg/l
  # 5506, #Ammoniacal Nitrogen, 2M KCl extractable : Dry Wt_mg/kg
  # 9057, #Ammoniacal Nitrogen, Filtered : Low level as N_ug/l
  # 3403, #Ammoniacal Nitrogen, Filtered as N_ug/l
  # 9993, #Ammoniacal Nitrogen, Filtered as N_mg/l
  # 7186, #Ammoniacal Nitrogen, GF/C Filtered as N_mg/l
  # 8078, #Ammoniacal Nitrogen, Leachable : Dry Wt as N_mg/kg
  # 8729, #Ammoniacal Nitrogen, Leachable as N_mg/l
  # 6872, #Ammonium : In Situ as NH4_mg/l
  # 8153, #Ammonium as NH4_mg/l
  # 4837, #Carbon/Nitrogen Ratio_UNITLESS
  # 740, #Hydrazine as N_mg/l
  # 8140, #Metals : Total ( As Ni Pb Sn Zn )_ug/l
  # 5507, #Nitrate (2M KCl extractable) : Dry Wt_mg/kg
  # 7194, #Nitrate + Chloride_mg/l
  # 8601, #Nitrate : Dry Wt as N_mg/kg
  # 6004, #Nitrate : Load as N_t/ANN
  # 8501, #Nitrate : Qualitative_PRES/NF
  # 9232, #Nitrate : Total load as NO3_kg/hr
  # 6490, #Nitrate : Total moving 7 day emissions as N_t/ANN
  # 7633, #Nitrate : Wet Wt as N_mg/kg
  # 6022, #Nitrate as N_umol/l
  # 9395, #Nitrate as N_ug/l
  # 117, #Nitrate as N_mg/l
  # 6380, #Nitrate as N_mg/l
  # 9880, #Nitrate as NO3_mg/l
  9853 #Nitrate, Filtered as N_mg/l
  # 9055, #Nitrate, Filtered as N_ug/l
  # 5567, #Nitrate, Filtered as NO3_ug/l
  # 9607, #Nitrate, GF/C Filtered as N_mg/l
  # 8070, #Nitrate, Leachable : Dry Wt as N_mg/kg
  # 5280, #Nitrate, Leachable as N_mg/l
  # 7634, #Nitrite : Dry Wt as N_mg/kg
  # 6005, #Nitrite : Load as N_t/ANN
  # 8502, #Nitrite : Qualitative_PRES/NF
  # 9886, #Nitrite : Total moving 7 day emissions as N_t/ANN
  # 7635, #Nitrite : Wet Wt as N_mg/kg
  # 118, #Nitrite as N_mg/l
  # 9396, #Nitrite as N_ug/l
  # 6023, #Nitrite as N_umol/l
  # 6381, #Nitrite as N_mg/l
  # 3402, #Nitrite as N_ug/l
  # 9879, #Nitrite as NO2_mg/l
  # 5508, #Nitrite, 2M KCl extractable : Dry Wt_mg/kg
  # 9056, #Nitrite, Filtered as N_ug/l
  # 3404, #Nitrite, Filtered as N_ug/l
  # 6485, #Nitrite, Filtered as N_mg/l
  # 7891, #Nitrite, GF/C Filtered as N_mg/l
  # 8079, #Nitrite, Leachable : Dry Wt as N_mg/kg
  # 5279, #Nitrite, Leachable as N_mg/l
  # 1164, #Nitrogen : Dry Wt % as N_%
  # 1188, #Nitrogen : Dry Wt as N_mg/kg
  # 7046, #Nitrogen : Wet Wt as N_mg/kg
  # 830, #Nitrogen : as % V/V N2_%
  # 4920, #Nitrogen Dioxide_mg/m3
  # 4915, #Nitrogen Oxide_mg/m3
  # 9181, #Nitrogen Oxides : Load as NO2 (Except N2O)_t/Qtr
  # 7371, #Nitrogen as N_ppm
  # 112, #Nitrogen, Albuminoid as N_mg/l
  # 3968, #Nitrogen, Digested as N_mg/l
  # 6832, #Nitrogen, Digested as N_ug/l
  # 3141, #Nitrogen, Dissolved Inorganic : Load_kg/ANN
  # 4925, #Nitrogen, Dissolved Inorganic : as N_mg/l
  # 7298, #Nitrogen, Kjeldahl : (Calculated) as N_mg/l
  # 6372, #Nitrogen, Kjeldahl : Dry Wt % as N_%
  # 1168, #Nitrogen, Kjeldahl : Dry Wt as N_g/kg
  # 825, #Nitrogen, Kjeldahl : Dry Wt as N_mg/kg
  # 114, #Nitrogen, Kjeldahl as N_mg/l
  # 7405, #Nitrogen, Kjeldahl as N_ug/l
  # 7678, #Nitrogen, Kjeldahl, settled as N_mg/l
  # 7751, #Nitrogen, Leachable : Air Dried Solids as N_mg/l
  # 7300, #Nitrogen, Organic : Dry Wt as N_mg/kg
  # 113, #Nitrogen, Organic as N_mg/l
  # 115, #Nitrogen, Organic, Filtered as N_mg/l
  # 6729, #Nitrogen, Particulate as N_mg/l
  # 5013, #Nitrogen, Sat CaSO4 extractable : Dry Wt_mg/l
  # 5014, #Nitrogen, Sat CaSO4 extractable : Dry Wt_mg/kg
  # 7636, #Nitrogen, Total : % on solids_%
  # 9893, #Nitrogen, Total : Total moving 7 day emissions as N_t/ANN
  # 9177, #Nitrogen, Total Inorganic : (Calculated)_ug/l
  # 3683, #Nitrogen, Total Inorganic : (Calculated)_mg/l
  # 8071, #Nitrogen, Total Inorganic, Leachable : Dry Wt as N_mg/kg
  # 7406, #Nitrogen, Total Organic as N_ug/l
  # 6978, #Nitrogen, Total Oxidisable as N_mg/l
  # 5509, #Nitrogen, Total Oxidised (2M KCl extractable) : Dry Wt_mg/kg
  # 120, #Nitrogen, Total Oxidised : Dry Wt as N_mg/kg
  # 7012, #Nitrogen, Total Oxidised : Dynamic load as N_t
  # 3508, #Nitrogen, Total Oxidised : Load as N_MG/S
  # 7350, #Nitrogen, Total Oxidised : Wet Wt as N_mg/kg
  # 9394, #Nitrogen, Total Oxidised as N_ug/l
  # 116, #Nitrogen, Total Oxidised as N_mg/l
  # 9881, #Nitrogen, Total Oxidised as NO3_mg/l
  # 9943, #Nitrogen, Total Oxidised, Filtered as N_mg/l
  # 9065, #Nitrogen, Total Oxidised, Filtered as N_ug/l
  # 7187, #Nitrogen, Total Oxidised, GF/C Filtered as N_mg/l
  # 8731, #Nitrogen, Total Oxidised, Leachable_mg/l
  # 8076, #Nitrogen, Total Oxidised, Leachable : Dry Wt as N_mg/kg
  # 9194, #Nitrogen, Total as N_ug/l
  # 9686, #Nitrogen, Total as N_mg/l
  # 6866, #Nitrogen, Total, Filtered_mg/l
  # 5982, #Pentaerythritol tetranitrate : Dry Wt :- {PETN}_mg/kg
  # 5984, #Pentaerythritol tetranitrate :- {PETN}_ug/l
  # 5983, #Pentaerythritol tetranitrate, Leachable :- {PETN}_ug/l
  # 8782, #Suite Indicator : NITRATE_UNITLESS
  # 7062, #WQMS : Nitrate electrode High C.O.P._%
  # 7063, #WQMS : Nitrate electrode Low C.O.P._%
  # 7067 #WQMS : Water Bath Temperature, Nitrate electrode_CEL
)

temp <- wims.p |> filter(time_period != 1 & #excludes pre-1990 data
                           MEAS_DETERMINAND_CODE %in% N_dets) |>
  collect()


### append site info
load("O:/National_Hydroecology/WIMS_cache/all.wims.sites.rda")

# want to retain estuarine and coastal water samples
all.wims.sites %>% 
  filter(grepl("^C",SMPT_TYPE)) %>% 
  write.csv(.,file="data/marineSites.csv",row.names = FALSE)

### join WB info to WIMS data
df <- left_join(temp, all.wims.sites, by=c("SAMP_SMPT_USER_REFERENCE" = "SMPT_USER_REFERENCE"))

df %>% 
  dplyr::select(.,notation:res) %>%
  distinct() %>% write.csv(.,file="data/marineNitrogenDataSites.csv",row.names = FALSE)

write.csv(df, file="data/marineNitrogenData.csv",row.names = FALSE)
toc()

table(df$SAMP_PURPOSE_CODE)


marineSites <- readxl::read_excel("data/MarineSamplingSitesJOIN.xlsx",guess_max = 10000)

#remove first 3 characters from string
marineSites$SAMP_SMPT_USER_REFERENCE <- substr(marineSites$notation,3+1,nchar(marineSites$notation))

df0 <- left_join(temp, marineSites, by="SAMP_SMPT_USER_REFERENCE")

## order by, Region, WB, SMP, Date
df0 %>% 
  arrange(.,wims_regio,WATER_BO_1, SAMP_SMPT_USER_REFERENCE, DATE_TIME) %>% 
  dplyr::select(.,res, WATER_BO_1, SAMP_SMPT_USER_REFERENCE,SMPT_SHORT,
                DATE_TIME,DETE_SHORT_DESC,UNIT_SHORT_DESC,MEAS_SIGN,MEAS_RESULT,
                everything()) -> df_out

### Cut out pre-2000 data
df_out %>% 
  filter(.,DATE_TIME >= as.POSIXct("2000-01-01")) %>% #keep only 2000+ data
  filter(.,SAMP_PURPOSE_CODE %in% c("MN", #remove unwanted sample purposes
                                    "MP",
                                    "MS",
                                    "MU"
                                    )) %>% 
  filter(.,!is.na(WATER_BO_1)) %>% 
  ### if "Less than", divide value by 2
  mutate(Meas_USE = ifelse(MEAS_SIGN == "<", MEAS_RESULT/2,MEAS_RESULT)) %>% 
  group_by(WATER_BO_1,lubridate::year(DATE_TIME)) %>% 
  rename(year="lubridate::year(DATE_TIME)") %>% 
    ## calculate annual mean nitrate
  mutate(mean_Nitrate = mean(MEAS_RESULT)) %>% 
  ungroup() %>% 
  dplyr::select(.,
                -c(
                  MEAS_SIGN,
                  MEAS_RESULT,
                  SAMP_SMPT_USER_REFERENCE,
                  SMPT_SHORT,
                  DATE_TIME,
                  SAMP_ID
                  )) %>% 
  distinct() %>%
  ##remove WBs with <3 years of data
  group_by(WATER_BO_1) %>% 
  filter(n() >= 3) %>% 
  dplyr::select(res,WATER_BO_1,year,mean_Nitrate,everything()) -> df_use#%>% View(.)

### TO DO ####
# Check above data prior to analysis


#### run trend analysis on df_out data
## remove data pre-2000
## remove unwanted purpose codes: use only: c("MN", "MP", "MS", "MU")
