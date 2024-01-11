# nwSusSol.R
# plotting of suspended solids extracted from NW region

library(tidyverse) # data handling
library(ggplot2) # plots
library(rnrfa) # read/convert OSGB

theme_set(ggthemes::theme_few())

## data based on a WIMS extract:
# Date range: 01/01/2008 - 11/01/2024
# Region: NW
# Purpose Code: MS
# Sampling Point type (SPT_CODE): C1, C2, C3, C4, CA, CB, CC, CD, CE, CF, CG
# Northings <= 410000

df0 <- read.csv("data/NW_SusSol.csv", header = TRUE)

df <- as_tibble(df0)
df$SAMP_SAMPLE_DATE <- as.Date(df$SAMP_SAMPLE_DATE, format="%d/%m/%Y")

# Convert NGR to easting and northing
x <- osg_parse(df$SMPT_GRID_REF)
df$Eastings <- x$easting
df$Northings <- x$northing
rm(x)

df %>% 
  filter(Northings <= 410000) %>% 
  ggplot(., aes(x=SAMP_SAMPLE_DATE,y=MEAS_RESULT))+
  geom_line(show.legend = FALSE,
            aes(group=as.factor(SAMP_SMPT_USER_REFERENCE),
                col=as.factor(SAMP_SMPT_USER_REFERENCE)))+
  geom_point(show.legend = FALSE,
             aes(group=as.factor(SAMP_SMPT_USER_REFERENCE),
                 col=as.factor(SAMP_SMPT_USER_REFERENCE)))+
  geom_smooth(method="loess")+
  ylim(0,300)

## export data for exploration
write.csv(df %>% filter(Northings <= 410000),
          file="data/NW_SusSol_trim.csv",
          row.names = FALSE)
