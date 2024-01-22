### saltmarshQuadratsFormatting.R ####

# initially, taxa % covers are recorded only when they are present.
# to calc mean density, we need values for when they are not present.

# PLAN:
# bring in data in long format
# widen data and fill gaps with 0s (using "values_fill = list(abundance = 0)")
# can then re-lengthen and calculate mean values by whatever columns are pertinent


### load packages
library("tidyverse")
library("readxl")

### import data
df0 <- as_tibble(read_xlsx(paste0("data/2023_Saltmarsh_Quads_Zonation_join.xlsx"),
                           sheet = "2023_Saltmarsh_Quads_Zonation_j"))

## convert to wide and fill in gaps with 0
dfw <- df0 %>% 
  dplyr::select(., -TARGET_FID) %>% # remove unneccessary variable which causes issues (each row has different value)
  distinct() %>% ## there are duplicated rows in the data. Retain unique rows only
  pivot_wider(., # widen data & fill in gaps with zero values
              names_from = Taxon_Name,
              values_from = Percentage,
              values_fill = 0)

write.csv(dfw, file="data/2023_Saltmarsh_Quads_Zonation_join_WIDE.csv",row.names = FALSE)
saveRDS(dfw, file="data/2023_Saltmarsh_Quads_Zonation_join_WIDE.Rdat")

## convert back to long
dfl <- dfw %>% 
  pivot_longer(cols = "Agrostis stolonifera":"Althaea officinalis",
               names_to="Taxon_Name,",
               values_to="Percentage")
write.csv(dfl, file="data/2023_Saltmarsh_Quads_Zonation_join_LONG.csv",row.names = FALSE)
saveRDS(dfl, file="data/2023_Saltmarsh_Quads_Zonation_join_LONG.Rdat")


# tidy up ####
detach("package:readxl", unload=TRUE)
detach("package:tidyverse", unload=TRUE)
rm(list = ls(pattern = "^df"))
