# NVZ_raw_trends.R ####
### this time, we are converting the results to consistent units!

## load packages
ld_pkgs <- c("tidyverse","ggplot2","tictoc")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)
tictoc::tic.clearlog()
tic("Import metadata and raw data")#start timer
### set metadata ####
cbPalette <- c("#000000","#0072B2","#009E73","#e79f00", "#9ad0f3", 
                        "#D55E00", "#CC79A7", "#F0E442")
ppi <- 300


theme_set(ggthemes::theme_few())

### load data
dfraw <- as_tibble(readxl::read_excel("data/All nutrients.xlsx",
                                      sheet="combinedNO3-N",
                                      guess_max = 500000))
dfraw %>% 
  janitor::clean_names(.) %>% 
  mutate(year = as.numeric(year)) %>% #change year to numeric
  select(.,-c(
    month,wb_distance,reg_distance,smpt_grid_ref, ###remove unnecessary and potentially confusing cols
    samp_id,samp_lab_ref_no,samp_sample_time,
    samp_notes,ngr_easting,ngr_northing
  )) -> df0 #clean variable names
## set factor levels
df0$Season <- factor(df0$season,levels=c("Winter","Spring","Summer","Autumn"))
toc(log = TRUE)

# Loop through unique WB_Names
for(name in unique(df0$water_body)) {
  tic("Faceted plots")
  sanitized_name <- gsub("/", "_", name)
  # Subset data for current WB_Name
  # wb_data <- subset(df0, water_body == "Mersey Mouth")
  wb_data <- df0 %>%
    filter(.,water_body == name) %>% 
    filter(.,nitrate_use <500)
  
### faceted version
  set.seed(pi);wb_data %>% 
  ggplot(.,aes(
    x=as.factor(year),
    y = nitrate_use
    )
    )+
  geom_boxplot(outlier.colour = NA)+
  geom_point(
             aes(colour=Season,
                 shape=Season),
             position = position_jitter(seed = 123),
             show.legend = FALSE,
             alpha = 0.4)+
  # geom_jitter(aes(
  #   colour=Season,
  #   shape=Season
  #   ),
  #   alpha=0.5)+
  facet_wrap(.~Season)+
  labs(
    y = bquote(NO[3]~(umol^-l)),
    title = unique(wb_data$water_body[1]),
    subtitle = "NO<sub>3</sub> concentrations"
    )+
  scale_colour_manual(values=cbPalette)+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle=3*90,hjust=-1,vjust=0.5),
    plot.subtitle = ggtext::element_markdown(),
    plot.title = element_text(face=2),
    #legend.position = element_blank(),
    strip.text = element_text(face=2)
  ) -> p
  # Export the plot to a file with the sanitized WB_Name value in the filename
  ggsave(paste("plots/ver4/NO3_box_facet_", sanitized_name, ".png", sep = ""),
         plot = p, width = 10, height = 6)
  toc(log = TRUE)
  flush.console()
  
  tic("Non-faceted plots")
  ## non-faceted version
  set.seed(pi);wb_data %>% 
    ggplot(.,aes(
      x=as.factor(year),
      y = nitrate_use
      )
      )+
    geom_boxplot(outlier.colour = NA)+
    geom_point(
    aes(colour=Season,
        shape=Season),
    position = position_jitter(seed = 123),
    alpha = 0.4)+
  labs(
    y = bquote(NO[3]~(umol^-l)),
    title = unique(wb_data$water_body[1]),
    subtitle = "NO<sub>3</sub> concentrations"
  )+
  scale_colour_manual(values=cbPalette)+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle=3*90,hjust=-1,vjust=0.5),
    plot.subtitle = ggtext::element_markdown(),
    plot.title = element_text(face=2),
    legend.title = element_text(face=2),
    strip.text = element_text(face=2)
  ) -> p
  # Export the plot to a file with the sanitized WB_Name value in the filename
  ggsave(paste("plots/ver4/NO3_box_", sanitized_name, ".png", sep = ""),
         plot = p, width = 10, height = 6)
  toc(log=TRUE)
}

(x <- unlist(tictoc::tic.log()))

### tidy up
rm(list=ls(pattern="^df"))
rm(p,wb_data,cbPalette,name,ppi,sanitized_name,x)

detach(package:tidyverse, unload = TRUE)
detach(package:ggplot2, unload = TRUE)
detach(package:tictoc, unload = TRUE)
