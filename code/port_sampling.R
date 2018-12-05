# GKC figures port sampling data  
# 12-5-18

# K.Palof, katie.palof@alaska.gov


# Load -------------
source("./code/helper.R")

# Data --------------
# need harvest data to complete graphs like they are previously done.  Focus on percent recruit right now.
# dockside or port sampling data is from ALEX and is held in multiple files
gkc_portsamp <- read_xlsx('./data/dockside_11_18.xlsx', sheet = "AlexData", skip = 13)
gkc_samp2 <- read_xlsx('./data/dockside_95_10.xlsx', sheet = "AlexData", skip = 13)
head(gkc_portsamp)


# clean -up ------------
gkc_samp2 %>% 
  select(YEAR, SEASON, PROJECT_CODE, PROJECT, TRIP_NO, ADFG_NO, SAMPLE_DATE, TICKET_NO, 
         I_FISHERY_CODE, I_FISHERY, DISTRICT, SUB_DISTRICT, SPECIMEN_NO, SPECIES_CODE, 
         SEX_CODE, RECRUIT_STATUS, LENGTH_MILLIMETERS, SHELL_CONDITION_CODE) -> gkc_samp2
gkc_portsamp %>% 
  select(YEAR, SEASON, PROJECT_CODE, PROJECT, TRIP_NO, ADFG_NO, SAMPLE_DATE, TICKET_NO, 
         I_FISHERY_CODE, I_FISHERY, DISTRICT, SUB_DISTRICT, SPECIMEN_NO, SPECIES_CODE, 
         SEX_CODE, RECRUIT_STATUS, LENGTH_MILLIMETERS, SHELL_CONDITION_CODE) %>% 
  bind_rows(gkc_samp2) -> gkc


# recruit status summary --------------
gkc %>% 
  group_by(YEAR, SEASON, I_FISHERY, TRIP_NO, RECRUIT_STATUS) %>% 
  summarise(n = n()) -> status

gkc %>% 
  group_by(YEAR, SEASON, I_FISHERY, TRIP_NO) %>% 
  summarise(total = n()) -> total_bytrip

status %>% 
  left_join(total_bytrip) %>% 
  mutate(recruit_percent = n/total) %>% 
  filter(RECRUIT_STATUS == "Recruit") -> recruit_only

recruit_only %>% 
  filter(n >= 20) -> recruit_only_adj


# figures -----------
recruit_only %>% 
  ggplot(aes(YEAR, recruit_percent)) + geom_point() +
  facet_wrap(~I_FISHERY)


  

