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

# all data included
status %>% 
  left_join(total_bytrip) %>% 
  mutate(recruit_percent = n/total) %>% 
  filter(RECRUIT_STATUS == "Recruit") %>% 
  group_by(YEAR, SEASON, I_FISHERY) %>% 
  summarise(meanR = mean(recruit_percent, na.rm = TRUE), SD = sd(recruit_percent, na.rm = TRUE), 
            n = length(unique(TRIP_NO)))-> recruit_only

# those trips with less than 20 crab excluded 
status %>% 
  left_join(total_bytrip) %>% 
  mutate(recruit_percent = n/total) %>% 
  filter(RECRUIT_STATUS == "Recruit") %>% 
  filter(n >= 20) %>% 
  group_by(YEAR, SEASON, I_FISHERY) %>% 
  summarise(meanR = mean(recruit_percent, na.rm = TRUE), SD = sd(recruit_percent, na.rm = TRUE), 
            n = length(unique(TRIP_NO))) -> recruit_only_adj


# figures -----------
recruit_only %>% 
  filter(!(is.na(I_FISHERY))) %>% 
  ggplot(aes(YEAR, meanR)) + 
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = meanR - (SD*1.96), ymax = meanR + (SD*1.96)), 
                width =.4) +
  facet_wrap(~I_FISHERY)

recruit_only_adj %>% 
  filter(!(is.na(I_FISHERY))) %>% 
  ggplot(aes(YEAR, meanR)) + 
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = meanR - (SD*1.96), ymax = meanR + (SD*1.96)), 
                width =.4) +
  facet_wrap(~I_FISHERY)
  

