# GKC figures port sampling data  
# 12-5-18

# K.Palof, katie.palof@alaska.gov


# Load -------------
source("./code/helper.R")

# Data --------------
# need harvest data to complete graphs like they are previously done.  Focus on percent recruit right now.
# dockside or port sampling data is from ALEX and is held in multiple files
gkc_portsamp <- read.csv('./data/dockside_11_18.csv')
head(gkc_portsamp)

#munipulation
# recruits on bottom
gkc_portsamp %>%
  mutate(prop_recruits = prop_recruits/100) %>%
  mutate(prop_post = 1 - prop_recruits, lb_recruit = prop_recruits*Sum.POUNDS. , 
         lb_post = prop_post*Sum.POUNDS.)%>%
  mutate(lb_other = ifelse(is.na(prop_recruits), Sum.POUNDS., 0 ))-> gkc_portsamp1

#bar graph
#east central 

gkc_portsamp1 %>%
  filter(I_FISHERY == "East Central GKC") -> ec_gkc
ec_gkc %>%
  gather(harvest, lbs, lb_recruit:lb_other) ->ec_gkc1

p1 <- ggplot(ec_gkc1, aes(x=YEAR, y = lbs, fill = harvest))+ 
  geom_bar(stat = 'identity', color = "black", width = 0.8) 

p1 <- p1 +  scale_fill_manual(values = c("gray", "white", "black"), 
                              labels = c("no data", "post recruits", "recruits")) +
  guides(fill=guide_legend(title = NULL))
p1 <- p1 + labs(title = "East Central", y = "Harvest (lb)") 
  

#munipulation
# recruits on bottom
gkc_portsamp %>%
  mutate(prop_recruits = prop_recruits/100) %>%
  mutate(prop_post = 1 - prop_recruits, lb_post = prop_post*Sum.POUNDS., 
         lb_recruit = prop_recruits*Sum.POUNDS.) %>%
  mutate(lb_other = ifelse(is.na(prop_recruits), Sum.POUNDS., 0 ))-> gkc_portsamp2

#bar graph
#east central 

gkc_portsamp2 %>%
  filter(I_FISHERY == "East Central GKC") -> ec_gkc
ec_gkc %>%
  gather(harvest, lbs, lb_post:lb_other) ->ec_gkc1

p1 <- ggplot(ec_gkc1, aes(x=YEAR, y = lbs, fill = harvest))+ 
  geom_bar(stat = 'identity', color = "black", width = 0.8) 

p1 <- p1 +  scale_fill_manual(values = c("gray", "white", "black"), 
                              labels = c("no data", "post recruits", "recruits")) +
  guides(fill=guide_legend(title = NULL))
p1 <- p1 + labs(title = "East Central", y = "Harvest (lb)") 

