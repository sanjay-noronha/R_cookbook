#######################################################
# Reference :https://rpubs.com/tangerine/economist-plot
#
# Learnings:
# ungroup(), rescale, replace_na(),%in%,  case_when(), labs()
#######################################################

library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
library(ggthemes)

# set working directory and read the file
setwd("/Users/sanjaynoronha/Desktop")
df <- read.csv("globalterrorismdb_0221dist.csv")

# we will only take  subset of colummns from this df
# iyear which contains the year in which the incident occurred.
# country_txt which identifies the country or location where the incident occurred.
# nkill : The number of total confirmed fatalities for the incident.
# nkillter : number of perpetrator fatalities
df_final <-  df %>% 
             select(iyear, country_txt, nkill, nkillter) %>%  
             filter(between(iyear, 2000, 2014)) %>%
             mutate(
              region = case_when(
                country_txt == "Iraq" ~ "Iraq",
                country_txt == "Nigeria" ~ "Nigeria",
                country_txt  %in% c("Syria","Afghanistan","Pakistan") ~ "Syria, Afghanistan & Pakistan",
                country_txt %in% c("Andorra", "Argentina", "Australia", "Austria", "Belgium", "Canada", "Chile", "Croatia", 
                                   "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", 
                                   "Iceland", "Ireland", "Israel", "Italy", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", 
                                   "Malta", "Monaco", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "San Marino", 
                                   "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom", "United States", "Vatican City") 
                ~ "Western Countries",
                TRUE ~ "Rest of the World", # Not this
            )) %>%
            # handle NA values
            mutate(
              nkill = replace_na(nkill, 0),
              nkillter = replace_na(nkillter,0),
              ntotal = nkill+nkillter
            ) %>%
            group_by(iyear, region) %>%
            summarise(deaths = sum(ntotal)/1000) %>%  # this works in conjunction with the above group by clause
            ungroup() %>%
            mutate(deaths = rescale(deaths, to = c(0,35)))  # how cool is this !
           # mutate(region = as.factor(region))

      
      total <- df_final %>% 
        mutate(region = "Rest of the World") %>% 
        group_by(iyear, region) %>% 
        summarise(deaths = sum(deaths)) %>% 
        ungroup() 
      
      
        
      df_final <-  df_final %>%
                  filter(region != "Rest of the World")


#######################################################
# Data Viz begins
#######################################################


      
      p <- ggplot(df_final, aes(iyear, deaths))+
        geom_area(data = total, aes(fill = region))+
        geom_line(data = total, aes(iyear, deaths))+
        geom_area(aes(fill = region),color = "white")+
        labs(title = "Global deaths from terrorism",
             subtitle = "'000",
             caption = "Source: START, IEP",
             x = NULL,
             y = NULL) +

        scale_x_continuous(breaks = seq(2000,2014),
                           labels = c(2000, paste0(0, seq(1,9)), seq(10,14)),
                           expand = expand_scale(mult = c(0.03,0.02)))+
        scale_y_continuous(position = "right",
                           breaks = seq(0,125,125/7),
                           labels = paste(seq(0,35,5)),
                           expand = expand_scale(mult = c(0,0.1)))+
        scale_fill_manual(values = c("#7b2713","#eb9e84","#00a4dc","#f15a40","#00526d"))
      
      p
