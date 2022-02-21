
# Libraries
library(ggplot2)
library(ggthemes)
library(dplyr)

# Variables
partyColor <- c("#F97D09",  "#1e90ff")
titleColor <- "#EC111A"
displayResolution <- 600
lineColor <- "#758D99"

# set working directory and read the file
setwd("/Users/sanjaynoronha/Desktop/R")
df <- read.csv("Fig2_Democracy_Index_India.csv")

# print the file to see the contents
df

# ggplot the data
p <- ggplot(df, aes(x=year, y= rank)) +
  geom_rect(data = df, aes(xmin = year, xmax = year_end, fill = party), ymin =-Inf, ymax = Inf, alpha = 0.2) +
  geom_text(aes(label = rank), vjust = - 1.6) +
  geom_line(color = lineColor) +
  geom_point(size = 3.5, color = titleColor) +
  scale_y_reverse(limits = c(60, 20)) + #Ref: https://r-graphics.org/recipe-axes-reverse
  scale_x_continuous(  breaks    =c(2006, 2008, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)
                       , labels = c(2006, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 2021)) +
  scale_fill_manual(values = partyColor) + 

# this is our base theme
theme_clean() +
  
# customize the base theme
  labs(
    title = "India, Democracy Index Rank Over the Years"
    , subtitle = "India's rank among 167 countries. Data unavailable: 2007, 9"
    , caption = c("Instagram: @plotShaala", "Source: EIU" )
  ) +
  
  theme(
    plot.caption = element_text(hjust=c(1, 0))
    , plot.title = element_text(colour = titleColor)
  ) +
  
  theme(
    panel.grid.major.x =   element_blank()
    , panel.grid.major.y =  element_blank()
    , panel.grid.minor.x =  element_blank()
    , panel.grid.minor.y = element_blank()
  ) +
  
  theme(
    axis.ticks = element_blank()
  ) +
  
  theme(
    legend.title = element_blank()
    , legend.position="bottom"
    , legend.background = element_blank()
  ) 

# save plot in high resolution
ggsave(plot = p,  width = 5.6, height = 4.8 ,dpi = displayResolution, filename = "Fig2_Democracy_Index_India.png")


