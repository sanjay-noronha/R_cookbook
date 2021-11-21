#Title of article = advantage scholtz

#===============
# References:
#===============
# font, typeface, family
#https://www.howtogeek.com/325644/whats-the-difference-between-a-font-a-typeface-and-a-font-family/

# using differnce colors for the stacked bar chart
# https://r-charts.com/part-whole/stacked-bar-chart-ggplot2/

# annotate() vs. geom_text()
# https://www.r-graph-gallery.com/233-add-annotations-on-ggplot2-chart.html

library(ggplot2)

# Variables
partyOrder <- c("Other", "Die Linke", "AfD", "FDP", "The Greens", "CDU/CSU", "SPD")
partyColor <- c("#6F8793", "#A81829", "#98DAFF", "#FFCB4D", "#2E9284", "#3F5661", "#FF6B6C")

# set working directory and read the file
setwd("/Users/sanjaynoronha/Desktop/R")
df <- read.csv("Economist_Oct2_2021.csv")

# make  YEAR a discrete value
# This will make the y axis discrete
df$year  <- factor(df$year)

# make PARTY a discrete value
# also it is the levels order that controls the display order in the Stacked bar chart
# so am organising the levels accordingly
df$party  <- factor(
                df$party 
                ,levels =  partyOrder
            )


df_2021 <- filter(df, year == 2021)

# adding a cumulative total field
df_2021$cum_seats <-  cumsum(df_2021$seats)

# I wish to create a stacked bar chart 
# grouped and filled by  PARTY
p <- ggplot(df_2021, aes(seats, year,  fill = party)) +
  
  # GEOM SECTION
     geom_bar( stat = "identity", width = 0.13 ) +
     geom_text( data = filter(df_2021, !party %in% c("Die Linke","Other")), aes(x = cum_seats - (seats/2), y = 1 , label = party) , colour = "white", size = 4.0, fontface="bold", family="sans") +
     geom_text( data = filter(df_2021, !party %in% c("Die Linke","Other")), aes(x = cum_seats - (seats/2), y = 0.97 , label = seats) , colour = "white", size = 3.0, fontface="bold",  family="sans") +
     scale_fill_manual(values = partyColor) + # add colours
     # add the line and text for "Die Linke"
     geom_text( data = filter(df_2021, party == "Die Linke"), aes(x = cum_seats - (seats/2), y = 1.15 , label = party) , colour = "black", size = 3.0, fontface="bold",  family="sans") +
     geom_segment(aes(x = cum_seats - (seats / 2), y = 1, xend = cum_seats - (seats / 2), yend = 1.14), data = filter(df_2021, party == "Die Linke")) +
     # add the line and text for "Other"
     geom_text( data = filter(df_2021, party == "Other"), aes(x = cum_seats - (seats/2), y = 1.125 , label = party) , colour = "black", size = 3.0, fontface="bold",  family="sans") +
     geom_segment(aes(x = cum_seats - (seats / 2), y = 1, xend = cum_seats - (seats / 2), yend = 1.1), data = filter(df_2021, party == "Other")) +
  
  # THEME SECTION
  
  labs(
    x = NULL
    , y = NULL
    , title = "Black knocked back"
    , subtitle = "Germany, federal election results, seats won"
    , caption = "Source: Germany federal returning officer"
  ) +
  
  theme_economist() +
  
  theme(
     panel.grid.major.x =   element_blank()
    , panel.grid.major.y =  element_blank()
    , panel.grid.minor.x =  element_blank()
    , panel.grid.minor.y = element_blank()
  ) +
  
  theme(
    legend.position = "none"
    , axis.ticks = element_blank()
    , axis.line.x = element_blank()
    , axis.line.y = element_blank()
  ) +
  
  theme(
    plot.title = element_text(colour="black", size = 16 ,family = "sans", face="bold")
    , plot.subtitle = element_text(colour = "black", size = 16, family = "sans", margin = margin(b=14),hjust = 0 )
    , axis.text.y = element_text(size = 12, hjust = 0, family = "sans")
    , axis.text.x = element_blank()
    , plot.caption = element_text(hjust = 0, size = 12, family = "sans")
    
  ) 

p

ggsave(plot = p, width = 2400, height = 1800, units = "px", dpi = 300, filename = "advantage_scholtz.png")
