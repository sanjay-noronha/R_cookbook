#Title of article = Long time coming. Joe Biden, selected events.

#===============
# References:
#===============
# Mostly the ggplot2 book by H.W.

library(ggplot2)
library(ggthemes)

# Main variables

# It's recommended to use a named vector to map dimension values with colours
discreteColor <- c("C" = "#98DAFF", "D" = "#00588D", "R" = "#A81829", "S" = "#5DA4DF", "VP" = "#00588D")


# set working directory and read the file
setwd("/Users/sanjaynoronha/Desktop/R")

df1 <- read.csv("economist_july4_20_df1.csv")
df2 <- read.csv("economist_july4_20_df2.csv")
df3 <- read.csv("economist_july4_20_df3.csv")



##########
# DATA VIZ
##########


p <- ggplot(df1) +
  geom_segment(aes(x = x, xend = x, y = 1, yend = y), alpha = 0.2) + # vertical lines
  geom_text( aes(x = x,  y= y, label = comment) , colour = "black", size = 3.0, fontface="bold",  family="sans", hjust = "inward") + # text against segments
  geom_rect(data = df2, aes(xmin = x, xmax = x + years, fill = partyCode), ymin = 0, ymax = 1.0, alpha = 0.5) + # lower rect
  geom_rect(data = df3, aes(xmin = x, xmax = x + years, fill = positionCode), ymin = 29, ymax = 30, alpha = 0.5) +
  scale_fill_manual(values = discreteColor) + # good use of named vectors
  scale_x_continuous(
      breaks = c(1968, 1970, 1975, 1980, 1985, 1990,1995, 2000, 2005, 2010, 2015, 2020), 
      labels = c(1968,   70,  75,    80,   85,   90,  95, 2000,   05,   10,   15,   20 )
      ) +
   ylim(NA, 31) + #changing the y limit so that I can move the top rectangular segment appropriately
  
  # THEME SECTION
  
  labs(
    x = NULL
    , y = NULL
    , title = "Long time coming"
    , subtitle = "Joe Biden, selected events"
    , caption = "Source: The Economist"
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
    , plot.subtitle = element_text(colour = "black", size = 12, family = "sans", margin = margin(b=14),hjust = 0, vjust = -3)
    , axis.text.y = element_blank()
    , axis.text.x = element_text(size = 12, hjust = 0, family = "sans")
    , plot.caption = element_text(hjust = 0, vjust = -3,  size = 9, family = "sans", face = "italic")
    
  ) 

p
  
  