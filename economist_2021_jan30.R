#  Title of article = Stockholm syndrome

library(ggplot2)
library(ggthemes)
library(cowplot) #Add annotation
library(dplyr)

# set working directory and read the file
setwd("/Users/sanjaynoronha/Desktop/R")(df)
df <- read.csv("economist_2021_jan30.csv")

# data cleaning

# to maintain the sorting of records as in source file, make company an ordered factor
# also had to reverse order of rows since it was showing last row first in the geom_bar()
df <- df[rev(rownames(df)), ]
df$company <- factor(df$company, levels = df$company)

# add a temporary column for the top annotation
df <- mutate(df, tmp_counter = 1:nrow(df))

# data viz
p <- 
##################  data component  
  ggplot(df, aes(x = revenue_perc, y = company)) +
  geom_bar(stat = "identity", fill = "#006BA2",  width = 0.6) + # do not run the default stat which counts the records
  annotate("label", x = max(df$revenue_perc) + 1.5 , y = df$company, label = df$revenue_amount, col = "black", size = 5, fill = "#BFD8E5") +
  
  annotate("text", x = max(df$revenue_perc) + 1.5 , y =  max(df$tmp_counter) + 0.5,  label = "$bn", col = "black", size = 4.5, fill = "#BFD8E5") +
##################  non data component  
  labs(
      x = NULL
      , y = NULL
      , title = "There be dragons"
      , subtitle = "OMX Stockholm 30 index, selected members"
      , caption = "Sources: Bloomberg; company reports"
      ) +
 
   scale_x_continuous(position = "top")  + # scales control mapping from data to aesthetic
  
  theme_economist() +
  
  theme(
      panel.grid.major.x =   element_line( color = "grey18", size = 0.2)
      , panel.grid.minor.x =  element_blank()
      , panel.grid.major.y =  element_blank()
      , panel.grid.minor.y = element_blank()
  ) +
  
  theme(
      axis.ticks = element_blank()
      , axis.line.x = element_blank()
      , axis.line.y = element_blank()
  ) +

  theme(
      plot.title = element_text(colour="black", size = 20 ,family = "sans", face="bold", margin = margin(b=10))
      , plot.subtitle = element_text(colour = "black", size = 16, family = "sans", margin = margin(b=14),hjust = 0 )
      , axis.text.y = element_text(size = 14, hjust = 0, family = "sans")
      , axis.text.x = element_text(size = 14, hjust = 0, family = "sans")
      , plot.caption = element_text(hjust = 0, size = 12, family = "sans")

  ) 
 
   


p

