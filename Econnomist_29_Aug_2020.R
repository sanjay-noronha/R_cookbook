# To do
# make fonts large
# make points larger
# add another caption to the right
# align 0 with main margin
# move title, subtitle and caption in line with the y axis labels


# References

# align ticks to x, y axes - force origin to start at 0
# https://stackoverflow.com/questions/13701347/force-the-origin-to-start-at-0

# Economist article
# https://www.economist.com/britain/2020/08/27/generation-rent-grows-up

# lollipop chart
# https://www.r-graph-gallery.com/303-lollipop-plot-with-2-values.html

# formatting the legend
# http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/#changing-the-position-of-the-legend

# changing default colours
# https://data-se.netlify.app/2018/12/12/changing-the-default-color-scheme-in-ggplot2/

# Library
library(ggplot2)
library(dplyr)
library(ggthemes)

# set working directory and read the file
setwd("/Users/sanjaynoronha/Desktop/R")
df <- read.csv("Economist_Aug_29_2020.csv")

#################
# DATA CLEANING
#################

yearColors <- c("#3EBCD2", "#A81829")

# changing the default colours
scale_colour_discrete <- function(...) {
  scale_colour_manual(..., values = yearColors)
}

#Getting the 2009 and 2019 values 
df_2009 <- filter(df, year == 2009)
df_2019 <- filter(df, year == 2019)

# taking 2009 as baseline, renaming the -value- column
df_2009 <- df_2009 %>%
rename(value_2009 = value)

# adding the 2019 column
df_2009$value_2019 <- df_2019$value  

#removing unwanted variable
df <- df_2009
rm(df_2009)
rm(df_2019)

#selecting a subset of values
df <- df %>% select(housing_type, value_2009, value_2019)


#################
## DATA VIZ
#################
ggplot(df) +
  geom_segment( aes( y = housing_type, yend = housing_type, x = value_2009, xend = value_2019), color="black") +
  geom_point( aes(y = housing_type, x = value_2009, colour = "2009*"),  size=4.5 ) +
  geom_point( aes(y = housing_type, x = value_2019, colour = "2019*"),  size=4.5 ) + 
  scale_x_continuous(limits = c(0, 250), position = "top", expand = c(0, 0)) +


#################
## THEME
#################
  labs(
    x = NULL
    , y = NULL
    , title = "Home economics"
    , subtitle = "England, average weekly housing costs, £"
    , caption = c("Source: English housing survey", "* Years ending March") # multiple captions
  ) +
  
  theme_economist() +
  
  theme( # grids
    panel.grid.major.x =   element_line( color = "grey18", size = 0.1)
    , panel.grid.minor.x =  element_blank()
    , panel.grid.major.y =   element_line( color = "grey18", size = 0.1)
    , panel.grid.minor.y = element_blank()
  ) +
  
  theme( # axes
    axis.ticks = element_blank()
    , axis.line.x = element_blank()
    , axis.line.y = element_line( color = "black", size = 0.3)
  ) +
  
  theme(
    plot.title = element_text(colour="black", size = 22 ,family = "sans", face="bold", margin = margin(b = 10) ,hjust = 0)
    , plot.subtitle = element_text(colour = "black", size = 18, family = "sans", margin = margin(b = 14) ,hjust = 0)
    , axis.text.y = element_text(size = 16, hjust = 0, family = "sans")
    , axis.text.x = element_text(size = 16, hjust = 0, family = "sans")
    , plot.caption = element_text(size = 14, family = "sans",hjust=c( 0, 1))
  ) +
  
  theme(
    legend.title = element_blank() # No legend title
    , legend.position="top"
    , legend.justification=c(0,0)
  ) 
  

ggsave(plot = p, width = 2400, height = 1800, units = "px", dpi = 300, filename = "advantage_scholtz.png")


