
# Notes:
# 1. Quartiles are also quantiles; they divide the distribution into four equal parts.
# 2. Good example - quantile(x, probs = seq(0, 1, 1/4))     # using quantile function to calculate quartiles

# Exported image details
#------------------------
# Output - png
# Dimensions - w x h = 300 x 416


#References:
#------------------------

# Understanding the box plot
# https://towardsdatascience.com/understanding-boxplots-5e2df7bcbd51

# The viridus library
# https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
  
# R graph gallery
# https://www.r-graph-gallery.com/violin_and_boxplot_ggplot2.html


library(ggplot2)
library(ggthemes)
library(dplyr)
library(viridis) ######### NOTE ##################

# set working directory 
setwd("/Users/sanjaynoronha/Desktop/R")

#  runif generates random deviates
tmp1 <- runif(1000, 1, 100)
tmp2 <- runif(20, 70000, 100000)

a15 <- c(tmp1, tmp2) # join both vectors
subPool <- rep("A", length(a15)) # repeat a value 
df <- data.frame(a15 = a15, subPool = subPool) # build the df combining the 2

# lets get the summary of our data
dim(df)
summary(df$a15)

###################
# DATA CLEANING
###################

# What is 2 * IQR
twoIQR <- 2 * IQR(df$a15)
twoIQR

# What is the 3rd quartile
thirdQuartile <- quantile(df$a15, 3/4)
thirdQuartile

# given the outliers
# Getting all values less than Q3 + (2 X IQR)
twoIQRDf <- df %>% filter(df$a15 < (thirdQuartile + twoIQR))
dim(twoIQRDf)
summary(twoIQRDf$a15)

# Lets also look at the outliers even though we are discarding them in the graph
# given the outliers
# Getting all values less than Q3 + (2 X IQR)
outliersDf <- df %>% filter(df$a15 > (thirdQuartile + twoIQR))
outliersDf

# median value
median(twoIQRDf$a15)

#clean up
rm(tmp1)
rm(tmp2)
rm(a15)
rm(subPool)
rm(twoIQR)
rm(df)

###################
# VIZ
###################
p <-  ggplot(data = twoIQRDf , aes(x=subPool, y=a15, fill=subPool)) +
  geom_violin(width=0.5) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) + # specify color here
  scale_fill_viridis(discrete = TRUE) +
  geom_text(data = twoIQRDf, aes(x = 1.14 , y = median(a15), label = "M"), colour = "white", size = 4.0) +


#################
## THEME
#################

labs(
  x = NULL
  , y = NULL
  , title = ""
  , subtitle = ""
  , caption = "M = median"
) +
  
  theme_economist() +
  
  theme( # grids
    panel.grid.major.x =   element_blank()
    , panel.grid.minor.x =  element_blank()
    , panel.grid.major.y =   element_line( color = "grey18", size = 0.1)
    , panel.grid.minor.y = element_blank()
  ) +
  
  theme( # axes
    axis.ticks = element_blank()
    , axis.line.x = element_line( color = "black", size = 0.3)
    , axis.line.y = element_line( color = "black", size = 0.3)
  ) +
  
  theme(
    legend.position =  "none"
  )
  
p
