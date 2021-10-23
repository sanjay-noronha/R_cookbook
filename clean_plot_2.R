# Reference : http://rstudio-pubs-static.s3.amazonaws.com/284329_c7e660636fec4a42a09eed968dc47f32.html

library(ggrepel) #Add point labels
library(ggplot2) #Main package for graph
library(ggthemes)#Themes for formating
# library(grid) #Add grid line
library(cowplot) #Add annotation

# set working directory and read the file
setwd("/Users/sanjaynoronha/Desktop")
EconomistData <- read.csv("EconomistData.csv")
head(EconomistData)

# rename the regions
EconomistData$Region <- factor(EconomistData$Region,
                               levels = c("EU W. Europe",
                                          "Americas",
                                          "Asia Pacific",
                                          "East EU Cemt Asia",
                                          "MENA",
                                          "SSA"),
                               labels = c("OECD",
                                          "Americas",
                                          "Asia &\nOceania",
                                          "Central &\nEastern Europe",
                                          "Middle East &\nnorth Africa",
                                          "Sub-Saharan\nAfrica"))

# lets begin plotting

graph1 <-  ggplot(EconomistData, aes (x=CPI, y=HDI))
#graph1 + geom_point(aes(color = Region))


# Change the point shape and color
g2 <- graph1 + geom_point(aes(color = Region),
                          shape=21, 
                          fill= "White",
                          size =3, 
                          stroke=1.5)

g2

# fit a log line
# remove the Confidence interval
g3 <- g2 + geom_smooth(aes(fill="red"),method = "lm", 
                       formula = y ~log(x),
                       se=FALSE, 
                       linetype=1 , 
                       color= "Red") 
g3

# addc country labels
g3 + geom_text(aes(label = Country))

# Choose the countries that need labeling
point_1 <- c( "Venezuela", "Iraq", "Myanmar", "Sudan",
              "Afghanistan", "Congo", "Greece", "Argentina", 
              "India", "Italy",
              "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
              "United States",  "Britain", "Barbados", "Norway", 
              "New Zealand", "Singapore")
point_2 <- c("Russia","Brazil","Spain","Germany", "Japan","China","South Africa")
point_3 <-  c( "Venezuela", "Iraq", "Myanmar", "Sudan",
               "Afghanistan", "Congo", "Greece", "Argentina", 
               "India", "Italy",
               "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
               "United States",  "Britain", "Barbados", "Norway", 
               "New Zealand", "Singapore","Russia","Brazil","Spain","Germany", "Japan","China","South Africa")


# add country labels
#g3 + geom_text(aes(label = point_1)) # This does not work
g3 + geom_text(data=EconomistData[EconomistData$Country %in% point_1, ], 
               aes(label=Country)) 
g3

# now we try a better way of labelling
g4 <- g3 + geom_text_repel(data=EconomistData[EconomistData$Country %in% point_1,],
                           aes(label=Country))

g4

g4 + geom_text_repel(data=EconomistData[EconomistData$Country %in% point_2,],
                     aes(label=Country))

g4

# but some countries are overlapping.
g5 <- g4 + theme_bw() +geom_text_repel(data=EconomistData[EconomistData$Country %in% point_2,],
                           aes(label=Country),
                           box.padding = unit(1.75, 'lines'))
g5

#It’s time to play around with the legend box and color.

g6 <- 
    g5 +  scale_color_manual( values = c("#23576E", "#099FDB", 
                                           "#29B00E", "#208F84", 
                                           "#F55840", "#924F3E")) + 
    scale_fill_manual(name='My Lines', values=c("red"),labels=c("R^2=52%"))

# Now we move the legend to the top
g6 + theme(legend.position="top")

# Now we want the legend in 1 line
g7 <- g6 + theme(legend.position="top",
                 legend.title = element_blank(),
                 legend.box = "horizontal" ,
                 legend.text=element_text(size=8.5)) +
  guides(col = guide_legend(nrow = 1))
ggdraw(g7)

# Only keeping major horizontal grid lines
g8 <- g7 + theme(panel.grid.minor = element_blank(), 
                 panel.grid.major = element_line(color = "gray50", size = 0.5),
                 panel.grid.major.x = element_blank(),
                 panel.background = element_blank(),
                 line = element_blank())
ggdraw(g8)

# Add a title
# Adding title is simple. The ggtitle will do the work just fine and we use theme to add some format.

g9 <- g8 + ggtitle("Corruption and human development\n") + 
  theme(plot.title = element_text(hjust = 0.15, 
                                  vjust=2.12, 
                                  colour="black",
                                  size = 14,
                                  face="bold"))
ggdraw(g9)

#After some intense google searching, I found a package called cowplot 
# and it has a feature add_sub that allows us to add a footnote directly into graph.

g10 <-  add_sub(g9,"Source:Transparency International; UN Human Development report",
                x = 0.07,
                hjust = 0,
                fontface = "plain",
                size= 10) 
ggdraw(g10)