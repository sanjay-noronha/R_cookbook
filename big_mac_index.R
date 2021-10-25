# Reference : https://rpubs.com/ysittaa/BMIsitta

library(ggplot2) #Main package for graph
library(ggthemes)#Themes for formating

# set working directory and read the file
setwd("/Users/sanjaynoronha/Desktop")
bmi <- read.csv("economist_big_mac.csv")
dim(bmi)

# There are 173 countries
# plotting a sub-set of these i.e. 16

countries <- c("Norway","Switzerland","Brazil","Canada","Euro area†","Britain","United States#","Australia","Turkey","Japan","China$","Russia","Egypt","Indonesia","South Africa","India**")
bmi1 <- bmi[bmi$Country %in% countries,c(1,4)]# pick up the 1st and 4th column only
bmi2 <- as.data.frame(bmi1)
bmi2$percentage <- ((bmi2$dollar_price/4.62) - 1)*100 # some sort of feature scaling
bmi2$ordercount <- factor(bmi2$Country, levels = bmi2$Country[order(bmi2$dollar_price)])
str(bmi2)

library(ggplot2)
p1 <- ggplot(bmi2, aes(x = ordercount, y = percentage)) + 
  geom_hline(yintercept = 0, linetype=1, color = "red", size=1) +
  geom_hline(yintercept = c(-80, -40, 40, 80), linetype = 1, color = "white", size = 1) +
  geom_bar(stat = "identity", width = .7, fill = "#01516c") +
  theme_bw()

p1 

# now we flip the coordinates
p2 <- p1 + coord_flip()
p2



# create a theme
p3 <- p2 + theme(panel.grid.minor = element_blank(), 
                 panel.grid.major = element_line(color = "white", size = 1),
                 panel.grid.major.y = element_blank(),
                 panel.background = element_blank(),
                 line = element_blank()) + 
                ggtitle ("The Big Mac Index\n") + 
                labs(subtitle = "local currency under (-)/over (+) valuation\nagainst the dollar, %") + 
               labs(caption = "*At market exchange rates(Jan 22nd 2014)\nWeighted average of member countries
               \n#Average of four cities     $Average of five cities\nSources: McDonald's; The Economist   
              **Maharaja Mac") 
p3

# economist theme
p5 <- p3 + theme_economist() + 
  scale_color_economist() + scale_y_discrete(expand = c(0,8), position = "right", breaks = seq(-80,80,40), lim = c(-80,-40,0,40,80)) 
p5

# add annotations
p8 <- p5 + annotate("label", x = bmi2$ordercount, y = max(bmi2$percentage)+25, 
                    label = bmi2$dollar_price, col = "black")
p8