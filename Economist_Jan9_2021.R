# Notes
##########################################################
#  Does not use ggplot2 hence not sure if i can annotate, 
#  add themes etc
##########################################################

# https://www.sankey-diagrams.com/

# https://cran.r-project.org/web/packages/networkD3/networkD3.pdf
# Creates 'D3' 'JavaScript' network, tree, dendrogram, and Sankey  graphs from 'R'

# Good article and example
# https://towardsdatascience.com/using-networkd3-in-r-to-create-simple-and-clear-sankey-diagrams-48f8ba8a4ace

##########################################################

# Library
library(networkD3)
library(dplyr)

# set working directory and read the file
setwd("/Users/sanjaynoronha/Desktop/R")
df <- read.csv("Economist_Jan9_2021.csv")


# make the target notes unique from the source
df <- mutate(df, target = paste(target, " "))


# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(df$source), 
         as.character(df$target)) %>% unique()
)



# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. 
#So we need to reformat it.
df$IDsource <- match(df$source, nodes$name)-1 
df$IDtarget <- match(df$target, nodes$name)-1

nodes
df

# Color
# prepare colour scale
ColourScal ='d3.scaleOrdinal() .range([
    "#7BBFFC","#006BA2","#C7303C",
    "#78405F","#98DAFF","#B4BA39",
    "#EBB434","#758D99"])'

# Make the Network
p <- sankeyNetwork(Links = df, Nodes = nodes, 
                   Source = "IDsource", Target = "IDtarget",
                   Value = "target_value", NodeID = "name", 
                   LinkGroup = "source" , 
                   sinksRight=FALSE , nodeWidth=5, 
                   fontSize=20, nodePadding=20, colourScale=ColourScal)


p



