library(plotly)


kitchen<-read.csv("kitchen.csv")
kitchen2<-kitchen
kitchen$image<-NULL
kitchen$attributes<-NULL
kitchen$categories <-NULL
kitchen$type<-NULL
kitchen$single_category<-NULL
kitchen<-scale(kitchen)

