library(dplyr)
library(Rtsne)

###pca data
data<- read.csv('pca.csv')
### Entropy data
entropy<-read.csv('averages.csv') 
#### csv to get categories
categories <- read.csv('categories.csv')
names(data)[names(data)=='img']<- 'image'
names(categories)[names(categories)=="image_num"]<-'image'

joined_data<- left_join(data,entropy)


#Data to be segmented into categories
segment_data<- left_join(data,entropy)
segment_data<- left_join(segment_data,categories)
segment_data <- na.omit(segment_data)

kitchen_data<- segment_data[segment_data$single_category %in% c("kitchen'"),]
kitchen <- kitchen_data
kitchen$image<-NULL
kitchen$attributes<-NULL
kitchen$categories <-NULL
kitchen$type<-NULL
kitchen$single_category<-NULL

write.csv(kitchen_data,"kitchen.csv")
folder_files <- list.files(path = "D:\\Harvard\\Csc 14\\NEW PROK\\Visualization\\imgs", ignore.case = T, full.names = T)
for (i in folder_files) {
  if (basename(i) %in% kitchen_data$image){
      file.copy(i,"D:\\Harvard\\Csc 14\\NEW PROK\\Visualization\\kitchen")
      file.remove(i)
    
  }
}