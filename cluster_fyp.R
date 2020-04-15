library(ggplot2)
library(forecast)
library(xts)
#read in data
data1 = read.csv("dbdata3-3.csv",header = TRUE)
data2 = read.csv("dbdata4.csv",header=TRUE)
data3 = read.csv("dbdata5.csv",header=TRUE)
data4 = read.csv("dbdata6.csv",header=TRUE)
data5 = read.csv("dbdata7.csv",header=TRUE)
data6 = read.csv("dbdata8.csv",header=TRUE)

#combine all datasets into one
live_data= rbind(data1,data2,data3,data4,data5,data6)

#set column x to null because this is same as index column
live_data['X']=NULL

#remove duplicates of data
live_data <-live_data[!duplicated(live_data),]

#get times in a vector
times = live_data[live_data$number==8,]$time
stand_1 = live_data[live_data$number==8,]
stand_2 = live_data[live_data$number==19,]
cor(stand_1$available_bikes,stand_2$available_bikes)

#plot lat and long onto map
#X = Longitude, Y = Latitude.
cc<-palette()
colours<-c("springgreen4","aliceblue","violetred4","royalblue4","orange","hotpink1","lawngreen","goldenrod2","firebrick4","cyan3","magenta2","deepskyblue1","coral2","darkviolet","azure4","yellow2","darkorchid2","darkorange2","darkolivegreen","cyan1","cornflowerblue","brown","palevioletred2","orchid3","tan3","plum2","lightsalmon2")
palette(c(cc,colours))

z<-data.frame(long=map_data[,5],lat=map_data[,4],stand_no=map_data$Number)
head(z)

library(sp)
library(rgdal)
library(geosphere)

geo.dist = function(df) {
  require(geosphere)
  d <- function(i,z){         # z[1:2] contain long, lat
    dist <- rep(0,nrow(z))
    dist[i:nrow(z)] <- distHaversine(z[i:nrow(z),1:2],z[i,1:2])
    return(dist)
  }
  dm <- do.call(cbind,lapply(1:nrow(df),d,df))
  return(as.dist(dm))
}

#det distance between 2 lat and long points
p1<-c(map_data$Longitude[1],map_data$Latitude[1])
p2<-c(map_data$Longitude[2],map_data$Latitude[2])
distHaversine(p1, p2)

d1<- geo.dist(z)   # distance matrix
hc<- hclust(d1,method="complete")      # hierarchical clustering
plot(hc)   
abline(h=500,lty=1,col=2)
clusterCut <- cutree(hc, h=500)
# add cluster col to z and out in which cluster each datapoint is in
z$clust <- cutree(hc,h=500)

#look at radius and distance between stands

plot <- ggplot(data = z, aes(x = long, y = lat, colour=factor(clust), fill = clust)) +
  geom_point() + 
  geom_polygon(aes(fill = clust), alpha = 0.3)

###make maps with points
library(ggmap)
library(ggplot2)
gc <- geocode("Ha'penny Bridge, Dublin")
map <- get_map(gc,zoom=13)
plot2<-ggmap(map) +
  geom_point(data = z,aes(x = long, y = lat, fill=clust), colour=factor(z$clust), size = 2) 

map <- get_map(gc,zoom=13)
plot3<-ggmap(map) +
  geom_point(aes(x = long, y = lat),data = z, colour="black", size = 2)

#within each cluster combine available bikes
clust_1 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2))
clust_2 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_3 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_4 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_5 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_6 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_7 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_8 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_9 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_10 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_11 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_12 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_13 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_14 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_15 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_16 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_17 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_18 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_19 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_20 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_21 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_22 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_23 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_24 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_25 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_26 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_27 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_28 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2))
clust_29 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_30 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_31 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_32 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_33 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_34 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 
clust_35 <- data.frame(matrix(nrow=nrow(stand_1),ncol=2)) 

#get demand for every cluster 
###############clust1##################
clust_1$X1 = times
clust_1$X2 = 0
colnames(clust_1) <- c("time", "available_bikes")
clust_1$total_stands = 0
clust_1$free_stands = 0
head(clust_1)

#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==1)
  {
    no = z$stand_no[i]
    clust_1$available_bikes = clust_1$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_1$free_stands = clust_1$free_stands+live_data[live_data$number==no,]$free_stands
    clust_1$total_stands = clust_1$total_stands+live_data[live_data$number==no,]$total_stands
  }
}
#time in data is in ms, this puts it to secs
clust_1$time= times/1000

#change from unix time to normal time
clust_1$normal_time=as.POSIXct(clust_1$time, origin="1970-01-01")

#get rid of duplicated rows
clust_1 <-clust_1[!duplicated(clust_1['time']),]

#run demand function and retrurn new df with demand
clust_1 <-demand(clust_1)

###########clust2#############
clust_2$X1 = times
clust_2$X2 = 0
colnames(clust_2) <- c("time", "available_bikes")
clust_2$total_stands = 0
clust_2$free_stands = 0


#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==2)
  {
    no = z$stand_no[i]
    clust_2$available_bikes = clust_2$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_2$free_stands = clust_2$free_stands+live_data[live_data$number==no,]$free_stands
    clust_2$total_stands = clust_2$total_stands+live_data[live_data$number==no,]$total_stands
  }
}

#time in data is in ms, this puts it to secs
clust_2$time= clust_2$time/1000

#change from unix time to normal time
clust_2$normal_time=as.POSIXct(clust_2$time, origin="1970-01-01")

#get rid of duplicated rows
clust_2 <-clust_2[!duplicated(clust_2['time']),]

#run demand function and retrurn new df with demand
clust_2 <-demand(clust_2)

###########clust3#############
clust_3$X1 = times
clust_3$X2 = 0
colnames(clust_3) <- c("time", "available_bikes")
clust_3$total_stands = 0
clust_3$free_stands = 0

#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==3)
  {
    no = z$stand_no[i]
    clust_3$available_bikes = clust_3$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_3$free_stands = clust_3$free_stands+live_data[live_data$number==no,]$free_stands
    clust_3$total_stands = clust_3$total_stands+live_data[live_data$number==no,]$total_stands
  }
}

#time in data is in ms, this puts it to secs
clust_3$time= clust_3$time/1000

#change from unix time to normal time
clust_3$normal_time=as.POSIXct(clust_3$time, origin="1970-01-01")

#get rid of duplicated rows
clust_3 <-clust_3[!duplicated(clust_3['time']),]

#run demand function and retrurn new df with demand
clust_3 <-demand(clust_3)

###########clust4#############
clust_4$X1 = times
clust_4$X2 = 0
colnames(clust_4) <- c("time", "available_bikes")
clust_4$total_stands = 0
clust_4$free_stands = 0


#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==4)
  {
    no = z$stand_no[i]
    clust_4$available_bikes = clust_4$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_4$free_stands = clust_4$free_stands+live_data[live_data$number==no,]$free_stands
    clust_4$total_stands = clust_4$total_stands+live_data[live_data$number==no,]$total_stands
  }
}

#time in data is in ms, this puts it to secs
clust_4$time= clust_4$time/1000

#change from unix time to normal time
clust_4$normal_time=as.POSIXct(clust_4$time, origin="1970-01-01")

#get rid of duplicated rows
clust_4 <-clust_4[!duplicated(clust_4['time']),]

#run demand function and retrurn new df with demand
clust_4 <-demand(clust_4)

###########clust5#############
clust_5$X1 = times
clust_5$X2 = 0
colnames(clust_5) <- c("time", "available_bikes")
clust_5$total_stands = 0
clust_5$free_stands = 0

#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==5)
  {
    no = z$stand_no[i]
    clust_5$available_bikes = clust_5$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_5$free_stands = clust_5$free_stands+live_data[live_data$number==no,]$free_stands
    
    clust_5$total_stands = clust_5$total_stands+live_data[live_data$number==no,]$total_stands
  }
}

#time in data is in ms, this puts it to secs
clust_5$time= clust_5$time/1000

#change from unix time to normal time
clust_5$normal_time=as.POSIXct(clust_5$time, origin="1970-01-01")

#get rid of duplicated rows
clust_5 <-clust_5[!duplicated(clust_5['time']),]

#run demand function and retrurn new df with demand
clust_5 <-demand(clust_5)

###########clust6#############
clust_6$X1 = times
clust_6$X2 = 0
colnames(clust_6) <- c("time", "available_bikes")
clust_6$total_stands = 0
clust_6$free_stands = 0

#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==6)
  {
    no = z$stand_no[i]
    clust_6$available_bikes = clust_6$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_6$free_stands = clust_6$free_stands+live_data[live_data$number==no,]$free_stands
    clust_6$total_stands = clust_6$total_stands+live_data[live_data$number==no,]$total_stands
  }
}

#time in data is in ms, this puts it to secs
clust_6$time= clust_6$time/1000

#change from unix time to normal time
clust_6$normal_time=as.POSIXct(clust_6$time, origin="1970-01-01")

#get rid of duplicated rows
clust_6 <-clust_6[!duplicated(clust_6['time']),]

#run demand function and retrurn new df with demand
clust_6 <-demand(clust_6)
###########clust7#############
clust_7$X1 = times
clust_7$X2 = 0
colnames(clust_7) <- c("time", "available_bikes")
clust_7$total_stands = 0
clust_7$free_stands = 0

#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==7)
  {
    no = z$stand_no[i]
    clust_7$available_bikes = clust_7$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_7$free_stands = clust_7$free_stands+live_data[live_data$number==no,]$free_stands
    clust_7$total_stands = clust_7$total_stands+live_data[live_data$number==no,]$total_stands
  }
}

#time in data is in ms, this puts it to secs
clust_7$time= clust_7$time/1000

#change from unix time to normal time
clust_7$normal_time=as.POSIXct(clust_7$time, origin="1970-01-01")

#get rid of duplicated rows
clust_7 <-clust_7[!duplicated(clust_7['time']),]

#run demand function and retrurn new df with demand
clust_7 <-demand(clust_7)
###########clust8#############
clust_8$X1 = times
clust_8$X2 = 0
colnames(clust_8) <- c("time", "available_bikes")
clust_8$total_stands = 0
clust_8$free_stands = 0


#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==8)
  {
    no = z$stand_no[i]
    clust_8$available_bikes = clust_8$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_8$free_stands = clust_8$free_stands+live_data[live_data$number==no,]$free_stands
    clust_8$total_stands = clust_8$total_stands+live_data[live_data$number==no,]$total_stands
  }
}

#time in data is in ms, this puts it to secs
clust_8$time= clust_8$time/1000

#change from unix time to normal time
clust_8$normal_time=as.POSIXct(clust_8$time, origin="1970-01-01")

#get rid of duplicated rows
clust_8 <-clust_8[!duplicated(clust_8['time']),]

#run demand function and retrurn new df with demand
clust_8 <-demand(clust_8)
###########clust9#############
clust_9$X1 = times
clust_9$X2 = 0
colnames(clust_9) <- c("time", "available_bikes")
clust_9$total_stands = 0
clust_9$free_stands = 0

#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==9)
  {
    no = z$stand_no[i]
    clust_9$available_bikes = clust_9$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_9$free_stands = clust_9$free_stands+live_data[live_data$number==no,]$free_stands
    clust_9$total_stands = clust_9$total_stands+live_data[live_data$number==no,]$total_stands
  }
}

#time in data is in ms, this puts it to secs
clust_9$time= clust_9$time/1000

#change from unix time to normal time
clust_9$normal_time=as.POSIXct(clust_9$time, origin="1970-01-01")
#get rid of duplicated rows
clust_9 <-clust_9[!duplicated(clust_9['time']),]

#run demand function and retrurn new df with demand
clust_9 <-demand(clust_9)
###########clust10#############
clust_10$X1 = times
clust_10$X2 = 0
colnames(clust_10) <- c("time", "available_bikes")
clust_10$total_stands = 0
clust_10$free_stands = 0


#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==10)
  {
    no = z$stand_no[i]
    clust_10$available_bikes = clust_10$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_10$free_stands = clust_10$free_stands+live_data[live_data$number==no,]$free_stands
    clust_10$total_stands = clust_10$total_stands+live_data[live_data$number==no,]$total_stands
  }
}


#time in data is in ms, this puts it to secs
clust_10$time= clust_10$time/1000

#change from unix time to normal time
clust_10$normal_time=as.POSIXct(clust_10$time, origin="1970-01-01")

#get rid of duplicated rows
clust_10 <-clust_10[!duplicated(clust_10['time']),]

#run demand function and retrurn new df with demand
clust_10 <-demand(clust_10)
###########clust11#############
clust_11$X1 = times
clust_11$X2 = 0
colnames(clust_11) <- c("time", "available_bikes")
clust_11$total_stands = 0
clust_11$free_stands = 0


#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==11)
  {
    no = z$stand_no[i]
    clust_11$available_bikes = clust_11$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_11$free_stands = clust_11$free_stands+live_data[live_data$number==no,]$free_stands
    clust_11$total_stands = clust_11$total_stands+live_data[live_data$number==no,]$total_stands
  }
}


#time in data is in ms, this puts it to secs
clust_11$time= clust_11$time/1000


#change from unix time to normal time
clust_11$normal_time=as.POSIXct(clust_11$time, origin="1970-01-01")

#get rid of duplicated rows
clust_11 <-clust_11[!duplicated(clust_11['time']),]

#run demand function and retrurn new df with demand
clust_11 <-demand(clust_11)
###########clust12#############
clust_12$X1 = times
clust_12$X2 = 0
colnames(clust_12) <- c("time", "available_bikes")
clust_12$total_stands = 0
clust_12$free_stands = 0


#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==12)
  {
    no = z$stand_no[i]
    clust_12$available_bikes = clust_12$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_12$free_stands = clust_12$free_stands+live_data[live_data$number==no,]$free_stands
    clust_12$total_stands = clust_12$total_stands+live_data[live_data$number==no,]$total_stands
  }
}


#time in data is in ms, this puts it to secs
clust_12$time= clust_12$time/1000


#change from unix time to normal time
clust_12$normal_time=as.POSIXct(clust_12$time, origin="1970-01-01")

#get rid of duplicated rows
clust_12 <-clust_12[!duplicated(clust_12['time']),]

#run demand function and retrurn new df with demand
clust_12 <-demand(clust_12)
###########clust13#############
clust_13$X1 = times
clust_13$X2 = 0
colnames(clust_13) <- c("time", "available_bikes")
clust_13$total_stands = 0
clust_13$free_stands = 0

#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==13)
  {
    no = z$stand_no[i]
    clust_13$available_bikes = clust_13$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_13$free_stands = clust_13$free_stands+live_data[live_data$number==no,]$free_stands
    clust_13$total_stands = clust_13$total_stands+live_data[live_data$number==no,]$total_stands
  }
}

#time in data is in ms, this puts it to secs
clust_13$time= clust_13$time/1000


#change from unix time to normal time
clust_13$normal_time=as.POSIXct(clust_13$time, origin="1970-01-01")

#get rid of duplicated rows
clust_13 <-clust_13[!duplicated(clust_13['time']),]

#run demand function and retrurn new df with demand
clust_13 <-demand(clust_13)
###########clust14#############
clust_14$X1 = times
clust_14$X2 = 0
colnames(clust_14) <- c("time", "available_bikes")
clust_14$total_stands = 0
clust_14$free_stands = 0


#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==14)
  {
    no = z$stand_no[i]
    clust_14$available_bikes = clust_14$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_14$free_stands = clust_14$free_stands+live_data[live_data$number==no,]$free_stands
    clust_14$total_stands = clust_14$total_stands+live_data[live_data$number==no,]$total_stands
  }
}


#time in data is in ms, this puts it to secs
clust_14$time= clust_14$time/1000


#change from unix time to normal time
clust_14$normal_time=as.POSIXct(clust_14$time, origin="1970-01-01")

#get rid of duplicated rows
clust_14 <-clust_14[!duplicated(clust_14['time']),]

#run demand function and retrurn new df with demand
clust_14 <-demand(clust_14)
###########clust15#############
clust_15$X1 = times
clust_15$X2 = 0
colnames(clust_15) <- c("time", "available_bikes")
clust_15$total_stands = 0
clust_15$free_stands = 0


#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==15)
  {
    no = z$stand_no[i]
    clust_15$available_bikes = clust_15$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_15$free_stands = clust_15$free_stands+live_data[live_data$number==no,]$free_stands
    clust_15$total_stands = clust_15$total_stands+live_data[live_data$number==no,]$total_stands
  }
}


#time in data is in ms, this puts it to secs
clust_15$time= clust_15$time/1000


#change from unix time to normal time
clust_15$normal_time=as.POSIXct(clust_15$time, origin="1970-01-01")

#get rid of duplicated rows
clust_15 <-clust_15[!duplicated(clust_15['time']),]

#run demand function and retrurn new df with demand
clust_15 <-demand(clust_15)
###########clust16#############
clust_16$X1 = times
clust_16$X2 = 0
colnames(clust_16) <- c("time", "available_bikes")
clust_16$total_stands = 0
clust_16$free_stands = 0


#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==16)
  {
    no = z$stand_no[i]
    clust_16$available_bikes = clust_16$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_16$free_stands = clust_16$free_stands+live_data[live_data$number==no,]$free_stands
    clust_16$total_stands = clust_16$total_stands+live_data[live_data$number==no,]$total_stands
  }
}


#time in data is in ms, this puts it to secs
clust_16$time= clust_16$time/1000


#change from unix time to normal time
clust_16$normal_time=as.POSIXct(clust_16$time, origin="1970-01-01")

#get rid of duplicated rows
clust_16 <-clust_16[!duplicated(clust_16['time']),]

#run demand function and retrurn new df with demand
clust_16 <-demand(clust_16)
###########clust17#############
clust_17$X1 = times
clust_17$X2 = 0
colnames(clust_17) <- c("time", "available_bikes")
clust_17$total_stands = 0
clust_17$free_stands = 0

#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==17)
  {
    no = z$stand_no[i]
    clust_17$available_bikes = clust_17$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_17$free_stands = clust_17$free_stands+live_data[live_data$number==no,]$free_stands
    clust_17$total_stands = clust_17$total_stands+live_data[live_data$number==no,]$total_stands
  }
}


#time in data is in ms, this puts it to secs
clust_17$time= clust_17$time/1000


#change from unix time to normal time
clust_17$normal_time=as.POSIXct(clust_17$time, origin="1970-01-01")

#get rid of duplicated rows
clust_17 <-clust_17[!duplicated(clust_17['time']),]

#run demand function and retrurn new df with demand
clust_17 <-demand(clust_17)
###########clust18#############
clust_18$X1 = times
clust_18$X2 = 0
colnames(clust_18) <- c("time", "available_bikes")
clust_18$total_stands = 0
clust_18$free_stands = 0

#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==18)
  {
    no = z$stand_no[i]
    clust_18$available_bikes = clust_18$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_18$free_stands = clust_18$free_stands+live_data[live_data$number==no,]$free_stands
    clust_18$total_stands = clust_18$total_stands+live_data[live_data$number==no,]$total_stands
  }
}


#time in data is in ms, this puts it to secs
clust_18$time= clust_18$time/1000


#change from unix time to normal time
clust_18$normal_time=as.POSIXct(clust_18$time, origin="1970-01-01")

#get rid of duplicated rows
clust_18 <-clust_18[!duplicated(clust_18['time']),]

#run demand function and retrurn new df with demand
clust_18 <-demand(clust_18)
###########clust19#############
clust_19$X1 = times
clust_19$X2 = 0
colnames(clust_19) <- c("time", "available_bikes")
clust_19$total_stands = 0
clust_19$free_stands = 0


#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==19)
  {
    no = z$stand_no[i]
    clust_19$available_bikes = clust_19$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_19$free_stands = clust_19$free_stands+live_data[live_data$number==no,]$free_stands
    clust_19$total_stands = clust_19$total_stands+live_data[live_data$number==no,]$total_stands
  }
}


#time in data is in ms, this puts it to secs
clust_19$time= clust_19$time/1000


#change from unix time to normal time
clust_19$normal_time=as.POSIXct(clust_19$time, origin="1970-01-01")

#get rid of duplicated rows
clust_19 <-clust_19[!duplicated(clust_19['time']),]

#run demand function and retrurn new df with demand
clust_19 <-demand(clust_19)
###########clust20#############
clust_20$X1 = times
clust_20$X2 = 0
colnames(clust_20) <- c("time", "available_bikes")
clust_20$total_stands = 0
clust_20$free_stands = 0

#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==20)
  {
    no = z$stand_no[i]
    clust_20$available_bikes = clust_20$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_20$free_stands = clust_20$free_stands+live_data[live_data$number==no,]$free_stands
    clust_20$total_stands = clust_20$total_stands+live_data[live_data$number==no,]$total_stands
  }
}


#time in data is in ms, this puts it to secs
clust_20$time= clust_20$time/1000

#change from unix time to normal time
clust_20$normal_time=as.POSIXct(clust_20$time, origin="1970-01-01")

#get rid of duplicated rows
clust_20 <-clust_20[!duplicated(clust_20['time']),]

#run demand function and retrurn new df with demand
clust_20 <-demand(clust_20)

###########clust21#############
clust_21$X1 = times
clust_21$X2 = 0
colnames(clust_21) <- c("time", "available_bikes")
clust_21$total_stands = 0
clust_21$free_stands = 0


#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==21)
  {
    no = z$stand_no[i]
    clust_21$available_bikes = clust_21$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_21$free_stands = clust_21$free_stands+live_data[live_data$number==no,]$free_stands
    clust_21$total_stands = clust_21$total_stands+live_data[live_data$number==no,]$total_stands
  }
}


#time in data is in ms, this puts it to secs
clust_21$time= clust_21$time/1000


#change from unix time to normal time
clust_21$normal_time=as.POSIXct(clust_21$time, origin="1970-01-01")

#get rid of duplicated rows
clust_21 <-clust_21[!duplicated(clust_21['time']),]

#run demand function and retrurn new df with demand
clust_21 <-demand(clust_21)

###########clust22#############
clust_22$X1 = times
clust_22$X2 = 0
colnames(clust_22) <- c("time", "available_bikes")
clust_22$total_stands = 0
clust_22$free_stands = 0

#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==22)
  {
    no = z$stand_no[i]
    clust_22$available_bikes = clust_22$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_22$free_stands = clust_22$free_stands+live_data[live_data$number==no,]$free_stands
    clust_22$total_stands = clust_22$total_stands+live_data[live_data$number==no,]$total_stands
  }
}

#time in data is in ms, this puts it to secs
clust_22$time= clust_22$time/1000


#change from unix time to normal time
clust_22$normal_time=as.POSIXct(clust_22$time, origin="1970-01-01")

#get rid of duplicated rows
clust_22 <-clust_22[!duplicated(clust_22['time']),]

#run demand function and retrurn new df with demand
clust_22 <-demand(clust_22)

###########clust23#############
clust_23$X1 = times
clust_23$X2 = 0
colnames(clust_23) <- c("time", "available_bikes")
clust_23$total_stands = 0
clust_23$free_stands = 0

#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==23)
  {
    no = z$stand_no[i]
    clust_23$available_bikes = clust_23$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_23$free_stands = clust_23$free_stands+live_data[live_data$number==no,]$free_stands
    clust_23$total_stands = clust_23$total_stands+live_data[live_data$number==no,]$total_stands
  }
}

#time in data is in ms, this puts it to secs
clust_23$time= clust_23$time/1000

#change from unix time to normal time
clust_23$normal_time=as.POSIXct(clust_23$time, origin="1970-01-01")

#get rid of duplicated rows
clust_23 <-clust_23[!duplicated(clust_23['time']),]

#run demand function and retrurn new df with demand
clust_23 <-demand(clust_23)

###########clust24#############
clust_24$X1 = times
clust_24$X2 = 0
colnames(clust_24) <- c("time", "available_bikes")
clust_24$total_stands = 0
clust_24$free_stands = 0


#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==24)
  {
    no = z$stand_no[i]
    clust_24$available_bikes = clust_24$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_24$free_stands = clust_24$free_stands+live_data[live_data$number==no,]$free_stands
    clust_24$total_stands = clust_24$total_stands+live_data[live_data$number==no,]$total_stands
  }
}


#time in data is in ms, this puts it to secs
clust_24$time= clust_24$time/1000


#change from unix time to normal time
clust_24$normal_time=as.POSIXct(clust_24$time, origin="1970-01-01")

#get rid of duplicated rows
clust_24 <-clust_24[!duplicated(clust_24['time']),]

#run demand function and retrurn new df with demand
clust_24 <-demand(clust_24)
###########clust25#############
clust_25$X1 = times
clust_25$X2 = 0
colnames(clust_25) <- c("time", "available_bikes")
clust_25$total_stands = 0
clust_25$free_stands = 0


#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==25)
  {
    no = z$stand_no[i]
    clust_25$available_bikes = clust_25$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_25$free_stands = clust_25$free_stands+live_data[live_data$number==no,]$free_stands
    clust_25$total_stands = clust_25$total_stands+live_data[live_data$number==no,]$total_stands
  }
}


#time in data is in ms, this puts it to secs
clust_25$time= clust_25$time/1000


#change from unix time to normal time
clust_25$normal_time=as.POSIXct(clust_25$time, origin="1970-01-01")

#get rid of duplicated rows
clust_25 <-clust_25[!duplicated(clust_25['time']),]

#run demand function and retrurn new df with demand
clust_25 <-demand(clust_25)
###########clust26#############
clust_26$X1 = times
clust_26$X2 = 0
colnames(clust_26) <- c("time", "available_bikes")
clust_26$total_stands = 0
clust_26$free_stands = 0

#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==26)
  {
    no = z$stand_no[i]
    clust_26$available_bikes = clust_26$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_26$free_stands = clust_26$free_stands+live_data[live_data$number==no,]$free_stands
    clust_26$total_stands = clust_26$total_stands+live_data[live_data$number==no,]$total_stands
  }
}

#time in data is in ms, this puts it to secs
clust_26$time= clust_26$time/1000

#change from unix time to normal time
clust_26$normal_time=as.POSIXct(clust_26$time, origin="1970-01-01")

#get rid of duplicated rows
clust_26 <-clust_26[!duplicated(clust_26['time']),]

#run demand function and retrurn new df with demand
clust_26 <-demand(clust_26)
###########clust27#############
clust_27$X1 = times
clust_27$X2 = 0
colnames(clust_27) <- c("time", "available_bikes")
clust_27$total_stands = 0
clust_27$free_stands = 0

#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==27)
  {
    no = z$stand_no[i]
    clust_27$available_bikes = clust_27$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_27$free_stands = clust_27$free_stands+live_data[live_data$number==no,]$free_stands
    clust_27$total_stands = clust_27$total_stands+live_data[live_data$number==no,]$total_stands
  }
}


#time in data is in ms, this puts it to secs
clust_27$time= clust_27$time/1000


#change from unix time to normal time
clust_27$normal_time=as.POSIXct(clust_27$time, origin="1970-01-01")

#get rid of duplicated rows
clust_27 <-clust_27[!duplicated(clust_27['time']),]

#run demand function and retrurn new df with demand
clust_27 <-demand(clust_27)
###########clust28#############
clust_28$X1 = times
clust_28$X2 = 0
colnames(clust_28) <- c("time", "available_bikes")
clust_28$total_stands = 0
clust_28$free_stands = 0

#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==28)
  {
    no = z$stand_no[i]
    clust_28$available_bikes = clust_28$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_28$free_stands = clust_28$free_stands+live_data[live_data$number==no,]$free_stands
    clust_28$total_stands = clust_28$total_stands+live_data[live_data$number==no,]$total_stands
  }
}

#time in data is in ms, this puts it to secs
clust_28$time= clust_28$time/1000

#change from unix time to normal time
clust_28$normal_time=as.POSIXct(clust_28$time, origin="1970-01-01")

#get rid of duplicated rows
clust_28 <-clust_28[!duplicated(clust_28['time']),]

#run demand function and retrurn new df with demand
clust_28 <-demand(clust_28)

###########clust29#############
clust_29$X1 = times
clust_29$X2 = 0
colnames(clust_29) <- c("time", "available_bikes")
clust_29$total_stands = 0
clust_29$free_stands = 0

#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==29)
  {
    no = z$stand_no[i]
    clust_29$available_bikes = clust_29$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_29$free_stands = clust_29$free_stands+live_data[live_data$number==no,]$free_stands
    clust_29$total_stands = clust_29$total_stands+live_data[live_data$number==no,]$total_stands
  }
}


#time in data is in ms, this puts it to secs
clust_29$time= clust_29$time/1000


#change from unix time to normal time
clust_29$normal_time=as.POSIXct(clust_29$time, origin="1970-01-01")

#get rid of duplicated rows
clust_29 <-clust_29[!duplicated(clust_29['time']),]

#run demand function and retrurn new df with demand
clust_29 <-demand(clust_29)
###########clust30#############
clust_30$X1 = times
clust_30$X2 = 0
colnames(clust_30) <- c("time", "available_bikes")
clust_30$total_stands = 0
clust_30$free_stands = 0


#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==30)
  {
    no = z$stand_no[i]
    clust_30$available_bikes = clust_30$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_30$free_stands = clust_30$free_stands+live_data[live_data$number==no,]$free_stands
    clust_30$total_stands = clust_30$total_stands+live_data[live_data$number==no,]$total_stands
  }
}

#time in data is in ms, this puts it to secs
clust_30$time= clust_30$time/1000

#change from unix time to normal time
clust_30$normal_time=as.POSIXct(clust_30$time, origin="1970-01-01")

#get rid of duplicated rows
clust_30 <-clust_30[!duplicated(clust_30['time']),]

#run demand function and retrurn new df with demand
clust_30 <-demand(clust_30)
###########clust31#############
clust_31$X1 = times
clust_31$X2 = 0
colnames(clust_31) <- c("time", "available_bikes")
clust_31$total_stands = 0
clust_31$free_stands = 0

#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==31)
  {
    no = z$stand_no[i]
    clust_31$available_bikes = clust_31$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_31$free_stands = clust_31$free_stands+live_data[live_data$number==no,]$free_stands
    clust_31$total_stands = clust_31$total_stands+live_data[live_data$number==no,]$total_stands
  }
}

#time in data is in ms, this puts it to secs
clust_31$time= clust_31$time/1000


#change from unix time to normal time
clust_31$normal_time=as.POSIXct(clust_31$time, origin="1970-01-01")

#get rid of duplicated rows
clust_31 <-clust_31[!duplicated(clust_31['time']),]

#run demand function and retrurn new df with demand
clust_31 <-demand(clust_31)
###########clust32#############
clust_32$X1 = times
clust_32$X2 = 0
colnames(clust_32) <- c("time", "available_bikes")
clust_32$total_stands = 0
clust_32$free_stands = 0


#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==32)
  {
    no = z$stand_no[i]
    clust_32$available_bikes = clust_32$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_32$free_stands = clust_32$free_stands+live_data[live_data$number==no,]$free_stands
    clust_32$total_stands = clust_32$total_stands+live_data[live_data$number==no,]$total_stands
  }
}

#time in data is in ms, this puts it to secs
clust_32$time= clust_32$time/1000

#change from unix time to normal time
clust_32$normal_time=as.POSIXct(clust_32$time, origin="1970-01-01")

#get rid of duplicated rows
clust_32 <-clust_32[!duplicated(clust_32['time']),]

#run demand function and retrurn new df with demand
clust_32 <-demand(clust_32)
###########clust33#############
clust_33$X1 = times
clust_33$X2 = 0
colnames(clust_33) <- c("time", "available_bikes")
clust_33$total_stands = 0
clust_33$free_stands = 0

#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==33)
  {
    no = z$stand_no[i]
    clust_33$available_bikes = clust_33$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_33$free_stands = clust_33$free_stands+live_data[live_data$number==no,]$free_stands
    clust_33$total_stands = clust_33$total_stands+live_data[live_data$number==no,]$total_stands
  }
}


#time in data is in ms, this puts it to secs
clust_33$time= clust_33$time/1000


#change from unix time to normal time
clust_33$normal_time=as.POSIXct(clust_33$time, origin="1970-01-01")
#get rid of duplicated rows
clust_33 <-clust_33[!duplicated(clust_33['time']),]

#run demand function and retrurn new df with demand
clust_33 <-demand(clust_33)
###########clust34#############
clust_34$X1 = times
clust_34$X2 = 0
colnames(clust_34) <- c("time", "available_bikes")
clust_34$total_stands = 0
clust_34$free_stands = 0

#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==34)
  {
    no = z$stand_no[i]
    clust_34$available_bikes = clust_34$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_34$free_stands = clust_34$free_stands+live_data[live_data$number==no,]$free_stands
    clust_34$total_stands = clust_34$total_stands+live_data[live_data$number==no,]$total_stands
  }
}


#time in data is in ms, this puts it to secs
clust_34$time= clust_34$time/1000


#change from unix time to normal time
clust_34$normal_time=as.POSIXct(clust_34$time, origin="1970-01-01")

#get rid of duplicated rows
clust_34 <-clust_34[!duplicated(clust_34['time']),]

#run demand function and retrurn new df with demand
clust_34 <-demand(clust_34)
###########clust35#############
clust_35$X1 = times
clust_35$X2 = 0
colnames(clust_35) <- c("time", "available_bikes")
clust_35$total_stands = 0
clust_35$free_stands = 0

#this should combine available bikes and total stands for each cluster
for(i in 1:100)
{
  if(z$clust[i]==35)
  {
    no = z$stand_no[i]
    clust_35$available_bikes = clust_35$available_bikes+live_data[live_data$number==no,]$available_bikes
    clust_35$free_stands = clust_35$free_stands+live_data[live_data$number==no,]$free_stands
    clust_35$total_stands = clust_35$total_stands+live_data[live_data$number==no,]$total_stands
  }
}

#time in data is in ms, this puts it to secs
clust_35$time= clust_35$time/1000

#change from unix time to normal time
clust_35$normal_time=as.POSIXct(clust_35$time, origin="1970-01-01")

#get rid of duplicated rows
clust_35 <-clust_35[!duplicated(clust_35['time']),]

#run demand function and retrurn new df with demand
clust_35 <-demand(clust_35)

###############remove unnessesary variable and save dataset##################
#combine all clusters datasets
clust_1$clust_no=1
clust_2$clust_no=2
clust_3$clust_no=3
clust_4$clust_no=4
clust_5$clust_no=5
clust_6$clust_no=6
clust_7$clust_no=7
clust_8$clust_no=8
clust_9$clust_no=9
clust_10$clust_no=10
clust_11$clust_no=11
clust_12$clust_no=12
clust_13$clust_no=13
clust_14$clust_no=14
clust_15$clust_no=15
clust_16$clust_no=16
clust_17$clust_no=17
clust_18$clust_no=18
clust_19$clust_no=19
clust_20$clust_no=20
clust_21$clust_no=21
clust_22$clust_no=22
clust_23$clust_no=23
clust_24$clust_no=24
clust_25$clust_no=25
clust_26$clust_no=26
clust_27$clust_no=27
clust_28$clust_no=28
clust_29$clust_no=29
clust_30$clust_no=30
clust_31$clust_no=31
clust_32$clust_no=32
clust_33$clust_no=33
clust_34$clust_no=34
clust_35$clust_no=35

all_clusters <- rbind(clust_1,clust_2,clust_3,clust_4,clust_5,clust_6,clust_7,clust_8,clust_9,clust_10,clust_11,clust_12,clust_12,clust_13,clust_14,clust_15,clust_16,clust_17,clust_18,clust_19,clust_20,clust_21,clust_22,clust_23,clust_24,clust_25,clust_26,clust_27,clust_28,clust_29,clust_30,clust_31,clust_32,clust_33,clust_34,clust_35)

##############demand function############
###input dataframe with x and y cols and reruns slopes
get_slopes <- function(slopevals)
{
  slopes=c(0,0,0,0)
  for(i in 1:(nrow(slopevals)-9))
  {
    x<- slopevals$x[i:(i+9)]
    y<- slopevals$y[i:(i+9)]
    a<-lm(y~x)$coefficients[2]
    slopes[i+4]=a
  }
  slopes[(nrow(slopevals)-3):nrow(slopevals)]=0
  return(slopes)
}

##input cluster dataframe and output demand variable
demand <- function(cluster)
{
  #lowess fits lines, loess can fit line or parabola(default = parabola)
  #The smoothing parameter, alpha , is the fraction of the total number n of data points that are used in each local fit
  temp = lowess(cluster$available_bikes~cluster$time,f=1/100,delta=1/100) #Use higher evaluation for more points
  
  #get rolling slope of average slope of 5 vals before and after each value
  slopevals<-data.frame(x=temp$x,y=temp$y)
  slopevals$slopes=(get_slopes(slopevals)*3600)#change from bikes per sec to bikes per hour
  
  #set demand value
  cluster$demand=slopevals$slopes
  
  #add varibale which describes whether stand is empty,full or neither
  for (i in 1:nrow(clust_1))
  {
    if(cluster$available_bikes[i]==0 )
    {
      cluster$state[i] = "empty"
    }
    else if(cluster$free_stands[i]==0)
    {
      cluster$state[i]= "full"
    }
    else
    {
      cluster$state[i] = "-"
    }
  }
  
  ############forecasting###########
  #loop through all times to find when stand is full or empty to predict demand
  x=1
  while(x<nrow(cluster))
  {
    if(cluster$state[x]!="-")
    {
      end_index=1
      while(cluster$state[x+end_index]!="-")
      {
        #find how long stand stays full/empty
        end_index=end_index+1
      }

      if(end_index>5&&x>100)
      {
        #make ts variable and predict demand based on all prev data to forecast demand when stand full/empty
        temp_df=cluster[0:x,]
        
        #temp_ts <- ts(temp_df$demand,start=1,end=24,frequency=60,class="ts")
        tts <- xts(temp_df$demand,order.by=as.POSIXct(temp_df$normal_time,origin="1970-01-01"))
        model=auto.arima(tts)
        temp_predict <- predict(model,n.ahead=end_index)
        
        #fill values in for demand
        for(i in 0:(end_index-1))
        {
          cluster$demand[x+i]=temp_predict$pred[i+1]
        }
      }
      x=x+end_index
    }
    else
    {
      x=x+1
    }
  }
  return(cluster)
}


#############graph+save##############
#graph of demands for getting rid of van made demands
all_clusters<-all_clusters[!(all_clusters$demand>210),]
all_clusters<-all_clusters[!(all_clusters$demand<(-300)),]


plot(all_clusters$demand,cex=0.2,ylab="Demand")
par(new=TRUE)
par(xpd=FALSE)
abline(h=100,col="red",lwd=3)
abline(h=-100,col="red",lwd=3)
all_clusters[1,]

#remove all rows with demand over 100
all_clusters<-all_clusters[!(all_clusters$demand>100),]
all_clusters<-all_clusters[!(all_clusters$demand<(-100)),]


write.csv(all_clusters, file = "all_clusters.csv")
write.csv(z, file = "cluster_info.csv")


