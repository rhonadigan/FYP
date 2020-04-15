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
data= rbind(data1,data2,data3,data4,data5,data6)

#set column x to null because this is same as index column
data['X']=NULL

#remove duplicates of data
data <-data[!duplicated(data),]

#get df with data from individual stand
stand_1 = data1[data1$number==36,]

stand_1 = stand_1[!duplicated(stand_1$time),]

#time in data is in ms, this puts it to secs
stand_1$time= stand_1$time/1000

#change from unix time to normal time
stand_1$normal_time=as.POSIXct(stand_1$time, origin="1970-01-01")

plot(stand_1$normal_time,stand_1$available_bikes,cex=0.5,main="ST. STEPHEN'S GREEN EAST",ylab="Available bikes",xlab="Time")
lines(lowess(stand_1$available_bikes~stand_1$time,f=1/100,delta=1/100),col="red",lwd=2)

#lowess fits lines, loess can fit line or parabola(default = parabola)
#The smoothing parameter, alpha , is the fraction of the total number n of data points that are used in each local fit
temp = lowess(stand_1$available_bikes~stand_1$time,f=1/100,delta=1/100) #Use higher evaluation for more points

#get rolling slope of average slope of 5 vals before and after each value
slopevals<-data.frame(x=temp$x,y=temp$y)
slopevals$slopes=(get_slopes(slopevals)*3600)#change from bikes per sec to bikes per hour
head(slopevals)

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


##############smooth using pspline smoothing then get 1st derivative of smoothed line = demand###########
pspl <- smooth.Pspline((stand_1$time/3600), stand_1$available_bikes, df=5, method=3)
f0 <- predict(pspl, stand_1$time/3600, nderiv=0)
plot(f0)
f1 <- predict(pspl, stand_1$time/3600, nderiv=1)
plot(stand_1$time/3600, f1, lwd=1, lty=2)
lines(stand_1$time/3600, f1, lwd=3, lty=2, col="red")

########set demand value#############
stand_1$demand=slopevals$slopes

#add varibale which describes whether stand is empty,full or neither
for (i in 1:nrow(stand_1))
{
  if(stand_1$available_bikes[i]==0 )
  {
    stand_1$state[i] = "empty"
    
  }
  else if(stand_1$free_stands[i]==0)
  {
    stand_1$state[i]= "full"
  }
  else
  {
    stand_1$state[i] = "-"
  }
}
###########simple slope##########
#get demand by finding slope of line of available bikes
#should it be slope between 2 points every 10 points or average slope over all points???
#calculate new slope 
y2=30
y1=25

while(y2<nrow(stand_1))
{
  #divide by 60 to get in terms of mins not sceonds
  #not working because often it results in 0/0
  demand = mean(((temp$y[y2-20]-temp$y[y1-20])/(temp$x[y2-20]-temp$x[y1-20])),((temp$y[y2-15]-temp$y[y1-15])/(temp$x[y2-15]-temp$x[y1-15])),((temp$y[y2-10]-temp$y[y1-10])/(temp$x[y2-10]-temp$x[y1-10])),((temp$y[y2-5]-temp$y[y1-5])/(temp$x[y2-5]-temp$x[y1-5])),((temp$y[y2]-temp$y[y1])/(temp$x[y2]-temp$x[y1])),((temp$y[y2+5]-temp$y[y1+5])/(temp$x[y2+5]-temp$x[y1+5])),((temp$y[y2+10]-temp$y[y1+10])/(temp$x[y2+10]-temp$x[y1+10])),((temp$y[y2+15]-temp$y[y1+15])/(temp$x[y2+15]-temp$x[y1+15])),((temp$y[y2+20]-temp$y[y1+20])/(temp$x[y2+20]-temp$x[y1+20])))
  print(demand)
  demand = demand/60
  for(i in 1:5)
  {
    stand_1$demand[y2+i]=demand
  }
  y1=y1+5
  y2=y2+5
}

############forecasting###########
#loop through all times to find when stand is full or empty to predict demand 
x=1
while(x<nrow(stand_1))
{
  if(stand_1$state[x]!="-")
  {
    end_index=1
    while(stand_1$state[x+end_index]!="-")
    {
      #find how long stand stays full/empty
      end_index=end_index+1  
    }
    #make ts variable and predict demand based on all prev data to forecast demand when stand full/empty
    temp_df=stand_1[0:10,] 
    #temp_ts <- ts(temp_df$demand,start=1,end=24,frequency=60,class="ts")
    tts <- xts(temp_df$available_bikes,order.by=as.POSIXct(temp_df$time,origin="1970-01-01"))
    model=auto.arima(tts)
    temp_predict <- predict(model,n.ahead=end_index)
    
    #fill values in for demand 
    for(i in 1:end_index)
    {
      print(paste("forecast ",x))
      stand_1$demand[x+i]=temp_predict$pred[i]
    }
        
    
    x=x+end_index
    
  }
  else
  {
  x=x+1
  }
}

write.csv(stand_1, file = "stand_1")

