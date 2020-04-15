# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import urllib2
import json
from datetime import datetime, timedelta
import sched,time
s = sched.scheduler(time.time, time.sleep)


main_df4=pd.DataFrame(columns=['name','number','status','total_stands','free_stands','available_bikes','time'])
url='https://api.jcdecaux.com/vls/v1/stations?contract=Dublin&apiKey=683573818c1365be248b7221e99ed4c19f05fbf5'
auto_get_data()
 
def get_data():
    data = json.load(urllib2.urlopen(url))
    #set temp lists to store surrent data
    temp_name = []
    temp_number = []
    temp_status=[]
    temp_total_stands=[]
    temp_free_stands=[]
    temp_available_bikes=[]
    temp_time=[]
    
    size =  len(data)
    count=0
    
    #put data into lists
    while count < size:
         temp_name.append(data[count]['name'])
         temp_number.append(data[count]['number'])
         temp_status.append( data[count]['status'])
         temp_total_stands.append(data[count]['bike_stands'])
         temp_free_stands.append(data[count]['available_bike_stands'])
         temp_available_bikes.append(data[count]['available_bikes'])
         temp_time.append(data[count]['last_update'])
         
         count +=1
    
    
#make data frame with lists    
    df4= pd.DataFrame(
        {'name': temp_name,
         'number': temp_number,
         'status': temp_status,
         'total_stands':temp_total_stands,
         'free_stands':temp_free_stands,
         'available_bikes':temp_available_bikes,
         'time':temp_time,
        })

    global main_df4
    main_df4 = main_df4.append(df4, ignore_index=True)

def auto_get_data():
   
    day =0
    
    while(day<7):
        hour = 0
        
        while (hour<24):
        #get data every min for an hour 
            print datetime.now()
            i =0
                
            while(i<60):
                #in 60 secs get data
                s.enter(60,1,get_data,())
                s.run()
                i=i+1
        #save data at end of every hour    
            main_df4.to_csv("dbdata8.csv")
            hour=hour+1
        
        day=day+1
    