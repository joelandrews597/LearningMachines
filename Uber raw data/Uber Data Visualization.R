setwd("C://Users//hp//Downloads//Compressed//Uber-dataset//uber-raw-data-aug14.csv")
#install.packages(c("lubridate","DT","scales"))
library(ggplot2)
library(lubridate) #Timeframes
library(scales) #graphical scales
library(dplyr)
library(DT)
library(reshape2)

#Reading data
apr_data <- read.csv("uber-raw-data-apr14.csv")
may_data <- read.csv("uber-raw-data-may14.csv")
jun_data <- read.csv("uber-raw-data-jun14.csv")
jul_data <- read.csv("uber-raw-data-jul14.csv")
aug_data <- read.csv("uber-raw-data-aug14.csv")
sep_data <- read.csv("uber-raw-data-sep14.csv")

data_2014 = rbind(apr_data,may_data, jun_data, jul_data, aug_data, sep_data)
data_2014$Date.Time = as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")
data_2014$Time = format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format = "%H:%M:%S")
data_2014$Date.Time = ymd_hms(data_2014$Date.Time)

data_2014$Day = factor(day(data_2014$Date.Time))
data_2014$Month = factor(month(data_2014$Date.Time, label = TRUE))
data_2014$Year = factor(year(data_2014$Date.Time))
data_2014$DayofWeek = factor(wday(data_2014$Date.Time, label = TRUE))
data_2014$Hour = factor(hour(data_2014$Date.Time))
data_2014$Minute = factor(minute(data_2014$Date.Time))
data_2014$Second = factor(second(data_2014$Date.Time))

#Plotting trips by the hours in a day
hourdata = data_2014 %>% group_by(Hour) %>% summarise(Total = n())
datatable(hourdata)

ggplot(hourdata,aes(x = Hour, y = Total)) + geom_bar(stat = "identity", fill = "#58D68D") +
  ggtitle("Trips Every Hour") + scale_y_continuous(labels = comma)

#Plotting trips by the hours in a day
month_hour = data_2014 %>% group_by(Month,Hour) %>% summarise(Total = n())
datatable(month_hour)  

ggplot(month_hour, aes(x = Hour,y = Total, fill = Month)) + 
  geom_bar(stat = "identity") + ggtitle("Trips by hour every month") +
  scale_y_continuous(labels = comma) 

#Plotting trips by hour of every the month
day_data = data_2014 %>% group_by(Day) %>% summarise(Total = n())
datatable(day_data)

ggplot(day_data, aes(x = Day, y = Total)) + geom_bar(stat = "identity", fill = "#F5B041") +
  ggtitle("Trips per day") + theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

#Plotting trips by day and month
day_month_group = data_2014 %>% group_by(Month, Day) %>% summarize(Total = n())
datatable(day_month_group)

ggplot(day_month_group, aes(Day, Total, fill = Month)) + 
  geom_bar(stat = "identity") + ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma)

#Plotting total trips per month
month_group = data_2014 %>% group_by(Month) %>% summarise(Total = n())
datatable(month_group)

ggplot(month_group, aes(Month, Total, fill = Month)) + geom_bar(stat = "identity") +
  ggtitle("Total trips per month") + scale_y_continuous(labels = comma)

#Plotting trips by day of the week every month
dayofW_month_group = data_2014 %>% group_by(Month, DayofWeek) %>% summarize(Total = n())
datatable(day_month_group)

#position="dodge"
ggplot(dayofW_month_group, aes(Month, Total, fill = DayofWeek)) + 
  geom_bar(position="dodge", stat = "identity") + ggtitle("Trips by Day of the Week and Month") +
  scale_y_continuous(labels = comma)

#Plotting trips by bases
ggplot(data_2014, aes(Base)) +  geom_bar(fill = "#58D68D") +
  scale_y_continuous(labels = comma) + ggtitle("Trips by Bases")

#Plotting trips by bases and month
base_month_group = data_2014 %>% group_by(Base, Month) %>% summarise (Total = n())
datatable(base_month_group)

ggplot(base_month_group, aes(Base, Total, fill = Month)) + geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Trips by base and month") + scale_y_continuous(labels = comma)

#Plotting trips by bases and day of week
ggplot(data_2014, aes(Base, fill = DayofWeek)) + geom_bar(position = "dodge") + 
  ggtitle("Trips by Bases and DayofWeek") + scale_y_continuous(labels = comma)  

#HEAT MAP VISUALISATIONS
#By day and hour
day_and_hour = data_2014 %>% group_by(Day, Hour) %>% summarize(Total = n())
datatable(day_and_hour)

ggplot(day_and_hour, aes(Day, Hour, fill = Total)) +
  geom_tile(color = "white") + ggtitle("Heat Map by Hour and Day")

#Heatmap by Month and Day
ggplot(day_month_group, aes(Day, Month, fill = Total)) +
  geom_tile(color = "white") + ggtitle("Heat Map by Month and Day")

#Heatmap by Month and Day of Week
ggplot(dayofW_month_group, aes(DayofWeek, Month, fill = Total)) +
  geom_tile(color = "white") + ggtitle("Heat Map by Month and Day of Week")

#
month_base = data_2014 %>% group_by(Base, Month) %>% summarize(Total = n()) 
dayofweek_bases = data_2014 %>% group_by(Base, DayofWeek) %>% summarize(Total = n()) 

ggplot(month_base, aes(Base,Month, fill = Total)) +
  geom_tile(color = "white") + ggtitle("Heat Map by Month and Bases")

ggplot(dayofweek_bases, aes(Base, DayofWeek, fill = Total)) +
  geom_tile(color = "white") + ggtitle("Heat Map by Bases and Day of Week")
