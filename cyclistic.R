# Setting up the environment
install.packages("tidyverse")
library(tidyverse)
library(lubridate)
library(readr)
library(dplyr)
library(tidyr)
library(hms)

jan_2022 <- read_csv("/home/brian/Downloads/bike trips/2022/202201-divvy-tripdata.csv")
feb_2022 <- read_csv("/home/brian/Downloads/bike trips/2022/202202-divvy-tripdata.csv")
mar_2022 <- read_csv("/home/brian/Downloads/bike trips/2022/202203-divvy-tripdata.csv")
apr_2022 <- read_csv("/home/brian/Downloads/bike trips/2022/202204-divvy-tripdata.csv")
may_2022 <- read_csv("/home/brian/Downloads/bike trips/2022/202205-divvy-tripdata.csv")
jun_2022 <- read_csv("/home/brian/Downloads/bike trips/2022/202206-divvy-tripdata.csv")
jul_2022 <- read_csv("/home/brian/Downloads/bike trips/2022/202207-divvy-tripdata.csv")
aug_2022 <- read_csv("/home/brian/Downloads/bike trips/2022/202208-divvy-tripdata.csv")
sep_2022 <- read_csv("/home/brian/Downloads/bike trips/2022/202209-divvy-publictripdata.csv")
oct_2022 <- read_csv("/home/brian/Downloads/bike trips/2022/202210-divvy-tripdata.csv")
nov_2022 <- read_csv("/home/brian/Downloads/bike trips/2022/202211-divvy-tripdata.csv")
dec_2022 <- read_csv("/home/brian/Downloads/bike trips/2022/202212-divvy-tripdata.csv")

#Merge into one dataframe
trips_df <- rbind(jan_2022, feb_2022, mar_2022, apr_2022, may_2022, jun_2022, jul_2022, aug_2022, sep_2022, oct_2022, nov_2022, dec_2022)

cycle_data <- trips_df

#calculate ride length/duration
cycle_data$ride_length <- difftime(cycle_data$ended_at, cycle_data$started_at, units = "mins")

#Create new date, day_of_the_week, month, time and hour columns
cycle_data$date <- as.Date(cycle_data$started_at) 
cycle_data$day_of_week <- wday(cycle_data$date)
cycle_data$month <- format(as.Date(cycle_data$date), "%m")
cycle_data$time <- as_hms(cycle_data$started_at)
cycle_data$hour <- hour(cycle_data$time)

#-------------------------------CLEANING THE DATA-------------------------------

#check for rows with negative ride_length
cycle_data %>%
  select(ride_id, started_at, ended_at, member_casual, ride_length) %>%
  filter(ride_length < 0) 

#check for records with different start and end dates as well as -ride_length
cycle_data %>%
  mutate(start_date = as.Date(started_at), end_date = as.Date(ended_at)) %>%   
  select(ride_id, started_at, start_date, ended_at, end_date, member_casual, ride_length) %>%   
  filter(start_date != end_date & ride_length < 0)   


cycle_data <- drop_na(cycle_data)   #remove blank rows
cycle_data <- distinct(cycle_data)   #remove duplicate rows
cycle_data <- cycle_data %>% 
  filter(ride_length > 0)   #based on the above analysis

View(cycle_data)


#------------------------------ANALYSIS------------------------------

#number of rows
count(cycle_data)

#rides according to member type
cycle_data %>%
  group_by(member_casual) %>%
  count(member_casual)

#rides according to ride type
cycle_data %>%
  group_by(rideable_type) %>%
  count(rideable_type)

#ride type per membership type
cycle_data %>%
  group_by(member_casual) %>%
  count(rideable_type)

#average time per membership
cycle_data %>%
  group_by(member_casual) %>%
  summarise(mean_ride_length = mean(ride_length))

#average time per ride type
cycle_data %>%
  group_by(rideable_type) %>%
  summarise(mean_time_per_bike = mean(ride_length))


#--------------DAY OF THE WEEK ANALYSIS--------------------
#members usage across the week
cycle_data %>%
  group_by(day_of_week, member_casual) %>%
  count(member_casual)

#ride types across the week
cycle_data %>%
  group_by(day_of_week, rideable_type) %>%
  count(rideable_type)

#average ride_length per day for each bike
cycle_data %>% 
  group_by(day_of_week, rideable_type) %>%
  summarise(avg_rides = mean(ride_length))

#average ride_length per day for each member
cycle_data %>% 
  group_by(day_of_week, member_casual) %>%
  summarise(avg_rides = mean(ride_length))


#---------------------MONTHLY ANALYSIS----------------------------------
#no of rides per month
cycle_data %>% 
  group_by(month) %>%
  count(rideable_type)

#member bike usage per month
cycle_data %>%
  group_by(month) %>%
  count(member_casual)

#average ride_length per month(general)
cycle_data %>%
  group_by(month) %>%
  summarise(mean_rides = mean(ride_length))

#average monthly ride_length per ride type
cycle_data %>%
  group_by(month, rideable_type) %>%
  summarise(mean_bike_time = mean(ride_length))

#average monthly rid_length per member type
cycle_data %>%
  group_by(month, member_casual) %>%
  summarise(mean_bike_time = mean(ride_length))


#------------------HOURLY ANALYSIS---------------
#number of rides per hour
cycle_data %>%
  group_by(cycle_data$hour) %>%
  count(rideable_type)

#member bike usage per hour
cycle_data %>%
  group_by(hour) %>%
  count(member_casual)

#average ride_length per hour(general)
cycle_data %>%
  group_by(hour) %>%
  summarise(avg_rides = mean(ride_length))

#average hourly ride_length per ride type
cycle_data %>%
  group_by(hour, rideable_type) %>%
  summarise(avg_bike_time = mean(ride_length))

#average hourly ride_length per member type
cycle_data %>%
  group_by(hour, member_casual) %>%
  summarise(avg_bike_time = mean(ride_length))


#min/max duration of each bike
cycle_data %>%
  group_by(rideable_type) %>%
  summarise(max(ride_length), min(ride_length))

#max/min duration of each member
cycle_data %>%
  select(ride_id, member_casual, ride_length) %>%
  group_by(member_casual) %>%
  summarise(max(ride_length), min(ride_length))













