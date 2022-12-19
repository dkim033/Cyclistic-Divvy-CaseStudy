### Divvy_Exercise_Full_Year_Analysis ###

# This analysis is based on the Divvy case study "'How Does a Bike-Share Navigate Speedy Success?’: Divvy and Data Visualization" written by Kevin Hartman 
# (found here: https://artscience.blog/home/divvy-dataviz-case-study). 
# The purpose of this script is to consolidate downloaded Divvy data into a single dataframe and then conduct simple analysis to help answer the key question: 
# “In what ways do members and casual riders use Divvy bikes differently?”

# Install required packages

# tidyverse for data import and wrangling
install.packages("tidyverse")

# lubridate for date functions
install.packages("lubridate")

# ggplot for visualization
install.packages("ggplot2")

library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data

getwd() #displays working directory
setwd("/cloud/project/Divvy_Exercise") #sets working directory to simplify calls to data

#=====================
# STEP 1: COLLECT DATA
#=====================
# Upload Divvy datasets (csv files)
dec_2021 <- read_csv("202112-divvy-tripdata.csv")
jan_2022 <- read_csv("202201-divvy-tripdata.csv")
feb_2022 <- read_csv("202202-divvy-tripdata.csv")
mar_2022 <- read_csv("202203-divvy-tripdata.csv")
apr_2022 <- read_csv("202204-divvy-tripdata.csv")
may_2022 <- read_csv("202205-divvy-tripdata.csv")
jun_2022 <- read_csv("202206-divvy-tripdata.csv")
jul_2022 <- read_csv("202207-divvy-tripdata.csv")
aug_2022 <- read_csv("202208-divvy-tripdata.csv")
sep_2022 <- read_csv("202209-divvy-tripdata.csv")
oct_2022 <- read_csv("202210-divvy-tripdata.csv")
nov_2022 <- read_csv("202211-divvy-tripdata.csv")

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files
# While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file
colnames(dec_2021)
colnames(jan_2022)
colnames(feb_2022)
colnames(mar_2022)
colnames(apr_2022)
colnames(may_2022)
colnames(jun_2022)
colnames(jul_2022)
colnames(aug_2022)
colnames(sep_2022)
colnames(oct_2022)
colnames(nov_2022)

# Typo found in jul_2022 colnames. Rename "day_pf_week" to "day_of_week"

(jul_2022 <- rename(jul_2022
                   ,day_of_week = day_pf_week))

# Inspect the dataframes and look for incongruencies
str(dec_2021)
str(jan_2022)
str(feb_2022)
str(mar_2022)
str(apr_2022)
str(may_2022)
str(jun_2022)
str(jul_2022)
str(aug_2022)
str(sep_2022)
str(oct_2022)
str(nov_2022)

# Stack individual month's data frames into one big data frame
all_trips <- bind_rows(dec_2021, jan_2022, feb_2022, mar_2022, apr_2022, may_2022, jun_2022, jul_2022, aug_2022, sep_2022, oct_2022, nov_2022)

# Remove lat and long fields as this data was dropped beginning in 2020
all_trips <- all_trips %>% select(-c(start_lat, start_lng, end_lat, end_lng))

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  
tail(all_trips) #Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

# There are a few problems we will need to fix:
# (1) The data can only be aggregated at the ride-level, which is too granular. We will want to add some additional columns of data 
# -- such as day, month, year -- that provide additional opportunities to aggregate the data.
# (2) There are some rides where tripduration shows up as negative, 
# including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.

# Check to make sure the proper number of observations were reassigned
table(all_trips$member_casual)

# Change the column type from character to datetime for started_at and ended_at to perform calculations
all_trips$started_at <- as_datetime(all_trips$started_at, format='%m/%d/%Y %H:%M')
all_trips$ended_at <- as_datetime(all_trips$ended_at, format='%m/%d/%Y %H:%M')

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
all_trips$date <- as_datetime(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as_datetime(all_trips$date), "%m")
all_trips$day <- format(as_datetime(all_trips$date), "%d")
all_trips$year <- format(as_datetime(all_trips$date), "%Y")
all_trips$day_of_week <- format(as_datetime(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Inspect the structure of the columns
str(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

#Remove rows with NA's using na.omit()
all_trips_v2 <- na.omit(all_trips_v2)

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

# Get summary()
summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Noticed that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# The average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

# Visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')

