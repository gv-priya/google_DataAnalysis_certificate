library(tidyverse)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr, stringr)
library(ggplot2)
library(vroom)
library("hms")
# the business task given is :
####How do annual members and casual members use Cyclistic bikes differently?
###aim is to recommend and find reasons for marketing strategies 
##audience: Cyclistic executive team.
##data sources used: 
Feb2025_cyclistic_tripdata <- read.csv("C:/Users/vgp_1/OneDrive/Desktop/google_data analysis/project1/202502-divvy-tripdata.csv")
View(Feb2025_cyclistic_tripdata)
####data set for year 2025 from February to May 2025 in csv format. from URL: https://divvy-tripdata.s3.amazonaws.com/index.html
typeof(Feb2025_cyclistic_tripdata ) #list
feb2025_df <- data.frame(Feb2025_cyclistic_tripdata)
view(feb2025_df)
## separating started date and end date into 3 different columns year, day, month
feb2025_df_startdate<-feb2025_df %>% select(ride_id, rideable_type, started_at, ended_at, start_station_name, start_station_id, end_station_name, end_station_id, start_lng, start_lat, end_lng, end_lat, member_casual) %>% mutate(start_Month = month(date(feb2025_df$started_at))) %>% mutate(start_day = day(date(feb2025_df$started_at))) %>% mutate(start_year = year(date(feb2025_df$started_at))) 
feb2025_df_enddate<-feb2025_df %>% select(ride_id, rideable_type, started_at, ended_at, start_station_name, start_station_id, end_station_name, end_station_id, start_lng, start_lat, end_lng, end_lat, member_casual) %>% mutate(end_Month = month(date(feb2025_df$ended_at))) %>% mutate(end_day = day(date(feb2025_df$ended_at))) %>% mutate(end_year = year(date(feb2025_df$ended_at))) 
view(feb2025_df_startdate)
view(feb2025_df_enddate)
#time- morning or AFTERNOON OR EVENING
start_datetime <- ymd_hms(feb2025_df$started_at)
end_datetime<-ymd_hms(feb2025_df$ended_at)
feb2025_df_timeofday <- feb2025_df %>%select(ride_id, rideable_type, started_at, ended_at, start_station_name, start_station_id, end_station_name, end_station_id, start_lng, start_lat, end_lng, end_lat, member_casual) %>% mutate(start_time = format(start_datetime, "%H:%M:%S")) %>% mutate(end_time = format(end_datetime, "%H:%M:%S")) %>% mutate(starttime_AM = am(start_datetime)) %>% mutate(end_time_pm = pm(end_datetime))
view(feb2025_df_timeofday)
##_________________________________________________________________##
#merging the start date and end date columns in the form of month, year, date, time of day column
df_list=list(feb2025_df_startdate,feb2025_df_enddate,feb2025_df_timeofday)
merged_df <- merge(feb2025_df_startdate, merge(feb2025_df_enddate,feb2025_df_timeofday))
view(merged_df)
#removed unwanted columns in merged_df
merged_df <- merged_df %>% select(-starting_date, -ending_date, -start_time, -end_time)
view(merged_df)
#exporting the data frame to csv in the project folder.
write_csv(merged_df, file = "cyclistic_feb2025", na= "NA", append = FALSE)
merged_df$starting_weekday <- weekdays(as.Date(merged_df$starting_date),  abbr = TRUE)
merged_df$ending_weekday <- weekdays(as.Date(merged_df$ending_date), abbr = TRUE)
#removing some columns that did not show the relevant details.
view(merged_df)
#typeof(merged_df$starting_weekday)
### prints TRUE in console. Comparing if 2 columns are the same for weekdays. this means the bikes were used within 1day. bikes were returned the same day.
result <- merged_df$starting_weekday %in% merged_df$ending_weekday
print(result) #True
##---------------------------------------------------------------------------------------------------------------------------------------------------##
#total number of unique bicycles:
n_distinct(merged_df$ride_id)  #151880 num of ride_id
n_distinct(merged_df$rideable_type) # 2 types of bikes
n_distinct(merged_df$start_station_id) #1082 unique starting stations
n_distinct(merged_df$end_station_id) #1063 unique end station id
member_list_feb2025<-merged_df %>% filter(member_casual == 'member')
view(member_list_feb2025)
write_csv(member_list_feb2025, file = "members_list_feb2025.csv",na="NA", append =FALSE)
casual_list_feb2025 <- merged_df %>% filter(member_casual == 'casual')
view(casual_list_feb2025)
write_csv(casual_list_feb2025, file = "casual_list_feb2025.csv",na="NA", append =FALSE)
## find duration of time for each trip
###__________________________________________________________________________________________________##
#elapsed time for each trip for members list
elapsed_time <- interval(ymd_hms(member_list_feb2025$started_at), ymd_hms(member_list_feb2025$ended_at))
trip_duration<- as.duration(elapsed_time)
T_D <- data.frame(trip_duration)
view(T_D)
member_list_feb2025 <- bind_cols(member_list_feb2025, trip_duration = T_D$trip_duration)
view(member_list_feb2025)
##____________________________________________________________________________________________________________________________________________________###
#elapsed time foreach trip in casuals file
trip_duration_casuals <- as.duration( interval(ymd_hms(casual_list_feb2025$started_at), ymd_hms(casual_list_feb2025$ended_at)))
T_D_casuals <- data.frame(trip_duration_casuals)
view(T_D_casuals)
casual_list_feb2025 <- bind_cols(casual_list_feb2025, trip_duration=T_D_casuals$trip_duration_casuals)
view(casual_list_feb2025)
##______________________________________________________________________________________________________________________________####
#separating trip duration columns into minutes and seconds for members list
member_list_feb2025 <- member_list_feb2025%>%separate( trip_duration, into= c("seconds", "minutes"), sep = "s \\(~", remove = TRUE) %>% 
  mutate(seconds_elapsed = as.numeric(seconds), minutes_elapsed = as.numeric(gsub("minutes\\)", "", minutes))
  )
view(member_list_feb2025)
#separating trip duration into minutes and seconds for casuals list
casual_list_feb2025 <- casual_list_feb2025 %>% separate( trip_duration, into= c("seconds", "minutes"), sep = "s \\(~", remove = TRUE) %>% 
  mutate(seconds_elapsed = as.numeric(seconds), minutes_elapsed = as.numeric(gsub("minutes\\)", "", minutes))
  )
view(casual_list_feb2025)
###_________________________________________________________________________________________________________________________###
