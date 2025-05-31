library(readr, stringr)
library(lubridate)
library(dplyr)
library(vroom)
library(tidyverse)
library(hms)
library(vctrs)
X202503_divvy_tripdata <- read_csv("202503-divvy-tripdata.csv")
view(X202503_divvy_tripdata)
cyclistic_march2025_df <- data.frame(X202503_divvy_tripdata)
view(cyclistic_march2025_df)
table(is.na(cyclistic_march2025_df$started_at))
table(grepl("^\\d{2}:\\d{2}:\\d{2}$", cyclistic_march2025_df$started_at))
str(cyclistic_march2025_df$started_at)
head(cyclistic_march2025_df$started_at)
str(cyclistic_march2025_df$ended_at)
head(cyclistic_march2025_df$ended_at)

new_march2025_start_date <-cyclistic_march2025_df %>% select(ride_id, rideable_type, started_at, ended_at,start_station_name,start_station_id, end_station_name, end_station_id, start_lat, start_lng, end_lat, end_lng, member_casual)%>% mutate(
  start_month = month(started_at),
  start_day = day(started_at),
  start_year = year(started_at),
  start_time_minutes = hour(started_at)*60+minute(started_at)+(second(started_at)/60)
)
view(new_march2025_start_date)
new_march2025_end_date <-cyclistic_march2025_df %>% select(ride_id, rideable_type, started_at, ended_at,start_station_name,start_station_id, end_station_name, end_station_id, start_lat, start_lng, end_lat, end_lng, member_casual)%>% mutate(
  end_month = month(ended_at),
  end_day = day(ended_at),
  end_year = year(ended_at),
  end_time = hour(ended_at)*60+minute(started_at)+(second(started_at)/60)
)
view(new_march2025_end_date)

#elapsed time in minutes
elapsed_time <- interval(ymd_hms(cyclistic_march2025_df$started_at), ymd_hms(cyclistic_march2025_df$ended_at))
trip_duration<- as.duration(elapsed_time)
T_D <- data.frame(trip_duration)
#view elapsed time in dataframe
class(T_D$trip_duration)
view(T_D)
#merge
merge_df <- new_march2025_start_date|>inner_join(new_march2025_end_date, by = c("ride_id", "rideable_type", "started_at", "ended_at","start_station_name","start_station_id", "end_station_name", "end_station_id", "start_lat", "start_lng", "end_lat", "end_lng", "member_casual"))
view(merge_df)
# adding the column trip duration to the merge_df dataframe
new_march2025_datetime <-  bind_cols(merge_df, Trip_Duration = T_D$trip_duration)
colnames(new_march2025_datetime)<-tools::toTitleCase(colnames(new_march2025_datetime))
view(new_march2025_datetime)
#separating the trip duration column into seconds and minutes
new_march2025_datetime <- new_march2025_datetime%>%separate( Trip_Duration, into= c("seconds", "minutes"), sep = "s \\(~", remove = TRUE) %>% 
  mutate(seconds = round(as.numeric(seconds)), minutes = as.numeric(gsub("minutes\\)", "", minutes))
         )
view(new_march2025_datetime)
#seperating members list and casual list
memberlist_march2025 <- new_march2025_datetime %>% filter(Member_casual == "member")
view(memberlist_march2025)
write_excel_csv(memberlist_march2025, "memberlist_march2025.csv", delim = ",",na = "NA",append = FALSE)
#seperating the casual list
casuallist_march2025 <- new_march2025_datetime %>% filter(Member_casual == "casual")
view(casuallist_march2025)
write_excel_csv(casuallist_march2025, "casuallist_march2025.csv", delim = ",",na = "NA",append = FALSE)
#export to excel csv sheet
write_excel_csv(new_march2025_datetime, "march2025_cyclistic2025",append = FALSE)
#summarise or plot the time taken to travel and whether it is a weekend or weekday
## total number of casuals for 2025
#-------------------------------------------------------------------------#
total_number_casuals<-nrow(casuallist_march2025) 
##85869 casual bike rentals in march 2025
print(total_number_casuals)
##how many bike rentals in weekend and for how long
weekday_casual<- casuallist_march2025 %>% mutate(weekday = wday(as.Date(casuallist_march2025$Started_at), label = TRUE, abbr= TRUE))
view(weekday_casual)
## filter for each day of the week how many bikes were rented
number_of_casuals_onSunday<-weekday_casual %>% filter(weekday_casual$weekday == "Sun")%>%nrow()
print(number_of_casuals_onSunday)
##total occurrences of weekday for every trip in casual list for march2025
weekday_counts <- weekday_casual %>%
  group_by(weekday) %>%
  summarise(count = n())
view(weekday_counts)
print(max(weekday_counts))

