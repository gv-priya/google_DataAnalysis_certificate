library(readr, stringr)
library(lubridate)
library(dplyr)
library(vroom)
library(tidyverse)
library(tidyr)
library(hms)
library(vctrs)
X202504_divvy_tripdata <- read_csv("202504-divvy-tripdata.csv")
view(X202504_divvy_tripdata)
cyclistic_April2025_df <- data.frame(X202504_divvy_tripdata)
view(cyclistic_April2025_df)
table(is.na(cyclistic_April2025_df$started_at))
table(grepl("^\\d{2}:\\d{2}:\\d{2}$", cyclistic_April2025_df$started_at))
str(cyclistic_April2025_df$started_at)
head(cyclistic_April2025_df$started_at)
str(cyclistic_April2025_df$ended_at)
head(cyclistic_April2025_df$ended_at)
#dividing date and time into different columns for start_time and start_date
new_April2025_start_date <-cyclistic_April2025_df %>% select(ride_id, rideable_type, started_at, ended_at,start_station_name,start_station_id, end_station_name, end_station_id, start_lat, start_lng, end_lat, end_lng, member_casual)%>% mutate(
  start_month = month(started_at),
  start_day = day(started_at),
  start_year = year(started_at),
  start_time_minutes = hour(started_at)*60+minute(started_at)+(second(started_at)/60)
)
view(new_April2025_start_date)
#dividing date and time for end_time, end date.
new_April2025_end_date <-cyclistic_April2025_df %>% select(ride_id, rideable_type, started_at, ended_at,start_station_name,start_station_id, end_station_name, end_station_id, start_lat, start_lng, end_lat, end_lng, member_casual)%>% mutate(
  end_month = month(ended_at),
  end_day = day(ended_at),
  end_year = year(ended_at),
  end_time = hour(ended_at)*60+minute(started_at)+(second(started_at)/60)
)
view(new_April2025_end_date)
#elapsed time in minutes
elapsed_time <- interval(ymd_hms(cyclistic_April2025_df$started_at), ymd_hms(cyclistic_April2025_df$ended_at))
trip_duration<- as.duration(elapsed_time)
T_D <- data.frame(trip_duration)
#view elapsed time in dataframe
class(T_D$trip_duration)
view(T_D)
#merge
merge_df <- new_April2025_start_date|>inner_join(new_April2025_end_date, by = c("ride_id", "rideable_type", "started_at", "ended_at","start_station_name","start_station_id", "end_station_name", "end_station_id", "start_lat", "start_lng", "end_lat", "end_lng", "member_casual"))
view(merge_df)
# adding the column trip duration to the merge_df dataframe
new_April2025_datetime <-  bind_cols(merge_df, Trip_Duration = T_D$trip_duration)
colnames(new_April2025_datetime)<-tools::toTitleCase(colnames(new_April2025_datetime))
view(new_April2025_datetime)
#separating the trip duration column into seconds and minutes
new_April2025_datetime <- new_April2025_datetime%>%separate( Trip_Duration, into= c("seconds", "minutes"), sep = "s \\(~", remove = TRUE) %>% 
  mutate(seconds = as.numeric(seconds), minutes = as.numeric(gsub("minutes\\)", "", minutes))
  )
view(new_April2025_datetime)
#Seperating members list and casual list
memberlist_April2025 <- new_April2025_datetime %>% filter(Member_casual == "member")
view(memberlist_April2025)
write_excel_csv(memberlist_April2025, "memberlist_April2025.csv", delim = ",",na = "NA",append = FALSE)
#Seperating the casual list
casuallist_April2025 <- new_April2025_datetime %>% filter(Member_casual == "casual")
view(casuallist_April2025)
write_excel_csv(casuallist_April2025, "casuallist_April2025.csv", delim = ",",na = "NA",append = FALSE)
#export to excel csv sheet
write_excel_csv(new_April2025_datetime, "April2025_cyclistic2025",delim = ",", na="NA", append = FALSE)
##_______________________________________________________________________________________________________________##
#Analysis
##Weekdays for the starting date
weekday_casual <- casuallist_April2025 %>% mutate(weekday = wday(as.Date(casuallist_April2025$Started_at), label = TRUE, abbr = TRUE))
view(weekday_casual)
## Number of bike rentals for each day of the week and number of electric bikes and classic bikes. 
rental_byweekday_biketype <- weekday_casual%>% group_by(weekday, Rideable_type)%>%summarise(count = n())
view(rental_byweekday_biketype)
##total rentals categorized by type of bike for each day in April for casuals
rental_byweekday_biketype_casuals_T <- rental_byweekday_biketype%>% pivot_wider(names_from =Rideable_type, values_from=count) %>% mutate(Total = classic_bike + electric_bike)
print(rental_byweekday_biketype_casuals_T)
##total rental by week day
rental_byday <-weekday_casual%>%group_by(weekday)%>%summarise(Total = n())
view(rental_byday)
##Weekdays for the members_list starting date
weekday_members <- memberlist_April2025 %>% mutate(weekday = wday(as.Date(memberlist_April2025$Started_at), label = TRUE, abbr = TRUE))
view(weekday_members)
##Members list - number of bike rentals in weekdays and weekends. added Rideable_type to count the number of electric and classic bikes
rental_byweekday_biketype_members <- weekday_members%>% group_by(weekday, Rideable_type)%>%summarise(count = n())%>% summarise_at(group_by(weekday_members$weekday)%>%summarise(total = n()))
view(rental_byweekday_biketype_members)
##total_rental by week day for members
rental_byday_members <- weekday_members %>% group_by(weekday)%>%summarise(total = n())
view(rental_byday_members)
class(rental_byday_members)
## creating the totals column into 
rental_byweekday_biketype_member_T <- rental_byweekday_biketype_members%>% pivot_wider(names_from =Rideable_type, values_from=count) %>% mutate(Total = classic_bike + electric_bike)
print(rental_byweekday_biketype_member_T)
##-------------------------------------------------------------------------------------------------------##
## plotting the rental_byweekday for casuals and rental_byweekday_members




