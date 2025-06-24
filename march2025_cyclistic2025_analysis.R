
# reading file name"202503-divvy-tripdata.csv"
X202503_divvy_tripdata  <- read.csv(file.choose())
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
#writing csv to excel here
write.xlsx(memberlist_march2025, "memberlist_march2025.xlsx", colNames =TRUE, rowNames =TRUE, sheetName = "memberslist_march2025")
#separating the casual list
casual_list_march2025 <- new_march2025_datetime %>% filter(Member_casual == "casual")
view(casual_list_march2025)
write_excel_csv(casual_list_march2025, "casual_list_march2025.csv", delim = ",",na = "NA",append = FALSE)
#
write.xlsx(casuallist_march2025, "casual_list_march2025.xlsx", colNames = TRUE, rowNames =TRUE, sheetName = "casuallist_march2025")

#export to excel csv sheet
write_excel_csv(new_march2025_datetime, "march2025_cyclistic2025",append = FALSE)
write.xlsx(new_march2025_datetime, "march2025_cyclistic2025.xlsx", colNames = TRUE, rowNames = TRUE, sheetName = "march2025_cyclistic2025")
#summarise or plot the time taken to travel and whether it is a weekend or weekday
## total number of casuals for 2025
#-------------------------------------------------------------------------#
#summarising casual bike rides
total_number_casuals<-nrow(casuallist_march2025) 
##85869 casual bike rentals in march 2025
print(total_number_casuals)
##how many bike rentals in weekend and for how long
weekday_casual<- casuallist_march2025 %>% mutate(weekday = wday(as.Date(casuallist_march2025$Started_at), label = TRUE, abbr= TRUE))
view(weekday_casual)
## filter for each day of the week how many bikes were rented
number_of_casuals_onSunday<-weekday_casual %>% filter(weekday_casual$weekday == "Sun")%>%nrow()
number_of_casuals_onSaturday<-weekday_casual %>% filter(weekday_casual$weekday == "Sat")%>%nrow()
print(number_of_casuals_onSaturday) #20469 casuals were riding a bike on saturday
print(number_of_casuals_onSunday)  #10498 casual bike rides on a sunday
number_of_casuals_onweekends<-weekday_casual %>% filter((weekday_casual$weekday == "Sat") & (weekday_casual$weekday == "Sun"))%>%nrow()
print(number_of_casuals_onweekends) # 0 number of casuals were riding on both saturdays and sundays
##total occurrences of weekday for every trip in casual list for march2025
weekday_counts <- weekday_casual %>%
  group_by(weekday, Rideable_type) %>%
  summarise(count = n())
view(weekday_counts)
print(weekday_counts) #weeks list of casual bike rides
# pivot wider to get separate totals for electric and classic bikes
weekday_counts_casuals <- weekday_counts%>% pivot_wider(names_from = Rideable_type, values_from = count)|> mutate(Total = electric_bike + classic_bike)
write_excel_csv(weekday_counts_casuals, "casuallist_march2025$sheet1",append = TRUE)
write.xlsx(weekday_counts_casuals, "casuallist_march2025.xlsx", colNames = TRUE, rowNames = TRUE, sheetName = "weekday_counts_casuals")
print(weekday_counts_casuals)
##____________________________________________________________________________________________________
#summarizing the members bike rides
total_number_members <- nrow(memberlist_march2025)
print(total_number_members) #212286 total number of members
#how many bike rentals on weekends and for how long
weekday_members <- memberlist_march2025%>% mutate(weekday = wday(as.Date(memberlist_march2025$Started_at), label = TRUE, abbr = TRUE))
view(weekday_members)
number_of_members_onsunday <-weekday_members|> filter(weekday_members$weekday == "Sun")|> nrow()
number_of_members_onsunday_saturday <-weekday_members|> filter((weekday_members$weekday == "Sun") & (weekday_members$weekday == "Sat"))|> nrow()
print(number_of_members_onsunday_saturday) #0 members took a ride on both Saturdays and Sundays in march
print(number_of_members_onsunday) #21569 members took a bike ride on Sunday
number_of_members_onsaturday <-weekday_members|> filter(weekday_members$weekday == "Sat")|> nrow()
print(number_of_members_onsaturday)#30640 members were riding a bike on Saturday in march2025




