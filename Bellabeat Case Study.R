#download all the important libraries that are needed for cleaning/analyzing the data
library(dplyr)
library(here)
library(ggplot2)
library(ggpubr)
library(skimr)
library(janitor)
library(lubridate)
library(tidyverse)


#check the working directory 
print(getwd())

#read all the csv data files into R
daily_activity <- read.csv("/cloud/project/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
heartrate_seconds <- read.csv("/cloud/project/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")
hourly_calories <- read.csv("/cloud/project/Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv")
hourly_intensities <- read.csv("/cloud/project/Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv")
hourly_steps <- read.csv("/cloud/project/Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")
calories_minutes <- read.csv("/cloud/project/Fitabase Data 4.12.16-5.12.16/minuteCaloriesNarrow_merged.csv")
intensities_minutes <- read.csv("/cloud/project/Fitabase Data 4.12.16-5.12.16/minuteIntensitiesNarrow_merged.csv")
MET_minutes <- read.csv("/cloud/project/Fitabase Data 4.12.16-5.12.16/minuteMETsNarrow_merged.csv")
sleep_status_minutes <- read.csv("/cloud/project/Fitabase Data 4.12.16-5.12.16/minuteSleep_merged.csv")
steps_minutes <- read.csv("/cloud/project/Fitabase Data 4.12.16-5.12.16/minuteStepsNarrow_merged.csv")
sleep_day <- read.csv("/cloud/project/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
weight <- read.csv("/cloud/project/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")

#check how all the data is organized 
glimpse(daily_activity)
glimpse(heartrate_seconds)
glimpse(hourly_calories)
glimpse(hourly_intensities)
glimpse(hourly_steps)
glimpse(intensities_minutes)
glimpse(MET_minutes)
glimpse(sleep_status_minutes)
glimpse(steps_minutes)
glimpse(sleep_day)
glimpse(weight)

#count the distinct IDs so we know if we have consistent data points across all data sets
count(distinct(daily_activity,Id)) #33
count(distinct(heartrate_seconds,Id)) #14
count(distinct(hourly_calories,Id)) #33
count(distinct(hourly_intensities,Id)) #33
count(distinct(hourly_steps,Id)) #33
count(distinct(calories_minutes,Id)) #33
count(distinct(intensities_minutes,Id)) #33
count(distinct(MET_minutes,Id)) #33
count(distinct(sleep_status_minutes,Id)) #24
count(distinct(steps_minutes,Id)) #33
count(distinct(sleep_day,Id)) #24
count(distinct(weight,Id)) #8

#remove any duplicate data points (only using these datasets because they contain dates)
anyDuplicated(daily_activity) #0
anyDuplicated(hourly_calories) #0
anyDuplicated(hourly_intensities) #0
anyDuplicated(hourly_steps) #0
anyDuplicated(sleep_day) #162

#remove duplicates and N/A from sleep_day
sleep_day<-sleep_day %>% distinct() %>% drop_na()
anyDuplicated(sleep_day) #0

#clean the data by formatting the date-time formats to correct formats
daily_activity$ActivityDate<- mdy(daily_activity$ActivityDate)
hourly_calories$ActivityHour<-mdy_hms(hourly_calories$ActivityHour)
hourly_intensities$ActivityHour<-mdy_hms(hourly_intensities$ActivityHour)
hourly_steps$ActivityHour<-mdy_hms(hourly_steps$ActivityHour)
sleep_day$SleepDay<-mdy_hms(sleep_day$SleepDay)

#Create a chart about usage time using the daily_activity dataset 
#Creates variable Total_Time where we sum all the minutes from the daily_activity dataset into one variable
daily_activity$Total_Time = rowSums(daily_activity[c("VeryActiveMinutes","FairlyActiveMinutes","LightlyActiveMinutes","SedentaryMinutes")])

#Create a chart in which we have daily use time versus how many users use their devices for that long
daily_activity %>% group_by(Id) %>% 
  summarise(daily_usage_hours = mean(Total_Time/60)) %>% 
  ggplot()+geom_histogram(mapping=aes(x=daily_usage_hours), color="black", fill ="purple") +
                            labs(title="Daily Average Device Usage Time", x = "Daily Use Time (Hours)",y="Amount of Users (Hundreds)")
  
daily_usage<-daily_activity %>% 
  group_by(Id) %>%  
  summarise(daily_usage_hours = mean(Total_Time/60)) %>% 
  mutate(usage = case_when(
    daily_usage_hours >= 20 ~ "high",
    daily_usage_hours < 20 ~ "low"
  ))
                            
daily_use_percent <- daily_usage %>%  
  group_by(usage) %>% 
  summarise(total = n()) %>% 
  mutate(whole_total = sum(total)) %>% 
  group_by(usage) %>% 
  summarise(total_percent = total/whole_total) %>% 
  mutate(labels = scales::percent(total_percent))


daily_use_percent %>% 
  ggplot(aes(x="", y=total_percent, fill = usage))+
  geom_bar(stat = "identity", width =1)+
  coord_polar("y", start=0)+
  theme_minimal()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid =  element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold"))+
  geom_text(aes(label=labels),
            position = position_stack(vjust = 0.5))+
              scale_fill_manual(values = c("#9C06F4","#CF97F0"),
                                labels = c("High Use => 20 hours", "Low Use < 20 hours")) +
              labs(title="Daily Usage Level of Device")

#This graph helps us show the correlation of total steps vs calories
daily_activity %>% 
  ggplot() + (mapping = aes(x= TotalSteps, y = Calories))+ 
  geom_jitter()+
  geom_smooth(color="purple")+
  stat_cor(method="pearson", label.x = 20000, label.y = 2300)+
  labs(title="Daily Steps vs. Calories",x="Daily Steps", y= "Calories")

#This graph helps us show the progression of hourly intensity vs. calories of a user throughout a day
hourly_intensities$day <- format(hourly_intensities$ActivityHour, format = "%Y %m %d")
hourly_intensities$calories <- cbind(hourly_calories$Calories)

hourly_intensities %>% 
  group_by(day) %>% 
  summarise(total_int = TotalIntensity, total_cal = calories) %>% 
  ggplot() + (mapping = aes(x=total_int, y = total_cal)) + geom_jitter()+
  geom_smooth(color="purple")+
  stat_cor(method = "pearson", label.x = 50, label.y =750)+
  labs(title="Hourly Intensity vs. Calories", x = "Hourly Intensity", y = "Calories")

#Create a chart that shows the average sleep time of a user 

sleep_day$weekday <- weekdays(sleep_day$SleepDay)
sleep_day$weekday <- factor(sleep_day$weekday,levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

sleep_day %>% 
  group_by(weekday) %>% 
  summarise(avg_sleep = mean(TotalMinutesAsleep)/60) %>% 
  ggplot()+
  geom_col(mapping=aes(x=weekday, y = avg_sleep), fill = "purple")+
  geom_hline(aes(yintercept = 7)) +
  annotate(geom = "text", x = 3.5, y = 7.5, label = "7 hours of sleep required",
           color = "black") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Average Sleep Time",x = "Weekday",y = "Hours")


#Create a chart to show the Average Time it takes a user to fall asleep

sleep_day$time_to_sleep = (sleep_day$TotalTimeInBed - sleep_day$TotalMinutesAsleep)

sleep_day %>% 
  group_by(weekday) %>% 
  summarise(avg_time_to_sleep = mean(time_to_sleep)) %>% 
  ggplot()+
  geom_col(mapping = aes(x = weekday, y = avg_time_to_sleep), fill = "purple") +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_hline(aes(yintercept = 20)) +
  annotate(geom = "text", x = 3.5, y = 17.5, label = "15 to 20 minutes to fall asleep", color = "black")+
  labs(title = "Average Time to Fall Asleep", x = "Weekday", y= "Minutes")

#Create a chart showing the average total intensity vs the time of the day

hourly_intensities$time <- format(hourly_intensities$ActivityHour, format = "%H:%M:%S")

hourly_intensities %>% 
  group_by(time) %>% 
  summarise(avg_hourly_int = mean(TotalIntensity)) %>% 
  ggplot()+
  geom_col(mapping = aes(x= time, y = avg_hourly_int, fill = avg_hourly_int))+
  scale_fill_gradient(low = "#CF97F0", high = "#9C06F4")+
  theme(axis.text.x = element_text(angle=90))+
  labs(title = "Average Total Intensity vs. Time", x = "Time", y = "Intensity")

#Create a chart showing Average Total Intensity vs the day of the week

hourly_intensities$weekday <- weekdays(hourly_intensities$ActivityHour)

hourly_intensities$weekday <- factor(hourly_intensities$weekday,levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday" ))

hourly_intensities %>% 
  group_by(weekday) %>% 
  summarise(avg_daily_int = mean(TotalIntensity)) %>% 
  ggplot()+
  geom_col(mapping = aes(x=weekday,y=avg_daily_int, fill = avg_daily_int))+
  scale_fill_gradient(low = "#CF97F0", high = "#9C06F4")+
  theme(axis.text.x = element_text(angle=90))+
  labs(title = "Average Total Intensity vs. Day", x = "Day of the Week", y = "Intensity")

#Combine the data from both charts above into one chart 

hourly_intensities$weekday<-weekdays(hourly_intensities$ActivityHour)

hourly_intensities$weekday <- factor(hourly_intensities$weekday,levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday" ))

hourly_intensities$time <- format(hourly_intensities$time, format = "%H:%M:%S")

hourly_intensities %>% 
  group_by(time, weekday) %>% 
  ggplot(hourly_intensities, mapping = aes(x=time, y = weekday, fill = TotalIntensity))+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Most Active Time Slots", x = "Time", y = "Day of the Week")+
  scale_fill_gradient(low = "#CF97F0", high = "darkblue")+
  geom_tile(color = "white", lwd=.6, linetype =1 )+
  coord_fixed()+
  theme(plot.title= element_text(hjust= 0.5,vjust= 0.8, size=16),
        panel.background= element_blank())
