install.packages("tidyverse")
install.packages("lubridate")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("here")
install.packages("skimr")
install.packages("janitor")
install.packages("ggpubr")
install.packages("scales")
install.packages("reshape2")

#load packages
library("tidyverse")
library("lubridate")
library("dplyr")
library("ggplot2")
library("tidyr")
library("here")
library("skimr")
library("janitor")
library("ggpubr")
library("scales")
library("ggrepel")
library("reshape2")

#import datasets
daily_activity <- read_csv("dailyActivity_merged.csv")

daily_calories <- read_csv("dailyCalories_merged.csv")

daily_intensities <- read_csv("dailyIntensities_merged.csv")

daily_steps <- read_csv("dailySteps_merged.csv")

heart_rate_in_secs <- read_csv("heartrate_seconds_merged.csv")

minutes_met <- read_csv("minuteMETSNarrow_merged.csv")

sleep_record_day <- read_csv("sleepDay_merged.csv")

weight_log <- read_csv("weightLogInfo_merged.csv")

#View dataframes using the head, view, colnames and glimpse function

# daily_activity
head(daily_activity)
View(daily_activity)
colnames(daily_activity)
str(daily_activity)
glimpse(daily_activity)

#daily_calories
head(daily_calories)
View(daily_calories)
colnames(daily_calories)
glimpse(daily_calories)

#daily_intensities
head(daily_intensities)
View(daily_intensities)
colnames(daily_intensities)
glimpse(daily_intensities)

#daily_steps
head(daily_steps)
View(daily_steps)
colnames(daily_steps)
glimpse(daily_steps)

#heart_rate_in_secs
head(heart_rate_in_secs)
View(heart_rate_in_secs)
colnames(heart_rate_in_secs)
glimpse(heart_rate_in_secs)

#minutes_met
head(minutes_met)
View(minutes_met)
colnames(minutes_met)
glimpse(minutes_met)

#sleep_record_day
head(sleep_record_day)
View(sleep_record_day)
colnames(sleep_record_day)
glimpse(sleep_record_day)

#weight_log
head(weight_log)
View(weight_log)
colnames(weight_log)
glimpse(weight_log)

#Data Cleaning and Preparation using clean_name function

daily_activity %>%
  clean_names(case = "upper_camel")

daily_calories %>%
  clean_names(case = "upper_camel")

daily_intensities %>%
  clean_names(case = "upper_camel")

daily_steps %>%
  clean_names(case = "upper_camel")

heart_rate_in_secs %>%
  clean_names(case = "upper_camel")

minutes_met %>%
  clean_names(case = "upper_camel")

sleep_record_day %>%
  clean_names(case = "upper_camel")

weight_log %>%
  clean_names(case = "upper_camel")

#Formatting the time stamp for daily_activity and sleep_record_ day, weight_log, heart_rate_secs and minutes_met from character to date.

# Mutate ActivtyHour data type to date / time and create new time, date, month, and weekday columns in df
daily_activity$ActivityDate=as.POSIXct(daily_activity$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
daily_activity$date <- format(daily_activity$ActivityDate, format = "%m/%d/%y")
daily_activity$month <- format(daily_activity$ActivityDate, format = "%B")
daily_activity$day <- format(daily_activity$ActivityDate, format = "%d")
daily_activity$weekday <- format(daily_activity$ActivityDate, format = "%A")

# Mutate weight log data type to date / time and create new time, date, month, and weekday columns in df

weight_log$Date=as.POSIXct(weight_log$Date, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
weight_log$date <- format(weight_log$Date, format = "%m/%d/%y")
weight_log$time <- format(weight_log$Date, format = "%H:%M:%S")
weight_log$month <- format(weight_log$Date, format = "%B")

#Mutate Time data type to date / time and create new date,time and month columns in df
heart_rate_in_secs$Time=as.POSIXct(heart_rate_in_secs$Time, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
heart_rate_in_secs$date <- format(heart_rate_in_secs$Time, format = "%m/%d/%y")
heart_rate_in_secs$time <- format(heart_rate_in_secs$Time, format="%H")
heart_rate_in_secs$month <- format(heart_rate_in_secs$Time, format = "%B")

# Mutate ActivityMinute data type to date / time and create new time, date, month, and weekday columns in df
minutes_met$ActivityMinute=as.POSIXct(minutes_met$ActivityMinute, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
minutes_met$time <- format(minutes_met$ActivityMinute, format = "%H:%M:%S")
minutes_met$date <- format(minutes_met$ActivityMinute, format = "%m/%d/%y")
minutes_met$month <- format(minutes_met$ActivityMinute, format = "%B")
minutes_met$day <- format(minutes_met$ActivityMinute, format = "%d")
minutes_met$weekday <- format(minutes_met$ActivityMinute, format = "%A")
View(minutes_met)

# Mutate Date data type to date / time and create new time, date, month, and weekday columns in df
weight_log$Date=as.POSIXct(weight_log$Date, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
weight_log$date <- format(weight_log$Date, format = "%m/%d/%y")
weight_log$time <- format(weight_log$Date, format = "%H:%M:%S")
weight_log$month <- format(weight_log$Date, format = "%B")

###Fixing Duplicate Values

heart_rate_in_secs %>% distinct()
daily_activity %>% distinct()
minutes_met %>% distinct()
sleep_record_day %>% distinct()
weight_log %>% distinct()

#Analyze
#Summary of the Data
#The n_distinct  and n_row will be used to determine the number of unique values and rows in the required data frames;
n_distinct(heart_rate_in_secs$Id)
n_distinct(daily_activity$Id)
n_distinct(minutes_met$Id)
n_distinct(weight_log$Id)
n_distinct(sleep_record_day$Id)

nrow(heart_rate_in_secs)
nrow(daily_activity)
nrow(minutes_met)
nrow(weight_log)
nrow(sleep_record_day)

#use the summary() function to pull key statistics about the dataframes

# Exploring the daily_activity dataframe
daily_activity %>%
  select(TotalSteps,
         TotalDistance,SedentaryMinutes,LightlyActiveMinutes,FairlyActiveMinutes,
         VeryActiveMinutes,Calories) %>%
  summary()

# Exploring the heart_rate_in_secs dataframe
heart_rate_in_secs %>%
  select(Value) %>%
  summary()

# Exploring the minutes_met dataframe
minutes_met %>%
  select(METs) %>%
  summary()

# Exploring the sleep_record_day dataframe
sleep_record_day %>%
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()

# Exploring the weight_log dataframe
weight_log %>%
  select(WeightPounds, BMI) %>%
  summary()

# Share

#plot a chart showing the distribution of activities daily.
daily_activity %>% count(month, sort = TRUE) %>%
  ggplot() + geom_col(aes(x=month, y=n)) +
  labs(title = "Count of Activities by month", x="month", 
       y="Number of Activities")

#create new dataframe computing the Total Active Minutes 
total_daily_activity <- daily_activity %>%
  mutate(TotalActiveMinutes = VeryActiveMinutes+FairlyActiveMinutes+LightlyActiveMinutes)
# Plot relationship between number of daily steps and minutes active during the day
dailysteps_vs_activeminutes <- ggplot(data=total_daily_activity, aes(x=TotalSteps, y=TotalActiveMinutes)) + 
  geom_point(color="#40916c") + 
  geom_smooth(method = 'loess', formula = y ~ x, span = 0.3) +
  labs(title="Daily Active Minutes vs. Daily Total Steps", 
       x="Total Daily Steps", y="Total Active Time (Minutes)")
dailysteps_vs_activeminutes

# Relationship between Very Active Minutes and the Total Calories Burned
ggplot(data= daily_activity, aes(x=VeryActiveMinutes, y=Calories)) + geom_point() + 
  stat_smooth(method='lm', formula = y ~ x, span = 0.3) + 
  labs(title="Very Active Minutes Vs. Total Daily Calories Burned")

# Relationship between Total Steps and Sedentary Minutes
ggplot(data= daily_activity, 
       aes(x=TotalSteps, y=SedentaryMinutes)) + geom_point() + geom_smooth() + 
  labs(title="Total Steps vs. Sedentary Minutes")


# Plot relationship between time spent in bed and sleep length
plot_sleep <- ggplot(data=sleep_record_day, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + 
  geom_point(color="#014f86") + stat_smooth(method=lm) +
  labs(title="Time Asleep vs. Time In Bed", x="Sleep Length (Minutes)", y="Time In Bed (Minutes)")
plot_sleep

# Relationship between Heart Rate and Time of Day
hourly_hr <- heart_rate_in_secs %>%
  group_by(time) %>%
  drop_na() %>%
  summarise(mean_total_hr = mean(Value))
View(hourly_hr)
# plot bar chart
bar_heart_rate <- ggplot(data=hourly_hr, 
                         aes(x=time, y=mean_total_hr)) + 
  geom_bar(stat = "identity", fill='deepskyblue4') +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title="Heart Rate by Time of Day", x="Time of Day (Hour)", y="Average Heart Rate (BPM)")
bar_heart_rate

# Relationship between Average Metabolic Rate and Day of the Week
# Create new df with the weekday name from the minutes met df
weekday_metabolic_equivalent <- minutes_met %>%
  group_by(weekday) %>%
  drop_na() %>%
  summarise(mean_total_METs = mean(METs))
head(weekday_metabolic_equivalent)

# Create vector for the weekdays
weekday_metabolic_equivalent$weekday <-ordered(weekday_metabolic_equivalent$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                                                              "Friday", "Saturday", "Sunday"))
# Plot relationship between average metabolic rate and day of the week
bar_metabolic_equivalent <- ggplot(data=weekday_metabolic_equivalent, aes(x=weekday, y=mean_total_METs)) + 
  geom_bar(stat = "identity", fill='deepskyblue4') +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title="Metabolic equivalent by Week Day", x="Day of the Week", y="Average Metabolic Equivalent (MET)")
bar_metabolic_equivalent



# Average Hours Asleep Vs. Average Hours in Bed

# Create new df to summarize the average hours asleep, average hours in bed, grouped by the time, and na values dropped
avg_sleep_record <- sleep_record_day %>%
  group_by(Id) %>%
  summarise (mean_hrs_asleep = mean(TotalMinutesAsleep)/60, 
             mean_hrs_in_bed = mean(TotalTimeInBed)/60)
head(avg_sleep_record)

# Create new df to summarize the average hours asleep, average hours in bed, grouped by the time, and na values dropped
sleeper_type_df <- avg_sleep_record %>%
  mutate(sleeper_type = case_when(
    mean_hrs_asleep < 4 ~ "Unhealthy Sleeper",
    mean_hrs_asleep >= 4 & mean_hrs_asleep < 6 ~ "Bad Sleeper", 
    mean_hrs_asleep >= 6 & mean_hrs_asleep < 7 ~ "Normal Sleeper", 
    mean_hrs_asleep >= 7 ~ "Good Sleeper"
  ))
head(sleeper_type_df)

# Create new df showing the percentage of each type of sleeper
percent_sleeper_type <- sleeper_type_df %>%
  group_by(sleeper_type) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(sleeper_type) %>%
  summarise(total_percent = scales::percent(total / totals))
head(percent_sleeper_type)
# Create vector for each type of sleeper
percent_sleeper_type$sleeper_type <- factor(percent_sleeper_type$sleeper_type, levels = c("Good Sleeper", "Normal Sleeper", "Bad Sleeper", "Unhealthy Sleeper"))

# Plot pie chart showing the percentage of sleeper type from the sleeper record day df. 
sleeper_type_chart <- percent_sleeper_type %>%
  ggplot(aes(x="",y=total_percent, fill=sleeper_type)) +
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start=0)+
  theme_minimal()+
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold")) +
  scale_fill_manual(values = c("#0075ab","#aa6fc5","#ff6583","#ffa600"),
                    labels = c("Good Sleeper", "Normal Sleeper", "Bad Sleeper", "Unhealthy Sleeper")) +
  geom_text(aes(label = total_percent), color="white",
            position = position_stack(vjust = 0.5))+
  labs(title="Distribution of Sleeper Type")

head(percent_sleeper_type)

# Dataframe for Average Weight and Average BMI

# Merge daily activity and weight log by id and date, and save as new df

weight_by_bmi <- merge(daily_activity, weight_log, by=c('Id', 'date'))
weight_by_bmi <- weight_by_bmi %>% select(-c("IsManualReport","LogId","WeightKg","month.y"))
glimpse(weight_by_bmi)

# Create new df with average weight in pounds, average BMI, grouped by id, and na values dropped
avg_weight_by_bmi <- weight_by_bmi %>%
  group_by(Id) %>%
  summarise (mean_weight = mean(WeightPounds), 
             mean_bmi = mean(BMI))
head(avg_weight_by_bmi)

# find the mininum and maximum BMI based on the weight log df.
max(weight_by_bmi$BMI)
min(weight_by_bmi$BMI)

# Stats and Visual Representation of Weight Categories.

# Create new df to classify weight type by BMI
bmi_weight_category <- avg_weight_by_bmi %>%
  mutate(bmi_weight = case_when(
    mean_bmi < 18.5 ~ "Underweight",
    mean_bmi >= 18.5 & mean_bmi < 24.9 ~ "Normal Weight", 
    mean_bmi >= 25 & mean_bmi < 29.9 ~ "Overweight", 
    mean_bmi >= 30 ~ "Obese"
  ))

head(bmi_weight_category)

# Create new df to calculate percentage of each type of weight type
percent_bmi_weight_category <- bmi_weight_category %>%
  group_by(bmi_weight) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(bmi_weight) %>%
  summarise(total_percent = scales::percent(total / totals))
head(percent_bmi_weight_category)

# Create vector for each weight type
percent_bmi_weight_category$bmi_weight <- factor(percent_bmi_weight_category$bmi_weight , levels = c("Obese", "Overweight", "Normal Weight", "Underweight"))

# Plot pie chart of percentage distribution of weight type
bmi_chart <- percent_bmi_weight_category %>%
  ggplot(aes(x="",y=total_percent, fill=bmi_weight)) +
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start=0)+
  theme_minimal()+
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold")) +
  scale_fill_manual(values = c("#8173bd","#ffa600","#009d9f"),
                    labels = c("Obese", "Overweight", "Normal Weight", "Underweight")) +
  geom_text(aes(label = total_percent),color="white",
            position = position_stack(vjust = 0.5))+
  labs(title="Distribution of Weight Type")
bmi_chart


# Classification of Wearable Tracker into User Types

# Merge daily activity and sleep record day data by id and date, and save as new df
activity_sleep_data <- merge(daily_activity, sleep_record_day, 
                             by=c ("Id", "date"))
activity_sleep_data <- activity_sleep_data %>% select(-c("month.y"))
glimpse(activity_sleep_data)

# Create new df to classify wearable tracker into different user type
daily_tracker_user_type <- activity_sleep_data %>%
  group_by(Id) %>%
  summarize(days_used=sum(n())) %>%
  mutate(tracker_usage = case_when(
    days_used < 11 ~ "Light Use",
    days_used >= 11 & days_used <= 20 ~ "Moderate Use", 
    days_used >= 21 & days_used <= 31 ~ "High Use", 
  )) %>%
  drop_na()

head(daily_tracker_user_type)

# Visual Representation of Tracker Usage.

# Create new df to calculate the percentage of wearable tracker usage
percent_of_daily_tracker_use <- daily_tracker_user_type %>%
  group_by(tracker_usage) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(tracker_usage) %>%
  summarise(total_percent = scales::percent(total / totals))
head(percent_of_daily_tracker_use)

# Create vector for each wearable tracker user type
percent_of_daily_tracker_use$tracker_usage <- factor(percent_of_daily_tracker_use$tracker_usage , 
                                                     levels = c("High Use", "Moderate Use", "Light Use"))

# Create vector for each wearable tracker user type
percent_of_daily_tracker_use$tracker_usage <- factor(percent_of_daily_tracker_use$tracker_usage , 
                                                     levels = c("High Use", "Moderate Use", "Light Use"))

# Plot bar chart of percentage distribution of wearable tracker based on their user types
daily_tracker_chart <- percent_of_daily_tracker_use %>%
  ggplot(aes(x="",y=total_percent, fill=tracker_usage)) +
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start=0)+
  theme_minimal()+
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold")) +
  scale_fill_manual(values = c("#2a9d8f","#e9c46a","#e76f51"),
                    labels = c("High Use", "Moderate Use", "Light Use")) +
  geom_text(aes(label = total_percent),color="white",
            position = position_stack(vjust = 0.5))+
  labs(title="Distribution of Tracker User Types ")

daily_tracker_chart




