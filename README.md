# Hi, I'm Lawrence 

## Cyclistic Bike Trip Project
#### Google Data Analytics Capstone: Complete a Case Study


## Setting up my environment 
Notes: setting up my R environment by istalling and loading 'tidyverse', 'conflicted and 'dplyr' packages.
 * tidyverse package helps wrangle data
 * conflicted package help manage conflicts
 * dplyr use the filter and lag as the default choices
 * ggplot2 for visualization

#### Installing and loading packages

```{r installing packages}
install.packages("tidyverse")
install.packages("conflicted")
install.packages("dplyr")
install.packages("ggplot2")
```

```{r loading the packages}
library(tidyverse)
library(conflicted)
library(dplyr)
library(ggplot2)
```

## STEP 1: COLLECT DATA
Uploading the Divvy datasets (csv files) here. The two files

```{r loading divvy datasets}
q1_2019 <- read_csv("Divvy_Trips_2019_Q1.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")
```

## STEP 2: WRANGLING AND TRANFORMING DATASETS INTO ONE
Comparing the column names each of the files. While the names don't have to be in the same order, they need to match perfectly before it can use a command to join them into one file which is my aim.
```{r}
colnames(q1_2019)
colnames(q1_2020)
```
#### Renaming of Column Names
Renaming columns of q1_2019 dataset to make them consistent with q1_2020 (as this will be the supposed going-forward table design for Divvy)

```{r renaming columns}
(q1_2019 <- rename(q1_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name
                   ,end_station_id = to_station_id
                   ,member_casual = usertype
))
```
#### Inspect the dataframes
Inspected the columns and look for inconsistencies 
```{r inspectin for consistency}
str(q1_2019)
str(q1_2020)
```
#### Convertion 
Convert ride_id and rideable_type to character so that they can stack correctly
```{r converting into characters}
q1_2019 <-  mutate(q1_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))
```
 
#### Combining the Datasets 
Combine the two data frames into one big data frame
```{r combining the two data frames}
all_trips <- bind_rows(q1_2019, q1_2020)
```

#### Removing Some Columns  
Remove certain columns not needed for analysis such as lat, long, birthyear, and gender fields.
```{r removing some columns}
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender,  "tripduration"))
```

## STEP 3: CLEANING AND PREPARING DATA FOR ANALYSIS
In this step of cleaning and preparing the data for analysis we need to inspect the new table that has been created by knowing
  * List of column names
  * How many rows are in data frame?
  * Dimensions of the data frame?
  * List of columns and data types (numeric, character, etc)
  * Statistical summary of data mainly for numerics
```{r inspecting the new table}
colnames(all_trips)  
nrow(all_trips)
dim(all_trips)
head(all_trips) 
tail(all_trips)
str(all_trips)  
summary(all_trips)
```

#### Fixing Few Problems
There are a few problems we will need to fix:
  * In the "member_casual" column, there are two names for members ("member" and "Subscriber") and two names for casual riders ("Customer" and "casual"). We will need to consolidate that from four to two labels by replacing "Subscriber" with "member" and "Customer" with "casual"
  * The data can only be aggregated at the ride-level, which is too granular. We will want to add some additional columns of data such as day, month, year -- that provide additional opportunities to aggregate the data.
  * We will want to add a calculated field for length of ride since the 2020Q1 data did not have the "tripduration" column. We will add "ride_length" to the entire dataframe for consistency.
  * There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.
  * Before 2020, Divvy used different labels for these two types of riders.We will want to make our dataframe consistent with their current one
  N.B.: "Level" is a special property of a column that is retained even if a subset does not contain any values from a specific level

#### Begin by seeing how many observations fall under each usertype
```{r}
table(all_trips$member_casual)
```

#### Reassign to the desired values (Using the current 2020 labels)
```{r}
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))
```

#### Check to make sure the proper number of observations were reassigned
```{r}
table(all_trips$member_casual)
```

#### Add columns that list the date, month, day, and year of each ride
This will allow us to aggregate ride data for each month, day, or year before completing these operations we could only aggregate at the ride level
```{r}
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
```

#### Inspect the structure of the columns
```{r}
str(all_trips)
```

#### Add a "ride_length" calculation to all_trips (in seconds)
```{r adding ride_length}
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
```

#### Convert "ride_length" from Factor to numeric so we can run calculations on the data
```{r}
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)
```

#### Removing "bad" data
The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative. We will create a new version of the dataframe (v2) since data is being removed
```{r}
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
```

## STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
Descriptive analysis on ride_length (all figures in seconds).
## Straight average (total ride length / rides)
```{r}
mean(all_trips_v2$ride_length)
```

#### Midpoint number in the ascending array of ride lengths
```{r}
median(all_trips_v2$ride_length)
```

#### Finding the longest and shortest ride
```{r}
max(all_trips_v2$ride_length)
min(all_trips_v2$ride_length)
```

#### Using summary() to see all the four calculations above on the specific attribute
```{r}
summary(all_trips_v2$ride_length)
```

#### Compare members and casual users
```{r}
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)
```

#### Reaaranging the days of the week
```{r}
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
```

#### The average ride time by each day for members vs casual users
```{r}
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
```

#### Analyze ridership data by type and weekday
Creates weekday field using wday(), groups by usertype and weekday.
We calculates the number of rides and average duration and calculates the average duration # sorts
```{r}
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  
  group_by(member_casual, weekday) %>%  
  summarise(number_of_rides = n()							
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)
```
								
#### Visualize the number of rides by rider type
```{r}
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")
```

#### Creating visualization for average duration
```{r}
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
```

## STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
Create a csv file that we will visualize in Excel, Tableau, or my presentation software
```{r}
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')
```
