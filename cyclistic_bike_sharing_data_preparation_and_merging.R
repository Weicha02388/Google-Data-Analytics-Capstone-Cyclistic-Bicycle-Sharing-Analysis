#My library
library("dplyr")                                                
library("plyr")                                                 
library("readr")
library("tidyverse")
library("tibble")
library("lubridate")
library("janitor")

#To merge all the Csv files into one csv file. Convert un-matched columns into Characters types

#Step 1: Load in the csv files:
setwd('C:/Users/my path....')
apr_20_data <- read.csv('202004-divvy-tripdata.csv')
may_20_data <- read.csv('202005-divvy-tripdata.csv')
jun_20_data <- read.csv('202006-divvy-tripdata.csv')
jul_20_data <- read.csv('202007-divvy-tripdata.csv')
aug_20_data <- read.csv('202008-divvy-tripdata.csv')
sep_20_data <- read.csv('202009-divvy-tripdata.csv')
oct_20_data <- read.csv('202010-divvy-tripdata.csv')
nov_20_data <- read.csv('202011-divvy-tripdata.csv')
dec_20_data <- read.csv('202012-divvy-tripdata.csv')
jan_21_data <- read.csv('202101-divvy-tripdata.csv')
feb_21_data <- read.csv('202102-divvy-tripdata.csv')
mar_21_data <- read.csv('202103-divvy-tripdata.csv')

#Step 2: do a test check on the columns and data types. Ensure they are consistent before merging them into one csv file.
compare_df_cols(apr_20_data,may_20_data,jun_20_data,jul_20_data,aug_20_data,sep_20_data,oct_20_data,nov_20_data,dec_20_data,jan_21_data,feb_21_data,mar_21_data, return = "mismatch")
#Noticed that start_station_id and end_station_id have different data types (integer vs character).

#step3: check for any mis-matched columns
csv_files = list.files(path = '.', pattern = '.csv$')
##use for loop to iterate through each file and store column names in a list.
col_names = list()
for(file in csv_files){
  df = read.csv(file)
  file_name = gsub('.csv$', '', file)
  col_names[[file_name]] = colnames(df)
}

col_names_df = stack(col_names) %>% setNames(., c('column_name', 'file_name'))
write.csv(col_names_df, 'column_names.csv')
## Do a pivot table using excel to check. Result: column names are all aligned. No need to make further changes.
## Remove the column_names.csv file from the directory folder. Else there will be errors running the next set of codes. 

#Step 4: Merge all files and add in pipes to convert the data types to characters.
cyclistic_data <- list.files(path = "C:/Users/my path....", pattern = "*.csv", full.names = TRUE) %>% 
  map_df(~read_csv(.x) %>% 
           mutate(start_station_id= as.character(start_station_id)) %>% #convert to character
           mutate(end_station_id= as.character(end_station_id)) %>% #convert to character
           mutate(started_at = as.POSIXct(started_at, format = "%m/%d/%Y %H:%M:%S")) %>% #error shown in console to convert to datetime format
           mutate(ended_at = as.POSIXct(ended_at, format = "%m/%d/%Y %H:%M:%S"))) %>% #error shownin console to convert to datetime format
  type_convert()

#step 5: calculate length of each trip and filter length greater than 0. Doesn't make sense to analyze trip length less than 0.
revised_cyclistic_data <- cyclistic_data %>%  
  mutate(length_of_trip = cyclistic_data$ended_at-cyclistic_data$started_at) %>% 
  filter(length_of_trip>0)

#Step 6: exclude any missing station names (both starting and ending stations)
revised2_cyclistic_data <- revised_cyclistic_data %>% 
  filter(!is.na(start_station_id)) %>% #filter out blank start station ids
  filter(!is.na(end_station_id)) #filter out blank end station ids

#Step 7: Export into a combined csv. I'll be using tableau to analyze the data. 
write.csv(revised2_cyclistic_data,"combined_trips_cyclistic.csv")
