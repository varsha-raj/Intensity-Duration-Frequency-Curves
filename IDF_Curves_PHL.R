#Required R packages
pkgs <- c(dplyr, zoo, tidyr, data.table, lubridate)

install.packages(pkgs)


library(dplyr)
library(zoo)
library(tidyr)
library(data.table)
library(lubridate)

#Global environment settings
Sys.setenv(TZ='America/New_york')
options(error=recover)
options(warn=-1)
options(stringsAsFactors=FALSE)
options(scipen=999)

#NOAA PDS-AMS ratios
#NOAA Atlas 14
conversion_table <- data.frame("ARI"=c(2,5,10,25,50, 100), "ratios"=c(1.086,1.023, 1.010, 1.004, 1.004, 1.004))


#Set time length

date_min_sim <- ymd("1899-01-01", tz = "EST")
date_max_sim <- ymd("2016-01-01", tz = "EST")

#filename should be in csv format.
# raw data should hace datetime column name as date.time and precip data should
#have column name as precip

#Some things to improve:
#Work on formatting the data before file is input to the function.

idf_dev <- function(filename) {

#Read PHL end of century simulated hourly rainfall data
df_raw <- read.csv(filename, header= TRUE, row.names = 1)

#Set datetime
df_raw$date.time <- ymd_hms(df_raw$date.time, tz = "America/New_york")

#Data processing
df_raw <- df_raw %>%
  arrange(date.time) %>% 
  filter(
    precip > 0 &
    date.time > date_min_sim &
    date.time < date_max_sim) %>%
  select(date.time, precip)

#Set column names
names(df_raw) <- c("DateTime", "Rainfall")

#Add additional columns, call the dataset df_temp
df_temp <- data.frame(
    DateTime = df_raw$DateTime,
    Rainfall = df_raw$Rainfall,
    Hour = hour(df_raw$DateTime),
    Month = month(df_raw$DateTime),
    Year = year(df_raw$DateTime))

#Create datetime series rounded to the hour
df_temp$DateTime_round_hour <-

ymd_hms(round_date(df_temp$DateTime, unit="hours"),tz='America/New_york')

filled_timeseries <-
  data.frame(DateTime = seq(
    from = min(df_temp$DateTime, na.rm = TRUE),
    to = max(df_temp$DateTime, na.rm = TRUE),
    by =  '1 hour'
  ))

filled_df <-
  merge(df_temp, filled_timeseries, by.x = 'DateTime_round_hour', by.y = 'DateTime', all = TRUE)

#Replace all NA va;ues woth zero
filled_df$Rainfall[is.na(filled_df$Rainfall)] <- 0

#Final data set to work with
final_df <- data.frame(
  DateTime = filled_df$DateTime_round_hour,
  Rainfall_in = filled_df$Rainfall,
  Rainfall_mm =  filled_df$Rainfall * 25.4,
  Hour = hour(filled_df$DateTime_round_hour),
  Month = month(filled_df$DateTime_round_hour),
  Year = year(filled_df$DateTime_round_hour))

#Empty dataframe
master_data <- NULL

#Durations list
x <- list(1,2,3,6,12,24,48)

#Create N hour duration precipitation totals
for(i in 1:length(x)) {

temp_d1 <- final_df %>%
  group_by(Year) %>%
  mutate(n_hr_totals = rollapply(Rainfall_mm, x[[i]] , sum, align='left', fill=0, partial= TRUE)) %>%
  summarize(largest_n_hr_totals = max(n_hr_totals)) %>% ungroup() %>%
  arrange(desc(largest_n_hr_totals))

#Get mean and standard deviation
p_ave <- mean(temp_d1$largest_n_hr_totals)

p_sd <- sd(temp_d1$largest_n_hr_totals)

#Return period array
return_period <- c(2,5,10,25,50,100)

#Empty list
gumbel_freqfactor <- list()

for(k in 1:length(return_period)) {
  gumbel_freqfactor[[k]] = 
  -(sqrt(6)/pi) * (0.5772 + log(log(return_period[k]/(return_period[k]-1))))

#Create a new empty dataframe, with columns return period and gumbel frequency factor
  temp_d2 <- data.frame(return_period=numeric(), gumbel_freqfactor=numeric()) 

  }

#Populate the data frame
temp_d2 <- data.frame(return_period)

temp_d2$gumbel_freqfactor <- do.call('rbind', gumbel_freqfactor)

#Caclulate precipitation frequency depths
temp_d2 <- within(temp_d2, {
  freq_precip_depth_ams = p_ave + gumbel_freqfactor*p_sd
  freq_precip_depth_pds = freq_precip_depth_ams*conversion_table$ratios
  duration_hr = x[[i]]
  ave_intensity_mm_hr = freq_precip_depth_pds/duration_hr 
  })

#Populate master data
master_data <- rbind(master_data, temp_d2)
  
  }

#For graphing puposes
master_data <- master_data %>%
  mutate(random_var = ifelse(duration_hr==1, 'hour', 'hours'),
  duration_hr_label = paste(duration_hr, random_var, sep=" "))

return(master_data)

}

#call the function here
#Specify csv file.name
#file.name = "myfile.csv"
idf_data <- idf_dev(filename = file.name)


