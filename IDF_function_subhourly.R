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

date_min<- ymd("1899-12-31", tz = "EST")
date_max <- ymd("2018-01-01", tz = "EST")

#Run function to create IDF data
idf_dev <- function(filename) {

df_raw <- read.csv(filename, header= TRUE)

#Set datetime
df_raw$DateTime <- ymd_hms(df_raw$DateTime, tz = "America/New_york")


#Data processing
df_raw <- df_raw %>%
  arrange(DateTime) %>% 
  filter(
    Rainfall > 0 &
    DateTime > date_min &
    DateTime < date_max) %>%
  select(DateTime, Rainfall)


df_temp <- data.frame(
    DateTime = df_raw$DateTime,
    Rainfall = df_raw$Rainfall,
    #Hour = hour(df_raw$DateTime),
    #Month = month(df_raw$DateTime),
    Year = year(df_raw$DateTime))

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

#Replace all NA values woth zero
filled_df$Rainfall[is.na(filled_df$Rainfall)] <- 0

#Final data set to work with
final_df <- data.frame(
  DateTime = filled_df$DateTime_round_hour,
  Rainfall_in = filled_df$Rainfall,
  Rainfall_mm =  filled_df$Rainfall * 25.4,
  #Hour = hour(filled_df$DateTime_round_hour),
  #Month = month(filled_df$DateTime_round_hour),
  Year = year(filled_df$DateTime_round_hour))

#Empty dataframe
master_data <- NULL

#Durations list
x <- list(1,2,3,6,12,24,48)

#Create N hour duration precipitation totals
for(i in 1:length(x)) {

temp_d1 <- final_df %>%
  group_by(Year) %>%
  mutate(n_hr_totals = rollapply(Rainfall_in, x[[i]] , sum, align='left', fill=0, partial= TRUE)) %>%
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
  ave_intensity_in_hr = freq_precip_depth_pds/duration_hr 
  })

#Populate master data
master_data <- rbind(master_data, temp_d2)
  
  }
#For graphing puposes
master_data <- master_data %>%
  
  group_by(duration_hr) %>%
  
  mutate(random_var = ifelse(duration_hr==1, 'hour', 'hours'),
  duration_hr_label = paste(duration_hr, random_var, sep=" ")) %>% ungroup()

#return(master_data)

hrs <- c(1,2)

master_data_new <- master_data %>%

    filter(duration_hr %in% hrs) %>%

    mutate(duration_min = duration_hr*60,
      freq_precip_depth_pds_conv =

      ifelse(duration_hr==1, freq_precip_depth_pds*1.16, freq_precip_depth_pds*1.05)) %>%

    ungroup()

setwd("D:/IDF_updates_2_7_16/Seb_future_timeseries/Meyers_Curves/1192017")

n_minute_data <- read.csv("n-minute_ratios.csv", header= TRUE)

masterdata_1hr <- master_data_new %>%

    filter(duration_hr==1) %>%

    select(return_period, freq_precip_depth_pds_conv)

temp <- merge(n_minute_data, masterdata_1hr, by='return_period')

temp_new <- temp %>%

  arrange(return_period, duration_min) %>%

  mutate(n_minute_depths = n_minute_ratios*freq_precip_depth_pds_conv) %>%

  select(return_period, duration_min, n_minute_depths) %>% ungroup()

master_data2 <- master_data_new %>%

  select(return_period, duration_min, freq_precip_depth_pds_conv) %>%

  rename(n_minute_depths = freq_precip_depth_pds_conv) %>% ungroup()

f_data <- bind_rows(temp_new, master_data2) %>%

  group_by(return_period) %>%

  ## New addition

  arrange(return_period, duration_min) %>%

  mutate(duration_hr = duration_min/60,
         n_minute_int_in_hr = n_minute_depths/duration_hr,
         inv_n_minute_int_in_hr = 1/n_minute_int_in_hr)

 f_data <- data.frame(f_data)

  return_period_yr <- c(2,5,10,25,50,100)

rainfall_constants = NULL
a_const = NULL
b_const = NULL

for (i in 1:length(return_period_yr)){

  #browser()

  temp_d1 <- subset(f_data, return_period==return_period_yr[i])
  row.names(temp_d1) <- NULL

  n_min_regress <- lm(inv_n_minute_int_in_hr ~ duration_min, data = temp_d1)

a = 1/n_min_regress$coefficients[[2]] 

b= a * n_min_regress$coefficients[[1]]

a_const = rbind(a_const, a)

b_const = rbind(b_const, b) 
}
temp_d2 <- data.frame(return_period_yr=numeric()) 

temp_d2 <- data.frame(return_period_yr)

temp_d2 <- within(temp_d2, {

  rainfall_b_constant = b_const

  rainfall_a_constant = a_const
}) 

rainfall_constants <- rbind(rainfall_constants, temp_d2) %>%

rename(return_period = return_period_yr)

final_data <- merge(f_data, rainfall_constants, by='return_period')

final_data$rainfall_a_constant <- as.numeric(final_data$rainfall_a_constant)

final_data$rainfall_b_constant <- as.numeric(final_data$rainfall_b_constant)

final_data <- final_data %>%

select(return_period, duration_min, duration_hr, rainfall_a_constant, rainfall_b_constant) %>%

mutate(design_int_in_hr = rainfall_a_constant/(rainfall_b_constant + duration_min))

return(final_data)

}

#call the function here
#Specify csv file.name
file.name = "phl_rg2017_final.csv"
idf_data <- idf_dev(filename = file.name)

#Write to csv
write.csv(final_data, 'idf_data_future2017_minutes.csv', row.names = FALSE, quote = FALSE)