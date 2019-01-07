# Description: 
# This code uses the gumbel method to create IDF data using simulation data. Generated IDF data from each simulation is saved as a csv file
# These files are also merged together to create one large dataset and is saved as both a cxsv file and an excel file.edit

# To make graph, only the maximum and minimum simulations are used and compared against the PHL time series.

# Required packages

library(dplyr)
library(zoo)
library(tidyr)
library(data.table)
library(lubridate)

#Global Environment settings
Sys.setenv(TZ='America/New_york')
options(error=recover)
options(warn=-1)
options(stringsAsFactors=FALSE)
options(scipen=999)

# Convertion ratios - from constrained to unconstrained
conversion_table <- data.frame("ARI"=c(2,5,10,25,50, 100), "ratios"=c(1.086,1.023, 1.010, 1.004, 1.004, 1.004))

#To set time period
date_min_sim <- ymd("1900-01-01", tz = "America/New_york")
date_max_sim <- ymd("2016-01-01", tz = "America/New_york")

### Side Note (VR puproses only): For journal only data until 2015 was used dated:10/12/2017

#Set local working directory
setwd("D:/annual_maxima/1900-2016 simulations/simulation_files")

#read only csv files in ascending of the run number, perfoms IDF cals using gumbel only and throws out stats of each 
#simulatation.


fileNames <- Sys.glob("*.csv")

run_number = 1

for (fileName in fileNames) {


	d_sim <- read.csv(fileName, header= TRUE)

	d_sim$date.time <- ymd_hms(d_sim$date.time, tz = "America/New_york")

    d_sim <- d_sim %>%

           filter(
           pr_in > 0 &
           date.time > date_min_sim &
           date.time < date_max_sim) %>%

           select(date.time, pr_in)

    names(d_sim) <- c("DateTime", "Rainfall")

    sim_data <- data.frame(
    DateTime = d_sim$DateTime,
    Rainfall = d_sim$Rainfall,
    Hour = hour(d_sim$DateTime),
    Month = month(d_sim$DateTime),
    Year = year(d_sim$DateTime))

    rainfall_thresh_sim = min(sim_data$Rainfall)

    sim_data$DateTime_round_hour <-

    ymd_hms(round_date(sim_data$DateTime, unit="hours"),tz='America/New_york')

    sim_full.series <-
    data.frame(DateTime = seq(
    from = min(sim_data$DateTime, na.rm = TRUE),
    to = max(sim_data$DateTime, na.rm = TRUE),
    by =  '1 hour'
  ))

  sim_full_data <-

  merge(sim_data, sim_full.series, by.x = 'DateTime_round_hour', by.y = 'DateTime', all = TRUE)


  sim_full_data$Rainfall[is.na(sim_full_data$Rainfall)] <- 0

  sim_final_data <- data.frame(
    DateTime = sim_full_data$DateTime_round_hour,
    Rainfall = sim_full_data$Rainfall,
    Hour = hour(sim_full_data$DateTime_round_hour),
    Month = month(sim_full_data$DateTime_round_hour),
    Year = year(sim_full_data$DateTime_round_hour))


master_data_sim <- NULL

x <- list(1,2,3,6,12,24,48)

for(i in 1:length(x)) {

temp_d1 <- sim_final_data %>%

group_by(Year) %>%

mutate(n_hr_totals = rollapply(Rainfall, x[[i]] , sum, align='left', fill=0, partial= TRUE)) %>%

summarize(largest_n_hr_totals = max(n_hr_totals)) %>% ungroup() %>%

arrange(desc(largest_n_hr_totals))

p_ave <- mean(temp_d1$largest_n_hr_totals)

p_sd <- sd(temp_d1$largest_n_hr_totals)

# return_period_seq <- seq(10,100,10)

# return_period <- c(2,3,5,return_period_seq)

return_period <- c(2,5,10,25,50,100)


gumbel_freqfactor <- list()

for(k in 1:length(return_period)){

gumbel_freqfactor[[k]] = 

-(sqrt(6)/pi) * (0.5772 + log(log(return_period[k]/(return_period[k]-1))))

temp_d2 <- data.frame(return_period=numeric(), gumbel_freqfactor=numeric())}

temp_d2 <- data.frame(return_period)

temp_d2$gumbel_freqfactor <- do.call('rbind', gumbel_freqfactor)

temp_d2 <- within(temp_d2, {

  freq_precip_depth_ams = p_ave + gumbel_freqfactor*p_sd

  freq_precip_depth_pds = freq_precip_depth_ams*conversion_table$ratios

  duration_hr = x[[i]]

  ave_intensity_in_hr = freq_precip_depth_pds/duration_hr
  })

master_data_sim <- rbind(master_data_sim, temp_d2)



}

master_data_sim$simulation_number <- rep(paste("run", sprintf("%s", run_number, sep="")), nrow(master_data_sim))

#browser()

setwd("D:/IDF_updates_2_7_16/Seb_future_timeseries/3202017_work/annual_maxima/1900-2016 simulations/n_duration_totals")

file_name = paste("master_data_sim", sprintf("%s", run_number), sep="_")
write.csv(master_data_sim, paste(sprintf("%s", file_name), "csv", sep="."),
	row.names= FALSE, quote= FALSE)

run_number = run_number + 1

setwd("D:/IDF_updates_2_7_16/Seb_future_timeseries/3202017_work/annual_maxima/1900-2016 simulations/simulation_files")

}


### to get the max and mean of each run duration totals

library(xlsx)

# setwd("E:/IDF_updates_2_7_16/Seb_future_timeseries/3202017_work/annual_maxima/1900-2016 simulations/n_duration_totals")


# n_duration_Files <- Sys.glob("*.csv")

# run_number = 1

# wb <- createWorkbook()

# for(n_duration_File in n_duration_Files){



# 	n_file <- read.csv(n_duration_File, header=TRUE)


# 	n_file <- n_file %>%

# 	group_by(duration_hr) %>%

# 	summarize(max_precip = max(freq_precip_depth),
		     
# 	          mean_precip = mean(freq_precip_depth)) %>%

# 	mutate(max_intensity= max_precip/duration_hr,

#            mean_intensity = mean_precip/duration_hr)
	         

# setwd("E:/IDF_updates_2_7_16/Seb_future_timeseries/3202017_work/annual_maxima/1900-2016 simulations/n_duration_totals/summary_files")

# sheet <- createSheet(wb , sheetName = sprintf('sheet.%s',run_number))

# addDataFrame(n_file, sheet, startRow=1, startColumn=1, row.names = TRUE)


# run_number= run_number + 1

# setwd("E:/IDF_updates_2_7_16/Seb_future_timeseries/3202017_work/annual_maxima/1900-2016 simulations/n_duration_totals")

# }

# saveWorkbook(wb, 'test.xlsx')

## dont know what I was thinking in the above block

# save data in excel format.

setwd("D:/annual_maxima/1900-2016 simulations/n_duration_totals")


n_duration_Files <- Sys.glob("*.csv")

master_duration_files <- do.call(rbind,lapply(n_duration_Files,read.csv))

write.csv(master_duration_files, 'master_duration_files.csv', row.names= FALSE, quote= FALSE)

library(xlsx)

wb <- createWorkbook()

sheet <- createSheet(wb , sheetName = 'all_data')

addDataFrame(master_duration_files, sheet, startRow=1, startColumn=1, row.names = TRUE)

saveWorkbook(wb, 'master_all_data.xlsx')


## changes on 8/11 -- Converting to metric units. 
setwd("D:/IDF_updates_2_7_16/Seb_future_timeseries/3202017_work/annual_maxima/2080-2100 simulations/n_duration_totals")

master_duration_files <- read.csv('master_duration_files.csv', header= TRUE)

master_duration_files <- master_duration_files %>%

mutate(ave_intensity_mm_hr = ave_intensity_in_hr * 25.4)

setwd("D:/IDF_updates_2_7_16/Seb_future_timeseries/3202017_work/annual_maxima/2080-2100 simulations/n_duration_totals")

#To get PHL time series data
#PHL data represented master data here
master_data <- read.csv('master_data_1900_2015_mm.csv', header= TRUE)

master_data <- master_data %>%

mutate(ave_intensity_mm_hr = ave_intensity_in_hr * 25.4)


library("ggplot2")
         library("grid")
         library("scales")
         #library("ggthemes")
         #library(reshape2)
         library(directlabels)
         library(ggalt)
         #library(mosaic)
         options(warn=-1)
         #library(Hmisc)

#override.shape= c(0,1,2,5,8,10,12,15,16,17) ## shapes not required
#
#adding additional line here to filter only the runs we want
#run_values <- c('run 1', 'run 10') ## current conditions

#Maximum and Minimum simulation runs
run_values <- c('run 5', 'run 3')
 filtered_data <- master_duration_files %>%
 filter(simulation_number %in% run_values) %>%

 filter(duration_hr==1) # Using only 1 houyr duration curves
 ## only seeing max and min values


# level_no <- levels(factor(check_data$simulation_number))

# check_data <- check_data %>%
#group_by(simulation_number) %>%
#mutate(shape_value = as.integer(factor(simulation_number, levels= level_no)))
#filtered_data$shape_value = rep(override.shape, each=13)
#filtered_data$shape_value <- as.factor(filtered_data$shape_value)

#View(check_data)
 


 #plot1 <- ggplot(subset(master_duration_files, duration_hr==1))

 #CODE FOR PLOTTING. 

 plot1 <- ggplot(filtered_data)

         # plot2 <- plot1 + geom_smooth(method='loess', formula= y~log(x), size=1.5, se= FALSE,

         # aes(x=return_period, y= ave_intensity_mm_hr,

         #   color= factor(simulation_number,levels(factor(simulation_number))[c(1, 3, 4, 5,6,
         #     	7,8,9,10, 2)])))  + geom_point(data= filtered_data,

         #      aes(x=return_period, y=ave_intensity_mm_hr, shape=filtered_data$shape_value), show.legend= TRUE, size=2.5

         #   ) + scale_shape_identity()#+ scale_shape_manual(values=(1:nlevels(as.character(filtered_data$shape_value)))%%10)#1, 4, 5,6,
          #   #7,8,9,10, 11, 2,3
          #   
plot2 <- plot1 + geom_smooth(method='loess', formula= y~log(x), size=0.5, se= FALSE,

  aes(x=return_period, y= ave_intensity_mm_hr,

     color= factor(simulation_number)), linetype='dashed')#,levels(factor(simulation_number))[c(1, 3, 4,5,6,
              #7,8,9,10, 2)])))

         #  plot2 <- plot1 + geom_smooth(method='loess', formula= y~log(x), size=1.5, se= FALSE,

         # aes(x=return_period, y= ave_intensity_mm_hr,

         #   color= factor(simulation_number,levels(factor(simulation_number))[c(1, 3, 4,5,6,
         #      7,8,9,10, 2)])))  #+ geom_point(data= filtered_data,

              #aes(x=return_period, y=ave_intensity_mm_hr, shape=filtered_data$shape_value), show.legend= TRUE, size=2.5

           #) + scale_shape_identity()

plot3 <- plot2 + geom_smooth(data= subset(master_data, duration_hr==1),

  method='loess', formula= y~log(x), size=0.5, se= FALSE,

  aes(x=return_period, y= ave_intensity_mm_hr, group= factor(duration_hr), linetype=''), color='black')#, lwd=0.5)


plot4 <- plot3 + scale_linetype_manual(values=c(rep('solid',1)),

  label="Created using PHL\ntime series (1900-2015)", name="")



           # plot_title <- plot4  + scale_color_manual(values=c("#EFF3FF" ,
           #                                                    "#9ECAE1",
           #                                                    "#6BAED6" ,"#4292C6",
           #                                                    "#2171B5",
           #                                                    "#08519C" ,"#08306B"),
           #                                           name="Duration Lines (hours)\nTime Period (1900-2016)",
           #                                           breaks=c("1","2","3","6","12","24","48"), labels=c("1","2","3","6","12","24","48")
           # )
#cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  # plot_title <- plot4  + scale_color_brewer(palette='Paired',
  #                                                name="IDF curves depicting variability in\ncurrent conditions for 1 Hour Duration")#,
                                             #breaks=c("1","2","3","6","12","24","48"), labels=c("1","2","3","6","12","24","48")

  
           #)

# plot_title <- plot4  + scale_color_manual(values= c("#FF0000FF", "#FF2400FF", "#FF4900FF",
#   "#FF6D00FF", "#FF9200FF", "#FFB600FF",
# "#FFDB00FF" ,"#FFFF00FF", "#FFFF40FF", "#FFFFBFFF"),
#                   name="IDF curves depicting variability in\ncurrent conditions for 1 Hour Duration")

plot_title <- plot4  + scale_color_manual(values= c(#"#E1F5FE", "#B3E5FC", "#81D4FA",
  #"#4FC3F7", "#29B6F6", "#03A9F4",
#"#039BE5" ,"#0288D1", "#0277BD", 
#"#01579B","#01579B" 
  'black', 'black'),

  name="", breaks=c("run 5", ""),

  labels=c("Created using stochastically\ngenerated time series\n(2080-2100)",""))

plot5 <- plot_title + scale_y_continuous(breaks=seq(20,70,10), minor_breaks=waiver(), limits=c(20,73), expand=c(0,0))  +
             
  scale_x_continuous(breaks= seq(0,100,10),expand=c(0,0)) +

  coord_cartesian(xlim=c(0, 110))

          #  plot5 <- plot_title + scale_y_continuous(breaks=seq(0,3,0.5), labels=c(0,"",1,"",2,"",3), limits=c(0,3), expand=c(0,0))  +
          #    scale_x_continuous(breaks= c(2,5,10,15,50,100), #labels=c("2", "5", "10", "15",
           #
          #    #"50", "100") ,
           #
          #    expand=c(0,0), limits=c(1,100)) #+
           #
          #    #coord_cartesian(xlim=c(1, 100))

plot6 <- plot5+labs(x= "\nAverage Recurrence Interval (years)", y= "Average Intensity (mm/hour)\n")

windowsFonts(F = windowsFont('Times New Roman'))

           # plot7 <- plot6 + theme(axis.text.x=element_text(size=8),
           #                          axis.text.y=element_text(size=8),
           #                          axis.title.x=element_text(size=8),
           #                          axis.title.y=element_text(size=8),
           #                          legend.title=element_text(size=8),
           #                          legend.text=element_text(size=8, face="bold"),
           #                          plot.title=element_text(size=8, hjust = 0.5, face='bold'),
           #                          #legend.text=element_blank(),
           #                          panel.background = element_rect(fill = "gray90", colour = NA), ##gray80
           #                          legend.key.size = unit(2.5, "cm"),
           #                          legend.key = element_rect(colour = 'gray55', fill='grey80'),
           #                          panel.grid.major = element_line(colour = 'gray21'),
           #                          panel.grid.minor = element_line(colour = 'gray21'),
           #                          axis.ticks = element_line(colour='gray21'),
           #                          #panel.grid.minor = element_blank(),
           #                          legend.key.height=unit(1,"line"),
           #                          legend.key.width=unit(2.5,"line"))
           #                          
plot7 <- plot6 + theme(axis.text.x=element_text(size=8, family="F"),
                       axis.text.y=element_text(size=8, family="F"),
                       axis.title.x=element_text(size=8, family="F"),
                       axis.title.y=element_text(size=8, family="F"),
                       legend.title=element_text(size=8, family="F"),
                       legend.text=element_text(size=8, family="F"),
                       plot.title=element_text(size=8, hjust = 0.5, face='bold',family="F"),
                                    #legend.text=element_blank(),
                       panel.background = element_rect(fill = "white", colour = NA),
                       panel.border = element_rect(fill = NA, color= 'grey20', size=0.4),
                       legend.key.size = unit(0, "cm"),
                       legend.key = element_rect(colour = NA, fill=NA),
                       panel.grid.major = element_line(size=0.2, colour = 'gray80'),
                       #panel.grid.major = element_blank(),
                       panel.grid.minor = element_line(size=0.05, colour = 'gray80'),
                       axis.ticks = element_line(size=0.2, colour='gray60'),
                       #panel.grid.minor = element_blank(),
                       legend.key.height=unit(0.5,"line"),
                       legend.key.width=unit(1,"line"),
                       #legend.spacing.y=unit(-0.5, 'cm'))
                       legend.margin = unit(0.1,'cm'))

# plot8 <- plot7 +
plot8 <- plot7 + guides(linetype=guide_legend(order=1), 

  color= guide_legend(order=2))#, override.aes=list(shape= override.shape, size=3)))

  # plot8 <- plot7 + guides(colour='legend', shape='legend') +

  #   guides(color=guide_legend("IDF curves depicting variability in\ncurrent conditions for 1 Hour Duration"),
  #   shape= guide_legend("IDF curves depicting variability in\ncurrent conditions for 1 Hour Duration"))
 
gt <- ggplot_gtable(ggplot_build(plot8))
      grid.draw(gt)

tiff("IDF_duration1_simulations_PHL.tiff", units= 'in', width=6.5, height=4.5, res=250); plot(gt);dev.off()