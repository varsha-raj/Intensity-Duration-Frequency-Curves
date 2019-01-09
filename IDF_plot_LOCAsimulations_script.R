pkgs <- c("ggplot2", "grid", "scales", "directlabels", "ggalt")

install.packages(pkgs)
# library(dplyr)
# library(zoo)
# library(tidyr)
# library(data.table)
# library(lubridate)

#Global environment settings
Sys.setenv(TZ='America/New_york')
options(error=recover)
options(warn=-1)
options(stringsAsFactors=FALSE)
options(scipen=999)

#Set working directory
setwd("D:/LOCA/Future_SImulations_1900-2017/outputfiles")

#Read all csv sythetic mutple data and rbind them to create with one big 
#dataset woith all the runs

file_list <- list.files()

ldf <- lapply(file_list , read.csv)

sim_all_masterdata <- do.call("rbind", ldf)

setwd("D:/LOCA/Future_SImulations_1900-2017/merged_data")

#Write to csv
write.csv(sim_all_masterdata, 'master_data_all.csv', quote = FALSE, row.names = FALSE)

#########################

#Packages for making plots:

library(ggplot2)
library(grid)
library(scales)
library(directlabels)
library(ggalt)
options(warn=-1)

#CODE For: Comparing 
#Read merge=
setwd("D:/LOCA/Future_SImulations_1900-2017/merged_data")

master_data_all <- read.csv('master_data_all.csv', header = TRUE)

setwd('D:/LOCA/Current_1900-2017')

master_data <- read.csv('idf_data_current2017.csv', header = TRUE)

plot1 <- ggplot()

plot2 <- plot1 + geom_smooth(data = filter(master_data, duration_hr == 1), 
  method='loess', formula= y~log(x), size = 1, se= FALSE,

	aes(x=return_period, y= ave_intensity_in_hr,

	color= factor(duration_hr))) 

plot3 <- plot2 + geom_smooth(data= filter(master_data_all, duration_hr== 1),

	method='loess', formula= y~log(x), size = 0.5, se= FALSE,

	aes(x=return_period, y= ave_intensity_in_hr, 
	
	group= factor(simulation_number), linetype=''), color='red')


plot4 <- plot3 + scale_linetype_manual(values=c(rep('solid',7)),

    label="Created using future\nPHL time series\n(2080-2100)", name="")

cbPalette <- rep('darkblue', 1)

 
plot5 <- plot4  + scale_color_manual(values=cbPalette,
                                                     name="",
                                                     breaks=c("1"), labels=c("Created using PHL\ntime series (1900-2015)")
           )

plot6 <- plot5 + scale_y_continuous(breaks=seq(1 ,4,0.5), minor_breaks=waiver(), limits=c(1,4), expand=c(0,0))  +
             scale_x_continuous(breaks= seq(0,100,10),expand=c(0,0)) +

             coord_cartesian(xlim=c(0, 112)) #+ geom_abline(v=seq(5,100,5))

plot7 <- plot6+labs(x= "\nAverage Recurrence Interval (years)", y= "Average Intensity (inches/hour)\n")

# plot8a <- plot7 + geom_dl(aes(x= return_period, y= ave_intensity_in_hr, label=master_data$simulation_number), 
  
# 	method= list(dl.trans(x=x+0.2), 'last.bumpup', cex=0.5, fontface='bold', hjust=0.18))
# run_values <- c('run5', 'run57')

# # max_df <- filter(master_data_all, simulation_number %in% c('run51'))
# # min_df <- filter(master_data_all, simulation_number %in% c('run57'))

# filtered_data <- master_data_all %>%

#                 filter(simulation_number %in% run_values) %>%

#                 filter(duration_hr == 1)

# plot8b <- plot7 + geom_dl(data = filter(filtered_data), aes(x= return_period, y= ave_intensity_in_hr,
#   label= simulation_number), 
  
#   method= list(dl.trans(x=x+0.2), 'last.bumpup', cex=0.5, fontface='bold', hjust=0.18))

# plot8 <- plot7 + geom_smooth(data= filtered_data,

#   method='loess', formula= y~log(x), size = 0.5, se= FALSE,

#   aes(x=return_period, y= ave_intensity_in_hr, 
  
#   group= factor(simulation_number), fill=''), color='green', linetype = 'dashed')


windowsFonts(F = windowsFont('Times New Roman'))

plot9 <- plot7 + theme(axis.text.x=element_text(size=10, family="F"),
                       axis.text.y=element_text(size=10, family="F"),
                       axis.title.x=element_text(size=10, family="F"),
                       axis.title.y=element_text(size=10, family="F"),
                       legend.title=element_text(size=10, family="F"),
                       legend.text=element_text(size=9.9, family="F"),
                       plot.title=element_text(size=10, hjust = 0.5, face='bold',family="F"),
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

plot10 <- plot9 +

guides(color= guide_legend(order=1)) + annotate('text', x= 90, y = 3.8, label = 'duration = 1 hour', cex= 2) #, override.aes=list(shape= override.shape, size=3)))

setwd('D:/LOCA')

gt <- ggplot_gtable(ggplot_build(plot10))
gt$layout$clip[gt$layout$name=='panel'] <- 'off'
grid.draw(gt)
png("plotname_n_hour.png", units='in', width=6.5, height=4.5, res=250); plot(gt);dev.off()
