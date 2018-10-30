pkgs <- c("ggplot2", "grid", "scales", "directlabels", "ggalt")

install.packages(pkgs)

library(ggplot2)
library(grid)
library(scales)
library(directlabels)
library(ggalt)
options(warn=-1)
         

plot1 <- ggplot(master_data)

plot2 <- plot1 + geom_smooth(method='loess', formula= y~log(x), size = 0.5, se= FALSE,

	aes(x=return_period, y= ave_intensity_mm_hr,

	color= factor(duration_hr))) 

plot3 <- plot2 + geom_smooth(data= master_data_sim,

	method='loess', formula= y~log(x), size = 0.5, se= FALSE,

	aes(x=return_period, y= ave_intensity_mm_hr, 
	
	group= factor(duration_hr), linetype=''), color='red')


plot4 <- plot3 + scale_linetype_manual(values=c(rep('solid',7)),

    label="Created using future\nPHL time series\n(2080-2100)", name="")

cbPalette <- rep('darkblue', 7)

 
plot5 <- plot4  + scale_color_manual(values=cbPalette,
                                                     name="",
                                                     breaks=c("","","","","","","48"), labels=c("","","","","","","Created using PHL\ntime series (1900-2015)")
           )

plot6 <- plot5 + scale_y_continuous(breaks=seq(0,70,10), minor_breaks=waiver(), limits=c(0,73), expand=c(0,0))  +
             scale_x_continuous(breaks= seq(0,100,10),expand=c(0,0)) +

             coord_cartesian(xlim=c(0, 110)) #+ geom_abline(v=seq(5,100,5))

plot7 <- plot6+labs(x= "\nAverage Recurrence Interval (years)", y= "Average Intensity (mm/hour)\n")

plot8 <- plot7 + geom_dl(aes(x= return_period, y= ave_intensity_mm_hr, label=master_data$duration_hr_label), 
  
	method= list(dl.trans(x=x+0.2), 'last.bumpup', cex=0.5, fontface='bold', hjust=0.18))

windowsFonts(F = windowsFont('Times New Roman'))

plot9 <- plot8 + theme(axis.text.x=element_text(size=10, family="F"),
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

guides(color= guide_legend(order=1))#, override.aes=list(shape= override.shape, size=3)))

gt <- ggplot_gtable(ggplot_build(plot10))
gt$layout$clip[gt$layout$name=='panel'] <- 'off'
grid.draw(gt)
png("figname.png", units='in', width=6.5, height=4.5, res=250); plot(gt);dev.off()

