# make BP plot
# MAC 10/20/24

library(ggplot2)
library(scales)
library(plotly)

# create helper function for determining years available in bpData

make_BP_plots<-function(bpData, yr){
  
  # yr<-2024
  
  # read in results list from calcBP
  burnHours<-bpData[[1]]
  dayQuantiles<-bpData[[2]]
  
  # get table of available years to plot; subset to selected year
  yrs<-unique(burnHours$year)
  currYr<-yrs[which(yrs==yr)] ### SELECT YEAR
  currBurnHRS<-subset(burnHours,year==currYr)
  
  # build plot label
  plotTitle<-paste0(sub("_", " ", bpData[[3]]$sta_nm), " Daily Burn Period Tracker (Climatology period of record: ",min(bpData[[1]]$year),"-",max(bpData[[1]]$year),")")
  
  # add in dummy dates
  #dumYr<-as.numeric(format(Sys.Date(),"%Y"))
  dumYr<-currYr
  dayQuantiles$date<-as.Date(paste0(dumYr,dayQuantiles$doy),format="%Y %j")
  #currBurnHRS$date<-as.Date(paste0(2016,currBurnHRS$doy),format="%Y %j")
  currBurnHRS$date<-as.Date(paste0(dumYr,"-",currBurnHRS$month,"-",
                                   currBurnHRS$day))
  #temp$dummyDate<-as.Date(paste0(2016,temp$doy),format="%Y %j")
  currBurnHRS$roll_rh_hrs<-zoo::rollmean(currBurnHRS$bp,10,fill=NA, align = 'right')
  
  # better names for Plotly labels
  colnames(dayQuantiles)[c(2,4,9)]<-c("q5th_percentile","q90th_percentile","mean")
  dayQuantiles$mean<-round(dayQuantiles$mean,1)
  colnames(currBurnHRS)[c(3,9)]<-c("Burn_hours","avg_10days")
  
  #####
  # plot with legend data
  #currBurnHRS$var<-"Observed"
  currBurnHRS$var<-paste0(yr," Obs")
  tempBurnHrs<-currBurnHRS[,c("date","Burn_hours","var")]
  colnames(tempBurnHrs)<-c("date","hours","var")
  #fcst_bhrs$var<-"Forecast"
  #tempFcstHrs<-fcst_bhrs[,c("date","Burn_hours_forecast","var")]
  #colnames(tempFcstHrs)<-c("date","hours","var")
  tempDay10avg<-currBurnHRS[,c("date","avg_10days")]
  tempDay10avg$var<-"10-day Avg"
  colnames(tempDay10avg)<-c("date","hours","var")
  plotData<-rbind.data.frame(tempBurnHrs,tempDay10avg)
  
  # make the plot
  barWidth<-1
  p1<-ggplot(dayQuantiles,aes(date,mean))+
    theme_bw()+
    #theme(plot.background = element_blank(),
    #      panel.grid.minor = element_blank(),
    #      panel.grid.major = element_blank(),
    #      panel.border = element_blank(),
    #      panel.background = element_blank()) +
    geom_line(colour='grey',size=0.5)+
    geom_linerange(dayQuantiles, mapping=aes(x=date, ymin=q5th_percentile, ymax=q90th_percentile), colour = "grey83",alpha=0.4, size=barWidth, show.legend = NA)
  # p<-p1 + geom_line(data=currBurnHRS,aes(date,Burn_hours, color=yr), size=0.5) +
  #   scale_colour_manual(values=c("red"),name='Year')+
  #   theme(legend.position=c(0.92,0.75),
  #         legend.title=element_blank(),
  #         legend.background = element_rect(fill=alpha('white', 0)))+
  #   #scale_y_discrete(name ="Burn period (hrs)", 
  #   #                 limits=c(0,4,8,12,16,20,24))+
  #   scale_y_continuous(name ="Burn period (hrs)",breaks=c(0,4,8,12,16,20,24),limits=c(0, 24))+
  #   
  #   scale_x_date(labels = date_format("%b"), date_breaks = "1 month")+
  #   ggtitle(plotTitle)
  
  p<-p1 + geom_line(data=plotData,aes(date,hours, color=var,linetype=var), size=0.5) +
    scale_color_manual(values=c("darkorange4","red"), name=NULL)+
    scale_linetype_manual(values=c("dashed", "solid","solid"), name=NULL)+
    theme(legend.position=c(0.91,0.92),
          #legend.title=element_blank(),
          legend.background = element_rect(fill=alpha('white', 0)))+
    #scale_y_discrete(name ="Burn period (hrs)", 
    #                 limits=c(0,4,8,12,16,20,24))+
    scale_y_continuous(name ="Burn period (hrs)",breaks=c(0,4,8,12,16,20,24),limits=c(0, 24),expand = c(0.01,0))+
    
    scale_x_date(labels = date_format("%b"), date_breaks = "1 month", expand = c(0,0))+
    ggtitle(plotTitle)
  
  # p<-p + geom_line(data=currBurnHRS,aes(date,avg_10days), size=0.5,linetype = "dashed", color="darkorange4") +
  #   #scale_colour_manual(values=c("red"),name='Year')+
  #   theme(legend.position=c(0.92,0.75),
  #         legend.title=element_blank(),
  #         legend.background = element_rect(fill=alpha('white', 0)))+
  #   #scale_y_discrete(name ="Burn period (hrs)", 
  #   #                 limits=c(0,4,8,12,16,20,24))+
  #   scale_y_continuous(name ="Burn period (hrs)",breaks=c(0,4,8,12,16,20,24),limits=c(0, 24))+
  #   
  #   scale_x_date(labels = date_format("%b"), date_breaks = "1 month")+
  #   ggtitle(plotTitle)
  
  # add in forecast line
  #p<-p + geom_line(data=fcst_bhrs, aes(date,Burn_hours_forecast), size=0.5, color="forestgreen")
  
  
  # interactive plot
  pLy<-plotly::ggplotly(p) %>%
    layout(legend = list(x = 0.85, y = 0.99))
  #htmlwidgets::saveWidget(pLy, paste0("/home/crimmins/RProjects/BurnPeriodTracker/plots/plotly/",temp$STA_NAME[1],"_BurnPeriod.html"))
  
  # make list of plot results
  bpPlots<-list()
  bpPlots[[1]]<-p
  bpPlots[[2]]<-pLy

  return(bpPlots)  
  
}






