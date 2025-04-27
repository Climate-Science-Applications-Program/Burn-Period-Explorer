# function to calculate Burn Period with adjustable threshold
# MAC 10/18/24

# import functions
import::from(magrittr, "%>%")
import::from(dplyr, group_by, summarize, n)
import::from(zoo, rollmean)
import::from(tidyr, fill)


calc_bp<-function(rawsData,rhThresh){
  
  # set threshold
  #rhThresh<-20
  #rawsData<-stationRH
  
  # calculate burn period
  burnHRS<-  rawsData %>%
    group_by(obs_dt) %>%
    summarize(n_hours = n(),
              bp = sum(as.numeric(as.character(rh)) <= rhThresh, na.rm = TRUE))
  
  # set bp to NA if hrs<=21
  burnHRS$bp<-ifelse(burnHRS$n_hours<=21, NA, burnHRS$bp)
  
  # complete dates
  dates<-  as.data.frame(seq.Date(min(burnHRS$obs_dt), max(burnHRS$obs_dt), by="day"))
  colnames(dates)<-"date"
  # complete list
  burnHRS<-merge(burnHRS, dates, by.x="obs_dt", by.y="date", all.y=TRUE)
  
  # add in necessary date fields
  burnHRS$doy<-as.numeric(format(burnHRS$obs_dt, "%j"))
  burnHRS$month<-as.numeric(format(burnHRS$obs_dt, "%m"))
  burnHRS$day<-as.numeric(format(burnHRS$obs_dt, "%d"))
  burnHRS$year<-as.numeric(format(burnHRS$obs_dt, "%Y"))
  
  # calculate daily climatologies
  dayQuantiles<- burnHRS %>% group_by(doy) %>% summarize(
    q05 = quantile(bp,0.05,na.rm=TRUE),
    q50 = quantile(bp,0.50,na.rm=TRUE),
    q95 = quantile(bp,0.95,na.rm=TRUE),
    min = min(bp,na.rm=TRUE),
    max = max(bp,na.rm=TRUE),
    avg = mean(bp,na.rm=TRUE),
    n = n())
  
  # moving average
  dayQuantiles$rollAvg<-zoo::rollmean(dayQuantiles$avg,14,fill=NA, align = 'center')
  # pad NAs
  dayQuantiles<-tidyr::fill(dayQuantiles, rollAvg, .direction = "downup")  
  
  # extract metadata
  metaData<-(rawsData[1,1:4])
  
  # return list of results
  bpList<-list()
  bpList[[1]]<-burnHRS
  bpList[[2]]<-dayQuantiles
  bpList[[3]]<-metaData
  return(bpList)
  
}

