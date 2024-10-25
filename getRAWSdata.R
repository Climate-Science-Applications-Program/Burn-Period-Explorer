# download Gap Filled RAWS data from DRI
# MAC 10/17/24


get_RAWS_rh <- function(nwsID) {
 
  # get gapfilled data from CEFA for climo
  #nwsID = NULL
  # example url https://cefa.dri.edu/raws/fw13-gapfilled/020312-20240219.fw13
  # nwsID<-20312 # Hopi
  # nwsID<-21411 # Carr
  # 
  baseUrl = "https://cefa.dri.edu/raws/fw13-gapfilled/"
  # 
  nwsID <- sprintf("%06d", (nwsID))
  # 
  url <- paste0(baseUrl, nwsID, "-20240219.fw13")
  # 
  options(timeout=180)
  tictoc::tic()
  download.file(url, destfile = 'temp.txt')
  tictoc::toc()  
  #####
  
  #####
  # get recent station from WIMs from 2023 to present 
  url<-paste0("https://famprod.nwcg.gov/wims/xsql/obs.xsql?stn=",nwsID,"&sig=&user=&type=&start=01-Jan-2023","&end=31-Dec-",format(Sys.time(), "%Y"),"&time=&sort=&ndays=")
  #stnID<-substr(file_names[i], 4,9)
  #url<-paste0("https://famprod.nwcg.gov/wims/xsql/obs.xsql?stn=",nwsID,"&sig=&type=&start=&end=&time=&sort=asc&ndays=10&user=")
  # past year
  #url<-paste0("https://famprod.nwcg.gov/wims/xsql/obs.xsql?stn=",nwsID,"&sig=&user=&type=&start=01-Jan-","2021","&end=31-Dec-","2021","&time=&sort=&ndays=")
  tictoc::tic()
  xData <- RCurl::getURL(url)
  tictoc::toc()
  xmldoc <- XML::xmlParse(xData)
  wimsData <- XML::xmlToDataFrame(xData) 
  # fix variable types
  wimsData[c(3,4,6,8:9)] <- lapply(wimsData[c(3,4,6,8:9)], as.numeric)
  wimsData$obs_dt<-as.Date(wimsData$obs_dt, format="%m/%d/%Y")
  # trim df
  wimsData<-wimsData[,c(1:6,8:9)]
  #####
  
  ##### process station file 
  # fw13 colnames
  fw13cols<-c("W13",
              "StationNum",
              "Date",
              "Time",
              "ObType",
              "WxCode",
              "Temp",
              "Moisture",
              "WDir",
              "ws",
              "10-fm",
              "MaxTemp",
              "MinTemp",
              "MaxRH",
              "MinRH",
              "PrecipDur",
              "Precip",
              "Wet",
              "HerbGreen",
              "ShrubGreen",
              "MoistureCode",
              "MeasureCode",
              "SeasCode")
  # #####
  # read in saved station climo file
  widths = c(3,6,8,4,1,1,3,3,3,3,2,3,3,3,3,2,5,1,2,2,1,1,1)
  stationData<-readr::read_fwf("temp.txt", col_positions = readr::fwf_widths(widths))
  colnames(stationData)<-fw13cols
  stationData<-tidyr::separate(stationData, Date, c("year","month","day"), sep=c(4,6))
  
  # change value types
  cols = c(3,4,5);
  stationData[,cols] = apply(stationData[,cols], 2, function(x) as.numeric(as.character(x)))
  
  # add in doy
  stationData$doy<-as.numeric(format(as.Date(paste0(stationData$year,"-",stationData$month,"-",stationData$day)),"%j"))
  
  # reformat df
  stationData$date<-as.Date(paste0(stationData$year,"-",stationData$month,
                                   "-",stationData$day))
  stationData$Time<-as.numeric(substr(stationData$Time, 1, 2))
  stationRH<-cbind.data.frame(wimsData[1,c(1:4)],
                              stationData$date,
                              stationData$Time,
                              stationData$Temp,
                              stationData$Moisture)
  colnames(stationRH)<-colnames(wimsData)
  
  # pull out RH from each dataset, combine into single time series
  stationRH<-rbind(stationRH,wimsData)
  
  # order rows by date
  stationRH <- stationRH[order(stationRH$obs_dt), ]
  
  # return full df
  return(stationRH) 
}





 