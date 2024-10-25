# generate climo plots from RAWS data and BP 
# need output from getRAWSdata.R and calcBP.R
# MAC 10/24/24

# Import necessary libraries
import::from(dplyr, mutate, group_by, summarise)
import::from(ggplot2, ggplot, aes, geom_line, geom_point, scale_color_manual, labs, geom_hline, theme_minimal, geom_boxplot, element_text, theme)
import::from(plotly, ggplotly, subplot, layout)
import::from(lubridate, as_date)
import::from(magrittr, `%>%`)

# create function
make_climo_plots<-function(rawsData, bpData, rhThresh){

  ##### plot hourly avg RH -----
  # Custom color palette for 12 months (seasonal theme)
  seasonal_colors <- c(
    "#1f78b4",  # January - Cool blue (Winter)
    "#6a3d9a",  # February - Purple (Winter)
    "#33a02c",  # March - Green (Spring)
    "#b2df8a",  # April - Light green (Spring)
    "#ffff99",  # May - Soft yellow (Spring)
    "#ff7f00",  # June - Orange (Summer)
    "#e31a1c",  # July - Red (Summer)
    "#fb9a99",  # August - Light red (Summer)
    "#b15928",  # September - Brown (Autumn)
    "#fdbf6f",  # October - Light orange (Autumn)
    "#cab2d6",  # November - Light purple (Autumn)
    "#a6cee3"   # December - Light blue (Winter)
  )
  
  # Convert the obs_dt to date format and extract month and hour
  data <- rawsData %>%
    mutate(obs_dt = as.Date(obs_dt, format = "%Y-%m-%d"),
           month = as.factor(as.numeric(format(obs_dt, "%m"))),
           hour = obs_tm)
  
  # Group by month and hour, and calculate the average relative humidity
  hourly_avg_rh <- data %>%
    group_by(month, hour) %>%
    summarise(avg_rh = round(mean(rh, na.rm = TRUE),1))
  
  # Plot using ggplot
  pRH<-ggplot(hourly_avg_rh, aes(x = hour, y = avg_rh, color = month)) +
    geom_line() +
    geom_point() +
    #geom_point(shape = 21, size = 3, color = "black", stroke = 1.2)+
    scale_color_manual(values = seasonal_colors)+
    #scale_color_viridis(option = "cividis")+
    labs(title = "Hourly Average Relative Humidity by Month",
         x = "Hour of Day",
         y = "Average Relative Humidity (%)",
         color = "Month") +
    geom_hline(yintercept = rhThresh)+
    theme_minimal() + theme(axis.title = element_text(size = 11), axis.text = element_text(size = 10))
  pRH<-ggplotly(pRH) %>% layout(showlegend=TRUE)
  ######
  
  ##### plot monthly and yearly BP distributions -----
  # Convert 'month' to a factor to ensure correct ordering in the plot
  bp_data<-bpData[[1]]
  bp_data$month <- factor(bp_data$month, levels = 1:12, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  
  # Generate the box plot using ggplot2
  pMo<-ggplot(bp_data, aes(x = month, y = bp)) +
    geom_boxplot(fill = "#fec44f", outlier.shape = NA) +
    labs(title = "Monthly Box Plots of BP Data",
         x = "Month",
         y = "Hours") +
    scale_y_continuous(breaks = c(0, 6, 12, 18, 24)) +
    theme_minimal() + theme(axis.title = element_text(size = 11), axis.text = element_text(size = 10))
  pMo<-ggplotly(pMo) %>% layout(showlegend=FALSE)
  
  # Generate the yearly box plot using ggplot2
  pYr<-ggplot(bp_data, aes(x = factor(year), y = bp)) +
    geom_boxplot(fill = "#bcbddc",outlier.shape = NA) +
    labs(title = "Yearly Box Plots of BP Data",
         x = "Year",
         y = "Hours") +
    scale_y_continuous(breaks = c(0, 6, 12, 18, 24))+
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    theme(axis.title = element_text(size = 11), axis.text = element_text(size = 10))
  pYr<-ggplotly(pYr) %>% layout(showlegend=FALSE)
  
  # Combine pMo and pYr side by side using subplot
  combined_MoYr <- subplot(pMo, pYr, nrows = 1, shareY = TRUE) %>%
    layout(title = "Burn Period Summaries",
           annotations = list(
             list(
               x = 0.2, 
               y = 1.05, 
               text = "Monthly", 
               showarrow = FALSE, 
               xref='paper', 
               yref='paper'
             ),
             list(
               x = 0.8, 
               y = 1.05, 
               text = "Annual", 
               showarrow = FALSE,
               xref='paper', 
               yref='paper'
             )
           ))
  
  # create list of plots
  climPlots<-list()
  climPlots[[1]]<-pRH
  climPlots[[2]]<-combined_MoYr
    
  # return interactive plot
  return(climPlots)
}





