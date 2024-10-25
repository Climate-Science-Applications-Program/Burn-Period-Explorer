# generate hourly RH plot from RAWS data
# need output from getRAWSdata.R
# MAC 10/24/24


# Assuming you have already loaded your data into a dataframe called 'data' and processed it accordingly
library(ggplot2)
library(dplyr)
library(viridis)

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
p<-ggplot(hourly_avg_rh, aes(x = hour, y = avg_rh, color = month)) +
  geom_line() +
  geom_point() +
  #geom_point(shape = 21, size = 3, color = "black", stroke = 1.2)+
  scale_color_manual(values = seasonal_colors)+
  #scale_color_viridis(option = "cividis")+
  labs(title = "Hourly Average Relative Humidity by Month",
       x = "Hour of Day",
       y = "Average Relative Humidity (%)",
       color = "Month") +
  geom_hline(yintercept = 20)+
  theme_minimal()

plotly::ggplotly(p)
