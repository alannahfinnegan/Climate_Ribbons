library(ggplot2)
library(tidyverse)
library(lubridate)

## 1981 - 2010 Rainfall data
Belmullet = c(134.0, 97.1, 99.2, 72.0, 70.4, 72.1, 79.0, 101.9, 101.8, 145.9, 134.0, 137.4)
Dublin.Airport = c(62.6, 48.8, 52.7, 54.1, 59.5, 66.7, 56.2, 73.3, 59.5, 79.0, 72.9, 72.7)
Valentia = c(173.8, 123.7, 123.8, 96.7, 93.5, 95.3, 99.0, 114.9, 125.4, 177.1, 169.3, 164.9)
Birr <- c(78.8, 58.6, 67.4, 55.0, 59.5, 66.5, 59.4, 81.6, 66.4, 94.2, 74.7, 83.8)
Malin.Head <- c(117.4, 84.8, 85.9, 63.1, 56.9, 69.1, 76.8, 93.2, 91.8, 118.4, 104.5, 114.2)
Rosslare <- c(88.4, 70.8, 69.1, 59.1, 55.7, 54.9, 49.9, 71.6, 75.0, 109.3, 100.9, 100.8)

# Data preparation
Rain30yr<- tibble(Month = factor(month.abb),
                  Belmullet, Dublin.Airport, Valentia, Birr, Malin.Head, Rosslare)

Rain30yr.df <- Rain30yr%>%
  group_by(Month)%>%
  pivot_longer(cols = c(Belmullet, Dublin.Airport, Valentia, Birr, Malin.Head, Rosslare), 
               values_to = "Rainfall", names_to = "Station")


# Data Points Rainfall
ggplot(Rain30yr.df, aes(x = Month, y = Rainfall, group = Station))+
  geom_point(aes(colour = Station))+
  geom_line(aes(colour = Station))+
  scale_x_discrete(limits = month.abb)+
  coord_cartesian(ylim=c(0,200), xlim = c(1, 12)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(0, 25, 50, 75, 100, 125, 150, 175, 200)) +
  theme(panel.grid.minor.y = element_blank())

  
# Smoothed Monthly Mean Rainfall
ggplot(Rain30yr.df, aes(x = Month, y = Rainfall, group = Station))+
geom_smooth(method='loess', linetype=0, aes(fill = Station), alpha = 0.4, level=0.95) +
coord_cartesian(ylim=c(0,200), xlim = c(1, 12)) +
scale_y_continuous(expand = c(0, 0), breaks = c(0, 25, 50, 75, 100, 125, 150, 175, 200)) +
scale_x_discrete(limits = month.abb)+
theme_classic()+
theme(axis.title.x = element_blank(), 
      axis.title.y = element_blank(),
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5))+
ggtitle("Mean Monthly 1981 - 2010 Rainfall (mm)")



# Mean Total Yearly Rainfall
YeartotalsRain <- tibble(mean.yeartotal.BEL, mean.yeartotal.BIRR, mean.yeartotal.DUB, 
                         mean.yeartotal.ROS, mean.yeartotal.VAL, mean.yeartotal.Malin)

YeartotalsRain.df <- YeartotalsRain%>%
  pivot_longer(cols = c(mean.yeartotal.BEL, mean.yeartotal.BIRR, mean.yeartotal.DUB, 
                        mean.yeartotal.ROS, mean.yeartotal.VAL, mean.yeartotal.Malin), 
               values_to = "Yearly.Rainfall", names_to = "Station")

StationLabels <- c("Belmullet", "Birr", "Dublin Airport", "Malin head", "Rosslare", "Valentia")

ggplot(YeartotalsRain.df, aes(x = Station, y = Yearly.Rainfall))+
  geom_col(aes(fill = Station), show.legend = F)+
  scale_x_discrete(labels= StationLabels)+
  coord_cartesian(ylim=c(0,1600)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(0, 200, 400, 600, 800, 1000, 1200, 1400, 1600)) +
  theme_classic()+
  ggtitle("Mean Total Yearly Rainfall 1981-2010 (mm)")