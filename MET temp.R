library(ggplot2)
library(tidyverse)
library(lubridate)

# 1981 - 2010 Temperature data
Belmullet <-     c(6.3, 6.4, 7.6, 9.0, 11.2, 13.3, 14.9, 15.0, 13.6, 11.1, 8.5, 6.7)
Dublin.Airport <-c(5.3, 5.3, 6.8, 8.3, 10.9, 13.6, 15.6, 15.3, 13.4, 10.5, 7.4, 5.6)
Valentia <-      c(7.3, 7.2, 8.2, 9.4, 11.6, 13.7, 15.4, 15.4, 14.1, 11.7, 9.3, 7.8)
Birr <-          c(5.1, 5.3, 6.8, 8.4, 11.0, 13.6, 15.6, 15.3, 13.2, 10.1, 7.2, 5.6)
Malin.Head <-    c(5.9, 5.8, 6.9, 8.3, 10.5, 12.7, 14.5, 14.7, 13.3, 10.8, 8.2, 6.4)
Rosslare <-      c(6.5, 6.3, 7.5, 8.8, 11.1, 13.6, 15.5, 15.7, 14.2, 11.6, 9.0, 7.4)

#Data preparation
mean.year.BEL = mean(Belmullet)
mean.year.DUB = mean(Dublin.Airport)
mean.year.VAL = mean(Valentia)
mean.year.BIRR = mean(Birr)
mean.year.MH = mean(Malin.Head)
mean.year.ROS = mean(Rosslare)

temp30yr <- tibble(Month = factor(month.abb),
                  Belmullet, Dublin.Airport, Valentia, Birr, Malin.Head, Rosslare)

temp30yr.df <- temp30yr%>%
  group_by(Month)%>%
  pivot_longer(cols = c(Belmullet, Dublin.Airport, Valentia, Birr, Malin.Head, Rosslare), values_to = "Temperature", 
               names_to = "Station")

AVGline <- temp30yr.df %>%
  mutate(yearly_avg = mean(mean.year.BEL, mean.year.DUB, mean.year.VAL, mean.year.BIRR, mean.year.MH, mean.year.ROS))

# Data Points Temperature
ggplot(temp30yr.df, aes(x = Month, y = Temperature, group = Station))+
  geom_point(aes(colour = Station))+
  geom_line(aes(colour = Station))+
  scale_x_discrete(limits = month.abb)+
  scale_y_continuous(expand = c(0, 0), breaks = c(0:17))+
  coord_cartesian(ylim=c(0,17), xlim = c(1, 12)) 

 # Smoothed Mean Monthly Temperature
ggplot(data = temp30yr.df, aes(x = Month, y = Temperature, group = Station))+
geom_smooth(method='loess', linetype=0, aes(fill = Station), alpha = 0.3, level=0.95) +
geom_line(aes(colour = Station))+ 
coord_cartesian(ylim=c(0,17), xlim = c(1, 12)) +
scale_y_continuous(expand = c(0, 0), breaks = c(0:17)) + 
scale_x_discrete(limits = month.abb)+
theme_classic()+
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))+
ggtitle("Mean Monthly 1981 - 2010 Temperature (degrees Celcius)")
