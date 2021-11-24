library(ggplot2)
library(tidyverse)
library(lubridate)

# 1981 - 2010 Sunshine
Belmullet	<- c(1.4, 2.3, 3.1, 5.2, 6.1, 5.2, 4.4, 4.4, 4.0, 2.8, 1.6, 1.2)
Dublin.Airport <- c(1.9, 2.7, 3.5, 5.3, 6.2, 5.8, 5.3, 5.1, 4.3, 3.3, 2.4, 1.7)
Valentia <- c(1.4, 2.2, 3.0, 5.2, 5.9, 5.3, 4.5, 4.4, 3.9, 2.7, 1.8, 1.3)
Malin.Head <- c(1.2, 2.3, 3.0, 5.1, 6.5, 5.5, 4.6, 4.4, 3.7, 2.6, 1.5, 1.1)
Birr <- c(1.5, 2.2, 2.9, 4.5, 5.1, 4.3, 3.9, 4.0, 3.5, 2.9, 1.9, 1.4)
Rosslare <- c(2.0, 2.6, 3.7, 5.7, 6.9, 6.2, 6.3, 6.0, 4.8, 3.4, 2.4, 1.8)

# Data Preparation
Sun30yr<- tibble(Month = factor(month.abb),
                 Belmullet, Dublin.Airport, Rosslare,
                 Valentia, Malin.Head, Birr)

Sun30yr.df <- Sun30yr%>%
  group_by(Month)%>%
  pivot_longer(cols = c(Belmullet, Dublin.Airport, Rosslare, 
                        Valentia, Malin.Head, Birr), values_to = "SunshineHours",
               names_to = "Station")

# Data Points Sunshine
ggplot(Sun30yr.df, aes(x = Month, y = SunshineHours, group = Station))+
  geom_point(aes(colour = Station))+
  geom_line(aes(colour = Station))+
  coord_cartesian(ylim=c(0,8), xlim = c(1, 12)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(0:8)) +
  scale_x_discrete(limits = month.abb)+
  theme(panel.grid.minor.y = element_blank())

# Smoothed Mean Monthly Sunshine
Sun30yr.df %>% 
  ggplot(aes(x = Month, y = SunshineHours, group = Station))+
  geom_smooth(method='loess', linetype=0, aes(colour = Station, fill = Station), alpha = 0.4) +
  coord_cartesian(ylim=c(0,8), xlim = c(1, 12)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(0:8)) +
  scale_x_discrete(limits = month.abb)+
  theme_classic()+
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Mean Daily 1981 - 2010 Bright Sunshine Hours")
