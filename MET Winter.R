library(ggplot2)
library(tidyverse)
library(lubridate)

# 1981 - 2010 Snow 
Belmullet <- c(4.5, 4.2, 3.1, 1.4, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.9, 3.0)
Dublin.Airport <- c(4.6, 4.2, 2.8, 1.2, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.8, 2.9)
Valentia <- c(1.0, 0.8, 0.7, 0.3, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.1, 0.7)
Malin.Head <- c(5.1, 5.2, 3.4, 1.6, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 1.1, 3.8)
Rosslare <- c(1.7, 2.3, 1.0, 0.4, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.6)
Birr <- c(3.5, 2.6, 2.5, 0.8, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.2, 1.9)

# Data Preparation
snow30yr<- tibble(Month = factor(month.abb),
                  Belmullet, Dublin.Airport, Valentia, Malin.Head, Rosslare, Birr)

snow30yr.df <- snow30yr%>%
  group_by(Month)%>%
  pivot_longer(cols = c(Belmullet, Dublin.Airport, Valentia, Malin.Head, Rosslare, Birr), 
               values_to = "Snow", names_to = "Station")

# Snow Data Points
ggplot(snow30yr.df, aes(x = Month, y = Snow, group = Station))+
  geom_point(aes(colour = Station))+
  geom_line(aes(colour = Station))+
  scale_x_discrete(limits = month.abb)+
  coord_cartesian(ylim=c(0,7), xlim = c(1, 12)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(0:7)) + 
  theme(legend.position = "none", panel.grid.minor.y = element_blank())

# Smoothed Mean Monthly Snow days
ggplot(snow30yr.df, aes(x=Month,y=Snow, group = Station, na.omit=TRUE)) + 
  geom_smooth(method = "loess", linetype=0, aes(fill = Station), alpha = 0.3)+
  coord_cartesian(ylim=c(0,7), xlim = c(1, 12)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(0:7)) + 
  scale_x_discrete(limits=month.abb)+
  theme_classic()+
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Mean Monthly 1981 - 2010 Snow Days")




# 1981 - 2101 Fog Data
Belmullet <- c(1.0, 0.4, 0.9, 1.4, 1.4, 1.7, 2.9, 1.9, 1.2, 0.7, 0.9, 0.7)	
Dublin.Airport <- c(3.3, 3.1, 3.6, 3.6, 3.4, 2.8, 3.3, 3.8, 4.2, 3.2, 3.1, 4.1)	
Valentia <- c(0.5, 0.3, 0.4, 0.9, 1.2, 1.7, 1.7, 1.3, 0.8, 0.6, 0.5, 0.4)	
Malin.Head <-c(0.4, 0.4, 0.8, 1.3, 1.7, 1.6, 1.6, 1.2, 0.6, 0.1, 0.4, 0.3)	
Rosslare <- c(1.8, 2.2, 3.6, 3.7, 2.9, 4.1, 4.4, 3.4, 2.8, 1.6, 1.7, 1.7)
Birr <- c(2.1, 1.3, 1.1, 1.5, 1.1, 0.8, 1.1, 1.8, 2.5, 2.1, 1.9, 2.9)	
Cork.Airport <- c(7.8, 6.8, 8.5, 7.5, 7.6, 7.6, 8.4, 8.8, 9.1, 8.7, 7.6, 8.4)
Shannon.Airport <- c(3.3, 2.0, 2.1, 1.9, 1.5, 1.4, 1.4, 2.0, 2.9, 2.9, 3.9, 4.2)

# Data Preparation
Fog30yr<- tibble(Month = factor(month.abb),
                 Belmullet, Dublin.Airport, Valentia, Malin.Head, Rosslare, Birr, Cork.Airport, Shannon.Airport)
Fog30yr.df <- Fog30yr%>%
  group_by(Month)%>%
  pivot_longer(cols = c(Belmullet, Dublin.Airport, Valentia, Malin.Head, Rosslare, Birr, Cork.Airport, Shannon.Airport), 
               values_to = "Fog", names_to = "Station")

# Fog Data Points
ggplot(Fog30yr.df, aes(x = Month, y = Fog, group = Station))+
  geom_point(aes(colour = Station), alpha = 0.3)+
  geom_line(aes(colour = Station))+
  coord_cartesian(ylim=c(0,10)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(0:10)) + 
  scale_x_discrete(limits = month.abb)+
  theme(panel.grid.minor.y = element_blank())

# Smoothed Mean Monthly Fog days
ggplot(Fog30yr.df, aes(x=Month,y=Fog, group = Station, na.omit=TRUE)) + 
  geom_smooth(method = "loess", linetype=0, aes(fill = Station), alpha = 0.3)+
  coord_cartesian(ylim=c(0,10)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(0:10)) + 
  scale_x_discrete(limits=month.abb)+
  theme_classic()+
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Mean Monthly 1981 - 2010 Fog Days")
