library(readxl)
library(dslabs)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(chron)
Session_Popularity <-read_excel("O:/CGi/Ryan McCoy/Aventri Analysis/Session_Popularity.xlsx", sheet = "2018")
#View(Session_Popularity)

x <- data.frame (Session_Popularity)
y <- format(as.POSIXct(strptime(Session_Popularity$TimeSlot,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M")
Session_Popularity <- mutate(x,Session_Hours = y)



Day_25 <-Session_Popularity %>% group_by(Month,Day,Session_Hours) %>% filter(Day== 25)
Day_26 <-Session_Popularity %>% group_by(Month,Day,Session_Hours) %>% filter(Day== 26)

Day_25
Day_26


Day_25_Plot <- Day_25 %>% ggplot(aes(SessionID,Number, label = SessionID, color = Session_Hours))+ geom_point(size = 3) + ggrepel::geom_label_repel(point.padding = .05,col = "black", size = 3) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

Day_26_Plot <- Day_26 %>% ggplot(aes(SessionID,Number, label = SessionID, color = Session_Hours))+ geom_point(size = 3) + ggrepel::geom_label_repel(point.padding = .05,col = "black", size = 3) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

Day_25_Plot
