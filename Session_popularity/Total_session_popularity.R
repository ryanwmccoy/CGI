library(readxl)
library(dslabs)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(chron)

Session_Popularity_Year1 <- read_excel("O:/CGi/Ryan McCoy/Aventri Analysis/Session_Popularity.xlsx", 
                                       sheet = "2018")
Session_Popularity_Year2 <- read_excel("O:/CGi/Ryan McCoy/Aventri Analysis/Session_Popularity.xlsx", 
                                                             sheet = "2019")


Total_Sessions <- rbind(Session_Popularity_Year1,Session_Popularity_Year2)
#View(Total_Sessions)
#View(Session_Popularity_Year1)
#View(Session_Popularity_Year2)

###THIS WILL BE A FUNCTION THAT WILL PULL THE FILE AND THEN PLACE THE APPROPRIATE INFO###


x <- data.frame (Total_Sessions)
y <- format(as.POSIXct(strptime(Total_Sessions$TimeSlot,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M")
Total_Sessions <- mutate(x,Session_Hours = y)
Total_Sessions


Reordered <- Total_Sessions %>% group_by(Year,Month,Day,Session_Hours)# %>% filter(Day== 25)
Most_attended <- rank(-Reordered$Number)
Most_attended

print(paste0(Reordered$SessionID[which.max(Reordered$Number)]," was the most attended event out of all the sessions."))
print(paste0(Reordered$SessionID[which.max(Reordered$Number)]," captured ",Reordered$X..of.total[which.max(Reordered$X..of.total)]," of the total attendees"))
print(paste0("This means ",Reordered$Number[which.max(Reordered$Number)]," people attended this session"))


#Day_25 <-Session_Popularity %>% group_by(Month,Day,Session_Hours) %>% filter(Day== 25)
#Day_26 <-Session_Popularity %>% group_by(Month,Day,Session_Hours) %>% filter(Day== 26)

#Day_25
#Day_26


Total_Plot <- Reordered %>% ggplot(aes(SessionID,Number, label = SessionID, color = Session_Hours, shape = factor(Year))) + geom_point(size = 3) + ggrepel::geom_label_repel(point.padding = .05,col = "black", size = 3) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Day_26_Plot <- Day_26 %>% ggplot(aes(SessionID,Number, label = SessionID, color = Session_Hours))+ geom_point(size = 3) + ggrepel::geom_label_repel(point.padding = .05,col = "black", size = 3) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

Total_Plot