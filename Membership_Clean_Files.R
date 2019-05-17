library(dslabs)
library(dplyr)
library(ggplot2)
library(data.table)
library(plyr)



###Breaking the surveyed data into different blocks to be cleansed
Survey_info <- (initial_data[1:9])
Personal_info <- (initial_data[10:24])
Hospital_options <-(initial_data[25:80])
Survey_questions <-(initial_data[81:121])
Testimonial_info <- (initial_data[122:127])



#### ---------------------Verify that everyone has finished the survey---------------------------
Finished_surveys <- length(Survey_info$Finished[Survey_info$Finished == TRUE])


IS_EVERYONE_FINSIHED <- if(Finished_surveys == length(Survey_info$Finished)){
  print("Everyone has finished")
}else{
  paste(length(Survey_info$Finished)- Finished_surveys, " entries are not completed. Caution before continuing that the data is not complete.")
  
}

IS_EVERYONE_FINSIHED
###Cleaning Block Survey_info
  ###----------------------------------Starting with StartDate---------------------------------------
Last_survey_started_indexed <- Survey_info$StartDate %>% which.max()
Last_survey_started <- Survey_info$StartDate[Last_survey_started_indexed]

First_survey_started_indexed <- Survey_info$StartDate %>% which.min()
First_survey_started <- Survey_info$StartDate[First_survey_started_indexed]

Survey_info.First <- subset(Survey_info,
                            Survey_info$StartDate >= as.POSIXct(First_survey_started,
                                                   tz= "UTC") &
                              Survey_info$StartDate <= as.POSIXct(Last_survey_started,
                                                   tz = "UTC"))

SHx <- data.frame (Survey_info.First)
SHy <- format(as.POSIXct(strptime(Survey_info.First$StartDate,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H")
Start_Hour_reformat<- mutate(SHx,Start_Hour = SHy)#, Start_Minute = Sminy, Start_Year = SYy, Start_Month = Smy, Start_Day = Sdy)

SMinx <- data.frame (Start_Hour_reformat)
SMiny <- format(as.POSIXct(strptime(Survey_info.First$StartDate,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%M")
Start_Min_reformat<- mutate(SMinx,Start_Minute = SMiny)


SYx <- data.frame (Start_Min_reformat)
SYy <- format(as.POSIXct(strptime(Survey_info.First$StartDate,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%Y")
Start_Year_reformat<- mutate(SYx,Start_Year = SYy)


Smx <- data.frame (Start_Year_reformat)
Smy <- format(as.POSIXct(strptime(Survey_info.First$StartDate,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%m")
Start_Month_reformat<- mutate(Smx,Start_Month = Smy)


Sdx <- data.frame (Start_Month_reformat)
Sdy <- format(as.POSIXct(strptime(Survey_info.First$StartDate,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%d")
Start_Day_reformat<- mutate(Sdx,Start_Day = Sdy)

Survey_info.First <- Start_Day_reformat

###------------------------------SEPARATING END DATETIME--------------------------------------------------

EHx <- data.frame (Survey_info.First)
EHy <- format(as.POSIXct(strptime(Survey_info.First$EndDate,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H")
End_Hour_reformat<- mutate(EHx,End_Hour = EHy)

EMinx <- data.frame (End_Hour_reformat)
EMiny <- format(as.POSIXct(strptime(Survey_info.First$EndDate,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%M")
End_Min_reformat<- mutate(EMinx,End_Minute = EMiny)


EYx <- data.frame (End_Min_reformat)
EYy <- format(as.POSIXct(strptime(Survey_info.First$EndDate,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%Y")
End_Year_reformat<- mutate(EYx,End_Year = EYy)


Emx <- data.frame (End_Year_reformat)
Emy <- format(as.POSIXct(strptime(Survey_info.First$EndDate,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%m")
End_Month_reformat<- mutate(Emx,End_Month = Emy)


Edx <- data.frame (End_Month_reformat)
Edy <- format(as.POSIXct(strptime(Survey_info.First$EndDate,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%d")
End_Day_reformat<- mutate(Edx,End_Day = Edy)

Survey_info.First <- End_Day_reformat
head(Survey_info.First)



Start_time_graph <- Survey_info.First %>% ggplot(aes(Start_Day,Start_Hour))
Start_time_graph


Numeric_Hour <- as.numeric(Survey_info.First$Start_Hour)
Numeric_Day <- as.numeric(Survey_info.First$End_Hour)
Numberic_Duration <- as.numeric(Survey_info.First$Duration..in.seconds.)

cor(Numeric_Day,Numberic_Duration)

Average_Start_Hour <- mean(as.numeric(Survey_info.First$Start_Hour), na.rm = TRUE)
Average_End_Hour <- mean(as.numeric(Survey_info.First$End_Hour), na.rm = TRUE)
Average_Start_Hour
Average_End_Hour

Median_Start_Hour <- median.default(as.numeric(Survey_info.First$Start_Hour), na.rm = TRUE)
Median_End_Hour <- median.default(as.numeric(Survey_info.First$End_Hour), na.rm = TRUE)
Median_Start_Hour
Median_End_Hour

Average_Duration <- mean(as.numeric(Survey_info.First$Duration..in.seconds.), na.rm = TRUE)
Average_Duration

Median_Duration <- median.default(as.numeric(Survey_info.First$Duration..in.seconds.), na.rm = TRUE)
Median_Duration

write.csv(Survey_info.First,
          file = "2019_Membership_Survey_info_Start_&_End_times.csv")

  ###Distribution of Start Times###


###--------------------------------Duration of Survey----------------------------------
#Survey_info$`Duration (in seconds)` <- as.numeric(Survey_info$`Duration (in seconds)`,na.rm= TRUE)
#Average_Duration <- mean(Survey_info$`Duration (in seconds)`,na.rm = TRUE)
#Average_Duration /60
#min(Survey_info$`Duration (in seconds)`,na.rm = TRUE)
#max(Survey_info$`Duration (in seconds)`,na.rm = TRUE)
#median(Survey_info$`Duration (in seconds)`,na.rm = TRUE)

#stdev <- sd(Survey_info$`Duration (in seconds)`,na.rm = TRUE)
#stdev

#Q1 <- summary(Survey_info$`Duration (in seconds)`,na.rm = TRUE)[["1st Qu."]]
#Q3 <- summary(Survey_info$`Duration (in seconds)`,na.rm = TRUE)[["3rd Qu."]]

#Q_upper <- Average_Duration + 1.5*(Q3-Q1)
#Q_lower <- Average_Duration - 1.5*(Q3-Q1)

#Q_upper
#Q_lower

#Outliers_upper <- Survey_info$`Duration (in seconds)`>=Q_upper
#Outliers_lower <- Survey_info$`Duration (in seconds)`<=Q_lower

#boxplot(Survey_info$`Duration (in seconds)`)
#outliers <- boxplot(Survey_info$`Duration (in seconds)`)$out
#print(outliers)

###Survey_info$`Duration (in seconds)`[which(Survey_info$`Duration (in seconds)` %in% outliers),na.rm = TRUE]


#Outliers2 <- function(x,na.rm = TRUE){
  #x[!x %in% boxplot.stats(x)$out]
#}
#plot(Outliers2(Survey_info$`Duration (in seconds)`))

###Distribution of Durations####
#Duration_graph <- data.frame(Survey_info$`Duration (in seconds)`) %>%
  #ggplot(aes(x = Alternative_Duration)) +
  #geom_histogram(binwidth = 30, na.rm = TRUE)

#Duration_graph


boxplot(Survey_info.First$Duration..in.seconds.<2000)

Survey_info.First$Duration..in.seconds.[which(Survey_info.First$Duration..in.seconds.<2000)]
1/998001

