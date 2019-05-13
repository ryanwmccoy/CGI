library(dslabs)
library(dplyr)
library(ggplot2)
library(data.table)



###Breaking the surveyed data into different blocks to be cleansed
Survey_info <- (initial_data[1:9])
Personal_info <- (initial_data[10:24])
Hospital_options <-(initial_data[25:80])
Survey_questions <-(initial_data[81:121])
Testimonial_info <- (initial_data[122:127])



#### ---------------------Verify that everyone has finished the survey---------------------------
Finished_surveys <- length(Survey_info$Finished[Survey_info$Finished == TRUE])


if(Finished_surveys == length(Survey_info$Finished)){
  print("Everyone has finished")
}else{
  paste(length(Survey_info$Finished)- Finished_surveys, " entries are not completed. Caution before continuing that the data is not complete.")
  
}


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
head(Survey_info.First[1])
tail(Survey_info.First[1])
ls(Survey_info.First)
class(Survey_info.First$StartDate)

Last_survey_started - First_survey_started



###write.csv(Survey_info.First,
###          file = "2019_Membership_Survey_info_Start_times.csv")

  ###Distribution of Start Times###
#Start_time_graph <- Survey_info.First %>% ggplot(aes(Survey_info.First$StartDate)) +
  ###geom_histogram(binwidth = 31, )
#  geom_density()

#Start_time_graph


###--------------------------------Duration of Survey----------------------------------
Survey_info$`Duration (in seconds)` <- as.numeric(Survey_info$`Duration (in seconds)`,na.rm= TRUE)
Average_Duration <- mean(Survey_info$`Duration (in seconds)`,na.rm = TRUE)
Average_Duration /60
min(Survey_info$`Duration (in seconds)`,na.rm = TRUE)
max(Survey_info$`Duration (in seconds)`,na.rm = TRUE)
median(Survey_info$`Duration (in seconds)`,na.rm = TRUE)

stdev <- sd(Survey_info$`Duration (in seconds)`,na.rm = TRUE)
stdev

Q1 <- summary(Survey_info$`Duration (in seconds)`,na.rm = TRUE)[["1st Qu."]]
Q3 <- summary(Survey_info$`Duration (in seconds)`,na.rm = TRUE)[["3rd Qu."]]

Q_upper <- Average_Duration + 1.5*(Q3-Q1)
Q_lower <- Average_Duration - 1.5*(Q3-Q1)

Q_upper
Q_lower

Outliers_upper <- Survey_info$`Duration (in seconds)`>=Q_upper
Outliers_lower <- Survey_info$`Duration (in seconds)`<=Q_lower

boxplot(Survey_info$`Duration (in seconds)`)
outliers <- boxplot(Survey_info$`Duration (in seconds)`)$out
print(outliers)

###Survey_info$`Duration (in seconds)`[which(Survey_info$`Duration (in seconds)` %in% outliers),na.rm = TRUE]


Outliers2 <- function(x,na.rm = TRUE){
  x[!x %in% boxplot.stats(x)$out]
}
plot(Outliers2(Survey_info$`Duration (in seconds)`))

###Distribution of Durations####
Duration_graph <- data.frame(Survey_info$`Duration (in seconds)`) %>%
  ggplot(aes(x = Alternative_Duration)) +
  geom_histogram(binwidth = 30, na.rm = TRUE)

Duration_graph
