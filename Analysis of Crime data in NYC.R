#Loading the dataset
nydata <- read.csv("~/Desktop/Visualization/Assignments/NYPD_Motor_Vehicle_Collisions.csv", header=TRUE, stringsAsFactors=FALSE)

library(dplyr)

#1. Number of Boroughs in the dataset

all_boroughs<-unique(nydata$BOROUGH)
#Excluding the rows which did not have Borough name
num_boroughs<-length(all_boroughs[all_boroughs !=""])
num_boroughs

#Alternate- Using the dplyr package
boroughs<-nydata$BOROUGH[nydata$BOROUGH!=""]
n_distinct(boroughs)


#2. Number of persons who got injured in each borough
summarise(group_by(nydata,BOROUGH),count_inj=sum(NUMBER.OF.PERSONS.INJURED,na.rm=TRUE)) %>%
  arrange(desc(count_inj))

#3. Number of persons who got killed in each borough
summarise(group_by(nydata,BOROUGH),count_killed=sum(NUMBER.OF.PERSONS.KILLED,na.rm=TRUE)) %>%
  arrange(desc(count_killed))

#4. 10 streets in Brooklyn with most number if injured persons
nydata_brooklyn<-filter(nydata,BOROUGH=="BROOKLYN")
summarise(group_by(nydata_brooklyn,ON.STREET.NAME),count_inj=sum(NUMBER.OF.PERSONS.INJURED,na.rm=TRUE)) %>%
  arrange(desc(count_inj)) %>%
  slice(1:11) #Since the first one is an empty string, we have taken the first 11 rows

#5. Combine Borough, Zipcode and street name to Address column
library(stringr)
#All the empty values for zipcode were NAs, as a result of which the Address for those rows were being
# set to NA, instead of concatenating the remaining strings- so we changed NA to empty strings
nydata$ZIP.CODE<-str_replace_na(nydata$ZIP.CODE, replacement = "")
nydata$Address <- str_c(nydata$BOROUGH, nydata$ZIP.CODE, nydata$ON.STREET.NAME,sep=", ")
nydata$Address<-str_replace_all(nydata$Address,", ,","")

#6. 10 times of the day with most Avg number if injured persons in Brooklyn
summarise(group_by(nydata_brooklyn,TIME),avg_inj=mean(NUMBER.OF.PERSONS.INJURED,na.rm=TRUE)) %>%
  arrange(desc(avg_inj)) %>%
  slice(1:10)

#7 How does the Number of persons injured and the number of persons killed vary, by year
nydata$Year<-str_sub(nydata$DATE,-4,-1)
summarise(group_by(nydata, Year), num_injured = sum(NUMBER.OF.PERSONS.INJURED, na.rm = TRUE), num_killed = sum(NUMBER.OF.PERSONS.KILLED, na.rm = TRUE)) %>%
  arrange(desc(Year))

paste0("The number of people injured and number of people killed doubled in 2013.After 2013, the numbers decreased by a very small amount in 2014 and 2015.Though the number of persons injured in 2016 increased by a significant amount, the number of persons killed in 2016 remained almost the same as 2015. Also, the number of people injured in 2018 is nearly same as in 2013-2015, but the number of people killed in 2018 is significantly lower than in 2013-2015.")


