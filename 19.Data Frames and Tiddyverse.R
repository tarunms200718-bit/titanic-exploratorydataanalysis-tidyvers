setwd("C:/Users/Admin/Downloads/r program")
getwd()
# reading the csv file
titanic<-read.csv("titanic_data.csv",header=T)
## Structure of the data
str(titanic)
# First 6 records 
head(titanic)
# Last 6 records
tail(titanic)
##summary of the data
summary(titanic)
titanic$Gender 

## Selecting specific columns
titanic$Survived  # Selecting a single column 

titanic[,c("Name","Gender","Fare")]  # Selecting multiple column

titanic[,5:8] # Select column by Index

##### FILTER operations (row selection)
# Filte(r and select passanger above the age of 35
titanic[titanic$Age>35,c("PassangerId","Gender","Age")]
titanic[titanic$Age>35,c("Gender","Age","Fare")]
head(titanic[titanic$Age>35,c("Gender","Age","Fare")])

#Selecting using SELECT - Select specific columns
sel_set_1<- titanic %>% select(Pclass,Age,Fare,Survived)
library(tidyverse)

# to change 0 and 1 in the survived column 
titanic$Survival_status
ifelse(titanic$Survived==1,"Survived","Not Survived")

# Create a Family count column using mutate()
titanic %>% mutate(FamilyMembers=titanic$SibSp+titanic$Parch)

# Create an Adult/Child column using age
titanic %>% mutate(AgeGroup=ifelse(titanic$Age>18,"Adult","child"))%>% head()

## this adds the count using mutate is used to create new column
titanic<-titanic %>% mutate(AgeGroup=ifelse(titanic$Age>18,"Major","Minor"))
titanic$AgeGroup

##sorting
#--------sorting by assending fare
fares_asc<-titanic %>% arrange(Fare)
#--------decending order
fare_des<-titanic %>% arrange(desc(Fare))

#---updating the age of the specific column id
titanic$Age[titanic$PassengerId==22]<-99
titanic$Age[titanic$PassengerId==2]

##group by function
#to count male and female
titanic%>% group_by(Gender)%>%summarise(count=n())

# Average age of male and female
titanic %>% group_by(Gender) %>% summarise(AvgFare=mean(Fare))

#count the Survivors by gender
titanic %>% group_by(Gender)%>% summarise(Survived=sum(Survived))

# count passengers by class 
titanic %>% group_by(Pclass)%>%summarise(count=n())

##____________________
titanic%>%  group_by(Pclass)%>%summarise(count=sum(survived))%>% arrange (Survived)
write.csv(titanic,"modified_titanic.csv")

###############################################################################
text_data<- readLines("text_data.txt")
text_data
