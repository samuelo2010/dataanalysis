###msc vldata analysis file
### 1.Exploring vldata

    ##a.navigating to git dataanalysis

setwd("C:/Users/USER/Desktop/dataanalysis")
getwd()

###b.install tidyverse package
install.packages("tidyverse")
library(tidyverse)

##c. load the vldata
vldata<-read.csv("vldata_csv.csv")

names(vldata)#view the existing variables
glimpse(vldata)
attach(vldata)#allow easy extraction of variables
unique(County)#40 counties
unique(District)#127 districts
unique(Facility.Name)#1047 facilities
unique(Sex)#Female,male,""
unique(AgeYears)# range from 0 years but some are in year of birth
unique(Sample.Type)# 4 sample types with no data
unique(Collection.Date)#range from 2012 to 2015, break into year month
unique(Received.Status)#3 status exist including rejection
unique(Rejected.Reason...Reason.for.Repeat)#change missing into none and retain the variable
unique(Current.Regimen)#13 regimens and retain all values
unique(ART.Initiation.Date)#Month and Year, get duration on medication
unique(Justification)#8 data values
unique(Date.of.Receiving)#inform of ddmmyyyy
unique(Date.of.Testing)#inform of ddmmyyyy
unique(Viral.Load)#<LDL,<400,<150,Collect new sample,"" are interesting unique values


#get frequency table for Justification
vldata %>% 
  select(Justification)%>%
  count(Justification)%>%
  arrange(desc(n))%>%
  view()
unique(County) #counties served
vldata %>% 
  select(County)%>%
  count(County)%>%
  arrange(desc(n))%>%
  view()
#explore facilities served
unique(Facility.Name)
vldata %>% 
  select(Facility.Name)%>%
  count(Facility.Name)%>%
  arrange(desc(n))%>%
  view()
view(vldata[is.na(County),])# It appears the missing data isn't labelled as na

summary(AgeYears)
###2. Data cleaning

###3. Data manipulation

###4. Data description and summary

###5. Data analysis