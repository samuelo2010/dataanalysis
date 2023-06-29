###msc vldata analysis file
### 1.Exploring vldata

    ##a.navigating to git dataanalysis

setwd("C:/Users/USER/Desktop/dataanalysis")
       #getwd()

###b.install tidyverse package
      #install.packages("tidyverse")
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

###2. Data cleaning
      
      
##a. Viralload variable
#i.remove misssing viral load test outcome
data1<-vldata %>%
  select(everything()) %>%
  filter(!Viral.Load %in% c("","Collet New Sample"))
##check for the values
         unique(data1$Viral.Load)#removed missing values under viral load outcome
#ii. convert ldl and < values to 0
data1$Viral.Load[data1$Viral.Load %in% c("< LDL ","< 400  ","< 150 ")]<-"0"
#iii. convert vl to numeric

data1$Viral.Load<-as.numeric(data1$Viral.Load)
#iv. convert vl to log10, remove NAs,
data2<-data1 %>%
  select(everything())%>%
  filter(!is.na(data1$Viral.Load))%>%
  mutate(Viral.Load1 = log10(Viral.Load))
#iv. categorize vl into low,high, convert -Inf to 0
data2$Viral.Load1[data2$Viral.Load1==-Inf]<-0
data2$Viral.Load<-as.numeric(data2$Viral.Load)
        data2 %>%
          select(everything())%>%
           mutate(Viral.Load2=case_when(Viral.Load1<3~"LOW",Viral.Load1>=3~"HIGH"))

##b. converting from character to date object;collection date,art initiation, reception date, testing date
  
   #collection_date
data2$Collection.Date<-as.Date(data2$Collection.Date,format ="%d-%b-%y")
    #reception date
data2$Date.of.Receiving<-as.Date(data2$Date.of.Receiving,format = "%d-%m-%y")
    #testing date
data2$Date.of.Testing<-as.Date(data2$Date.of.Testing,format = "%d-%m-%y")
    #art initiation date
data2$ART.Initiation.Date<-as.Date(paste0("1-",data2$ART.Initiation.Date),format="%d-%b-%y")

####c. extracting collection month, year, duration on art, duration on transit, duration before testing, duration in lab before testing
   ####1.collection month and year
data2$Month_collection <- format(as.Date(data2$Collection.Date), "%m")
data2$Year_collection <- format(as.Date(data2$Collection.Date), "%Y")
   ###2. duration on ART
data2$ART_duration<-difftime(data2$Collection.Date,data2$ART.Initiation.Date,units = "days")
    ###3.duration on transit
data2$dur_transit<-difftime(data2$Date.of.Receiving,data2$Collection.Date,units="days")
    ####4. duration in the lab
data2$dur_inlab<-difftime(data2$Date.of.Testing,data2$Date.of.Receiving,units = "days")
    ####5. overall-duration pre-testing
data2$pre_test_dur<-difftime(data2$Date.of.Testing,data2$Collection.Date,units="days")


install.packages("zoo")
library(zoo)
       data2$month<- format(as.Date(data2$Collection.Date, format="%d-%m-%y"),"%m")
         data2$year<- format(as.Date(data2$Collection.Date, format="%d-%m-%y"),"%y") 

######

###3. Data manipulation

###4. Data description and summary

###5. Data analysis