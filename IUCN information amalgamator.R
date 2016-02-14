#Data Editing Script (reorganizing and joining data)
library(dplyr)
library(tidyr)
CountryInfo=read.csv("./Data/Fish Country Info.csv")
names(CountryInfo)[1]="Scientific.Name"
StatusInfo=read.csv("./Data/Fish Red List Taxonomic and Status.csv")
TextInfo=read.csv("./Data/Fish Text Info.csv")
CountryConversion=read.csv("./Data/CountryConversion.csv")

#This matches ISO3 codes to our data for creation of maps. Use Country Conversion file if you'd like to find the country name matching the iso codes.
names(CountryInfo)[-1]=as.character(CountryConversion$ISO3[match(names(CountryInfo)[-1],CountryConversion$PythonOutput)])
CountryInfo=CountryInfo[!is.na(names(CountryInfo))]

CountryInfo[CountryInfo==0]=NA

gather(CountryInfo,key="CountryISO3",value="Species_Presence",-Scientific.Name,na.rm=TRUE)%>%
  left_join(TextInfo[,c(1,7,11)])%>%
  left_join(StatusInfo[c(1,3:6,13,14)])%>%
  subset(Systems=="Marine")%>%
  select(c(2,1,11,3,4,10,6,7,8,9))%>%
  write.csv(file="./Data/Long Format Collected Fish Information IUCN")
  