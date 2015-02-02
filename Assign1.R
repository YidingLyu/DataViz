setwd("/Users/Lichking/Desktop/Columbia/Courses/DataVisualization/Assignments/Assignment1")

LGA_Data=read.csv("lgas.csv")
Facility_Data=read.csv("Health_Mopup_and_Baseline_NMIS_Facility.csv")

###From the NMIS Health Facilities Inventory, select all facilities located in the Southern zones of Nigeria.
LGA_South=subset(LGA_Data,zone %in% c("South-South","Southeast","Southwest"))
SouthCity=LGA_South$unique_lga
Facility_South=subset(Facility_Data,unique_lga %in% SouthCity)

###Incorporate LGA metadata into this new dataframe.
Merge_Data=merge(LGA_South,Facility_South,by="unique_lga")

###calculate the total number of docters and nurses

States=levels(LGA_Data$state)
StateData=LGA_Data[LGA_Data$state=="Abia",]
StateCity=StateData$unique_lga
StateFacility=subset(Facility_Data,unique_lga %in% StateCity)
nurse1=na.omit(StateFacility$num_nurses_fulltime)
nurse2=na.omit(StateFacility$num_nursemidwives_fulltime)
doctor=na.omit(StateFacility$num_doctors_fulltime)
FulltimeTotal=sum(nurse1)+sum(nurse2)+sum(doctor)

###for all the states
States=levels(LGA_Data$state)
FacilityStatistics=data.frame(state=States,num_of_fulltime_nurses_and_doctors=rep(-1,length(States)),population=rep(-1,length(States)),area=rep(-1,length(States)),facility_per_capita=rep(-1,length(States)),facility_per_area=rep(-1,length(States)))
for (n in c(1:length(States))){
  StateData=LGA_Data[LGA_Data$state==States[n],]
  Population=sum(StateData$pop_2006)
  Area=sum(StateData$area_sq_km)
  FacilityStatistics[n,3]=Population
  FacilityStatistics[n,4]=Area
  StateCity=StateData$unique_lga
  StateFacility=subset(Facility_Data,unique_lga %in% StateCity)
  nurse1=na.omit(StateFacility$num_nurses_fulltime)
  nurse2=na.omit(StateFacility$num_nursemidwives_fulltime)
  doctor=na.omit(StateFacility$num_doctors_fulltime)
  FulltimeTotal=sum(nurse1)+sum(nurse2)+sum(doctor)
  FacilityStatistics[n,5]=FulltimeTotal/Population
  FacilityStatistics[n,6]=FulltimeTotal/Area
  FacilityStatistics[n,2]=FulltimeTotal
}

library(plyr)
FacilityStatistics_Sorted=arrange(FacilityStatistics,desc(population))

write.csv(FacilityStatistics_Sorted,file="./result.csv")
write.csv(FacilityStatistics_Sorted,file="./result_noRowName.csv",row.names=FALSE)

###for only southern states

