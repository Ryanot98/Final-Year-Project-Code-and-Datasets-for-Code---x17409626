#Ryan O'Toole
#Final Year Project
#R-Studio Code

#Installation of Packages
install.packages('readxl')
install.packages('readr')
install.packages('leaflet')
install.packages("dplyr")
install.packages('writexl')

#Adding Packages to the library
library(readxl)
library(leaflet)
library(sp)
library(writexl)
library(dplyr)

#Importing the dataset
X2014_2020_payments_for_student <- read_excel("C:/Users/ryano/OneDrive/Desktop/NCI Final Year/Project-Software Project/Excel Files for 2014-2020 Payments - Raw Data/2014-2020 payments for student.xlsx")
View(X2014_2020_payments_for_student)

#Observing number of rows within the data
nrow(X2014_2020_payments_for_student)

#Pre-Processing the Data

#Removing unnecessary columns
#These columns contained data that were found irrelevant to the project
X2014_2020_payments_for_student <- X2014_2020_payments_for_student[, -10]
X2014_2020_payments_for_student <- X2014_2020_payments_for_student[, -8]
X2014_2020_payments_for_student <- X2014_2020_payments_for_student[, -6]

#Assigning new column names
#This makes the column names easier to read
names(X2014_2020_payments_for_student)[1]<-"County"
names(X2014_2020_payments_for_student)[2]<-"Organisation/Club"
names(X2014_2020_payments_for_student)[5]<-"Scheme"
names(X2014_2020_payments_for_student)[7]<-"Organisation"
names(X2014_2020_payments_for_student)[8]<-"Sport"

#Checking for N/A with complete cases
complete.cases(X2014_2020_payments_for_student)
#Checking exact number of N/A using summary
sum(is.na(X2014_2020_payments_for_student))
#Removing N/A's found within dataset
#Tells us the exact row where NAS are in Data
colSums(is.na(X2014_2020_payments_for_student))
#Removing Rows with missing data
X2014_2020_payments_for_student.clean <- na.omit(X2014_2020_payments_for_student)
#Checking what rows we are left with
nrow(X2014_2020_payments_for_student.clean)


#Data Transformation

#Changing Data Types
X2014_2020_payments_for_student$County <- as.factor(X2014_2020_payments_for_student$County)
X2014_2020_payments_for_student$Organisation/Club <- as.factor(X2014_2020_payments_for_student$Organisation/Club)



#Exporting our Clean Data prepared for Data Mining Techniques, Testing and Visualisations
write.table(X2014_2020_payments_for_student.clean, file = "Payments_2014-2020.csv", row.names = F, sep=",")





#Combining Data obtained from the CSO on population Statistcis

#Reading Census Data
census <- read.csv("C:/Users/ryano/Downloads/SAPS2016_SA2017.csv")
View(census)

#Reading the Dataset to provide insight to census data
small_area <- read.csv("C:/Users/ryano/Downloads/Small_Areas_Generalised_100m_-_OSi_National_Statistical_Boundaries_-_2015.csv")
View(small_area)

#Joining the two Data sets by the common column GUID
census_small_area <- left_join(census, small_area, by = c("GUID"="GUID"))

#Exporting the combined datsets as a CSV file
write.table(census_small_area, file = "census_small_area.csv", row.names = F, sep=",")




#Code for Average Invest by County Map#

#Importing Dataset made in excel
AverageInvestmentByCountyforMap <- read_excel("C:/Users/ryano/OneDrive/Desktop/NCI Final Year/Project-Software Project/RStudio Code/AverageInvestmentByCountyforMap.xlsx")
View(AverageInvestmentByCountyforMap)

#Changing Variable Names for the analysis
names(AverageInvestmentByCountyforMap)[2]<-"grantavg"

#Changing Datatypes
AverageInvestmentByCountyforMap$Lon <- as.numeric(AverageInvestmentByCountyforMap$Lon)
AverageInvestmentByCountyforMap$Lat <- as.numeric(AverageInvestmentByCountyforMap$Lat)

data.sp <- SpatialPointsDataFrame(AverageInvestmentByCountyforMap[,c(4,3)],AverageInvestmentByCountyforMap[,-c(4,3)])

leaflet()%>%
  addTiles()%>% 
  addAwesomeMarkers(data = AverageInvestmentByCountyforMap, lng = ~Lon, lat = ~Lat, label =  AverageInvestmentByCountyforMap$County, popup = ~as.character(grantavg))

m

#Change color dependent on Investment Amount 
getColor <- function(AverageInvestmentByCountyforMap) {
  sapply(AverageInvestmentByCountyforMap$grantavg, function(grantavg) {
    if(grantavg <= 20000) {
      "Red"
    } else if(grantavg <= 30000) {
      "Orange"
    } else {
      "Green"
    } })
}