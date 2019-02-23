setwd("D:\\projects\\personal\\data-analytics-edge\\unit1-assignment1")
mvt = read.csv("mvtWeek1.csv")

# Using the "max" function, what is the maximum value of the variable "ID"?
mvt$ID[which.max(mvt$ID)]

# What is the minimum value of the variable "Beat"?
mvt$Beat[which.min(mvt$Beat)]

# How many observations have value TRUE in the Arrest variable 
#(this is the number of crimes for which an arrest was made)?
withArrests = subset(mvt, Arrest==TRUE)

#How many observations have a LocationDescription value of ALLEY?
str(subset(mvt, LocationDescription == "ALLEY"))

#In many datasets, like this one, you have a date field. 
#Unfortunately, R does not automatically recognize entries that look like dates. 
#We need to use a function in R to extract the date and time. 
#Take a look at the first entry of Date (remember to use square brackets when looking at a certain entry of a variable).
#In what format are the entries in the variable Date?
mvt$Date[0]

#Now, let's convert these characters into a Date object in R. In your R console, type
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
#What is the month and year of the median date in our dataset?
summary(DateConvert)

years <- function(x,abbreviate=FALSE) { 
  as.numeric(format(x, ifelse(abbreviate, "%y", "%Y"))) 
} 

mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Year = years(DateConvert)
mvt$Date = DateConvert
#In which month did the fewest motor vehicle thefts occur?
table(mvt$Month)

#On which weekday did the most motor vehicle thefts occur?
table(mvt$Weekday)

#Each observation in the dataset represents a motor vehicle theft, 
#and the Arrest variable indicates whether an arrest was later made for this theft. 
#Which month has the largest number of motor vehicle thefts for which an arrest was made?
table(subset(mvt, Arrest==TRUE)$Month)

table(mvt$Year)
hist(subset(mvt, Date >= as.Date(strptime("01/01/2002 00:00", "%m/%d/%y %H:%M"))), breaks=100)

#Crime Trends
arrests = subset(mvt, Arrest==TRUE)
boxplot(arrests$Date, main = "Arrests overt time")

#For what proportion of motor vehicle thefts in 2001 was an arrest made?
table(mvt$Arrest, mvt$Year)

#Popular locations
sort(table(mvt$LocationDescription))

Top5 = subset(mvt, LocationDescription=="STREET" | LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)" | LocationDescription=="ALLEY" | LocationDescription=="GAS STATION" | LocationDescription=="DRIVEWAY - RESIDENTIAL")
Top5$LocationDescription = factor(Top5$LocationDescription)
str(Top5)

# On which day of the week do the most motor vehicle thefts at gas stations happen?

table(Top5$Weekday)
