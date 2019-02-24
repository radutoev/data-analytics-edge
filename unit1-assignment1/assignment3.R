setwd("D:\\projects\\personal\\data-analytics-edge\\unit1-assignment1")

CPS <- read.csv("CPSData.csv")

#Among the interviewees with a value reported for the Industry variable, what is the most common industry of employment? Please enter the name exactly how you see it.
sort(table(CPS$Industry))

# Which state has the fewest interviewees?
sort(table(CPS$State))

#What proportion of interviewees are citizens of the United States?
table(CPS$Citizenship)
(116639+7073)/(116639+7073+7590)

#The CPS differentiates between race (with possible values American Indian, Asian, Black, Pacific Islander, White, or Multiracial) and ethnicity. A number of interviewees are of Hispanic ethnicity, as captured by the Hispanic variable. For which races are there at least 250 interviewees in the CPS dataset of Hispanic ethnicity? (Select all that apply.)
table(CPS$Race, CPS$Hispanic)

#Which variables have at least one interviewee with a missing (NA) value? (Select all that apply.)
summary(CPS)

table(CPS$Region, is.na(CPS$Married))

table(CPS$State, is.na(CPS$MetroAreaCode))

#Which region of the United States has the largest proportion of interviewees living in a non-metropolitan area?
table(CPS$Region, is.na(CPS$MetroAreaCode))

#Which state has a proportion of interviewees living in a non-metropolitan area closest to 30%?
tapply(is.na(CPS$MetroAreaCode), CPS$State, mean)

MetroAreaMap <- read.csv("MetroAreaCodes.csv")
CountryMap <- read.csv("CountryCodes.csv")

#Merge metropolitan area data
CPS <- merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
str(CPS)
table(is.na(CPS$MetroAreaCode))

#Which of the following metropolitan areas has the largest number of interviewees?
table(CPS$MetroArea)

#Which metropolitan area has the highest proportion of interviewees of Hispanic ethnicity? Hint: Use tapply() with mean, as in the previous subproblem. Calling sort() on the output of tapply() could also be helpful here.
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))

#Remembering that CPS$Race == "Asian" returns a TRUE/FALSE vector of whether an interviewee is Asian, determine the number of metropolitan areas in the United States from which at least 20% of interviewees are Asian.
sort(tapply(CPS$Race=="Asian", CPS$MetroArea, mean))

sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE))

#Merge with country of birth data
CPS <- merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
table(is.na(CPS$Country))

# Among all interviewees born outside of North America, which country was the most common place of birth?
sort(table(CPS$Country))

#What proportion of the interviewees from the "New York-Northern New Jersey-Long Island, NY-NJ-PA" metropolitan area have a country of birth that is not the United States? For this computation, don't include people from this metropolitan area who have a missing country of birth.
table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country != "United States")

#Which metropolitan area has the largest number (note -- not proportion) of interviewees with a country of birth in India? Hint -- remember to include na.rm=TRUE if you are using tapply() to answer this question.
sort(tapply(CPS$Country == "India", CPS$MetroArea, sum, na.rm=TRUE))
sort(tapply(CPS$Country == "Brazil", CPS$MetroArea, sum, na.rm=TRUE))
sort(tapply(CPS$Country == "Somalia", CPS$MetroArea, sum, na.rm=TRUE))
