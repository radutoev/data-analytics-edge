#Intro to R
#Just some commands to play around with
table(who$Region)
tapply(who$LiteracyRate, who$Region, min)