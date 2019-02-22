setwd("D:\\projects\\personal\\data-analytics-edge\\unit1")
USDA = read.csv("USDA.csv")

#Get some overview information of the dataset
str(USDA)
summary(USDA)

#Display what food has the highest concentration of sodium
USDA$Description[which.max(USDA$Sodium)]
HighSodium=subset(USDA, Sodium > 10000)
HighSodium$Description

#What about caviar? 
USDA$Sodium[match("CAVIAR", USDA$Description)]
summary(USDA$Sodium)
sd(USDA$Sodium)

#Visualise some stuf
plot(USDA$Protein, USDA$TotalFat, xlab = "Protein", ylab = "Fat", main = "Protein vs Fat", col="red")
hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C levels", xlim = c(0, 100), breaks = 2000)
boxplot(USDA$Sugar, main="Boxplot of Sugar Levels", ylab="Sugar in grams")

#
USDA$HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm = TRUE))
USDA$HighProtein = as.numeric(USDA$Protein > mean(USDA$Protein, na.rm = TRUE))
USDA$HighFat = as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm = TRUE))
USDA$HighCarbs = as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm = TRUE))
str(USDA)
table(USDA$HighSodium)
table(USDA$HighSodium, USDA$HighFat)
#Avg amount of iron sorted by high and low protein
#tapply(arg1, arg2, arg3) -> group arg1 by arg2 and apply arg3
tapply(USDA$Iron, USDA$HighProtein, mean, na.rm = TRUE)
#Max lvl of vit c in food with high and low carbs
tapply(USDA$VitaminC, USDA$HighCarbs, max, na.rm = TRUE)
#Are foods that are high in carbs more rich in vitamin c? 
tapply(USDA$VitaminC, USDA$HighCarbs, summary, na.rm = TRUE)
#It seems so, by looking at the mean