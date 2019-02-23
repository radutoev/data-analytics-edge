setwd("D:\\projects\\personal\\data-analytics-edge\\unit1-assignment1")
IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
Boeing = read.csv("BoeingStock.csv")

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

summary(IBM)
summary(GE)
summary(CocaCola)
summary(Boeing)
summary(ProcterGamble)
sd(ProcterGamble$StockPrice)

plot(CocaCola$Date, CocaCola$StockPrice, xlab = "Date", ylab = "Value", col = "red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = "blue")
abline(v=as.Date(c("2000-03-01")), lwd=2)

subset(CocaCola, Date == as.Date("2000-02-28") | Date == as.Date("2000-03-01"))
subset(ProcterGamble, Date == as.Date("2000-02-28") | Date == as.Date("2000-03-01"))
CocaCola$StockPrice[362]
ProcterGamble$StockPrice[362]


plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col = "blue", ylim=c(0,210))
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col = "green", ylim=c(0,210))
lines(GE$Date[301:432], GE$StockPrice[301:432], col = "purple", ylim=c(0,210))
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col = "orange", ylim=c(0,210))

# In October of 1997, there was a global stock market crash that was caused by an economic crisis in Asia. Comparing September 1997 to November 1997, which companies saw a decreasing trend in their stock price? (Select all that apply.)
abline(v=as.Date("1997-09-01"))
abline(v=as.Date("1997-11-01"))

#In the last two years of this time period (2004 and 2005) which stock seems to be performing the best, in terms of increasing stock price?
abline(v=as.Date("2004-01-01"))
abline(v=as.Date("2005-01-01"))

#For IBM, compare the monthly averages to the overall average stock price. In which months has IBM historically had a higher stock price (on average)? Select all that apply.(January~December)
mean(IBM$StockPrice)
tapply(IBM$StockPrice, months(IBM$Date), mean)

mean(GE$StockPrice)
tapply(GE$StockPrice, months(GE$Date), mean)

mean(CocaCola$StockPrice)
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
