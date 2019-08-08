author: mbigdelou

setwd("C:\\Users\\Mehdi\\Desktop\\R Final Project\\Done")
getwd()

d = read.csv("all_stocks_5yr.csv", header = TRUE, sep=",", na.strings = "na", stringsAsFactors=FALSE)
d$date = as.Date(d$date, "%m/%d/%Y")
colnames(d)
dim(d)

typeof(d$Return)
class(d$Return)

companies <- unique(d$Name) 
numberOfcompanies <- length(companies)
companieslength <- sort(table(d$Name), decreasing = TRUE)


sum(d$Return, na.rm=TRUE)
mean(d$Return, na.rm=TRUE)



library(ggplot2)

#=====================
#=====================
# Question 1)
#Show the distribution of Price (Return) for the Market index, as well as selected companies (Amazon and Apple and so on).
mktdaily = aggregate(d$close, by=list(d$date), mean, na.rm = TRUE) #average daily index price
names(mktdaily)[1] = "date"
names(mktdaily)[2] = "averagedaily"
dsplit <- split(d, d$Name)  
MKTclose=mktdaily

#Market
jpeg('Market_daily_price_boxplot.jpg', width=3000, height = 1500)
boxplot(mktdaily$averagedaily,data=d, main="Market Daily Price Boxplot",xlab="", ylab="Price",
        cex.lab=2, cex.axis=1.5, cex.main=2.5, cex.sub=1.5)
dev.off()

#The entire market (Open, High, Low, Close)
#c("open", "high", "low", "close")
jpeg('open_high_low_close_boxplot2.jpg', width=3000, height = 1500)
boxplot(open~Name,data=d, main="Market Daily Price Boxplot",xlab="", ylab="Price",
        cex.lab=2, cex.axis=1.5, cex.main=2.5, cex.sub=1.5, ylim=c(0,300))
dev.off()


#Amazon, Apple, Google, Microsoft, Intel
AMZNclose=dsplit$AMZN$close
AAPLclose=dsplit$AAPL$close
GOOGLclose=dsplit$GOOGL$close
MSFTclose=dsplit$MSFT$close
INTCclose=dsplit$INTC$close
Companiesboxplot = data.frame(AMZNclose, AAPLclose, GOOGLclose, MSFTclose, INTCclose)
dim(Companiesboxplot)
jpeg('Companies_daily_price_boxplot.jpg', width=3000, height = 1500)
boxplot(Companiesboxplot[1:5],data=Companiesboxplot, main="IT Industry Stocks Daily Price Boxplot",xlab="", ylab="Price",
        cex.lab=2, cex.axis=1.5, cex.main=2.5, cex.sub=1.5)
dev.off()


#=====================
#=====================
# Question 2)
#Show the Correlation Matrix of all of the variables.
#Show the Correlation Matrix of selected stocks.
install.packages("corrplot")
library(corrplot)
install.packages("dplyr")
library(dplyr)

cordf1 = select(d, open, high, low, close, volume, Return)

cor_1 = cor(cordf1, use = "complete.obs")
jpeg('Correlation_among_all_variables.jpg', width=1300, height = 800)
corrplot(cor_1, method = "pie")
dev.off()

#Show the Correlation Matrix of selected stocks.
AMZN=d[d$Name=="AMZN",c(5)]
AAPL=d[d$Name=="AAPL",c(5)]
GOOGL=d[d$Name=="GOOGL",c(5)]
MSFT=d[d$Name=="MSFT",c(5)]
INTC=d[d$Name=="INTC",c(5)]
IBM=d[d$Name=="IBM",c(5)]
INTU=d[d$Name=="INTU",c(5)]
V=d[d$Name=="V",c(5)]
KLAC=d[d$Name=="KLAC",c(5)]
EA=d[d$Name=="EA",c(5)]
NFLX=d[d$Name=="NFLX",c(5)]
CBS=d[d$Name=="CBS",c(5)]
TRIP=d[d$Name=="TRIP",c(5)]
VZ=d[d$Name=="VZ",c(5)]
EBAY=d[d$Name=="EBAY",c(5)]
NKE=d[d$Name=="NKE",c(5)]
KO=d[d$Name=="KO",c(5)]
AON=d[d$Name=="AON",c(5)]
COF=d[d$Name=="COF",c(5)]
GS=d[d$Name=="GS",c(5)]

brands <- data.frame(AMZN,AAPL,GOOGL,MSFT,INTC,IBM,INTU,V,KLAC,EA,NFLX,CBS,TRIP,VZ,EBAY,NKE,KO,AON,COF,GS)
cor2 = cor(brands)

jpeg('Correlation Matrix of selected stocks.jpg', width=1000, height = 800)
corrplot(cor2, type = "lower", order = "hclust", tl.col = "darkblue", tl.srt = 45)
dev.off()

jpeg('Correlation Matrix of selected stocks2.jpg', width=1000, height = 800)
pairs(brands[,1:20], pch = 19)
dev.off()

#=====================
#=====================
# Question 3)
#Illustrate the trend of Market daily index (average of all the componies within the index (S&P 500)) over the 5 years period on a graph.
# Compare the stock price movement of Amazon (AMZN) and Apple (AAPL) together and with the market (use "close price" as a proxy). 

jpeg('rplot.jpg', width=3000, height = 1700)
plot(x=mktdaily$date, y=mktdaily$averagedaily, type="b", pch=19, col="black", xlab="Date", ylab="Price")
dev.off()

#Amazon
x1 = dsplit$AMZN$date
y2 = log(dsplit$AMZN$close)
jpeg('rplotAMZN_AAPL.jpg', width=3000, height = 1500)
plot(x1, y2 , pch=18, col="blue", type="b", lty=2, xlab="Date", ylab="Price", main="A comparion between stock price movement of AMAZON vs APPLE", 
     cex.lab=2, cex.axis=1.5, cex.main=2.5, cex.sub=1.5)

#Apple 
x3 = dsplit$AAPL$date
y3 = log(dsplit$AAPL$close)
par(new=TRUE)
plot(x1, y3, pch=19, col="red", type="b", lty=2, cex.axis=0.5)
'''
plot(x1, y3, pch=18, col="red", type="b", lty=2, add=TRUE)'''

# legend
legend("topleft", legend=c("AMZN", "AAPL"), col=c("blue","red"), lty=1:3, cex=3.4, text.font=1, box.lty=1, pt.lwd = 10)
dev.off()


#Amazon & Apple and Market

jpeg('rplotAMZN_AAPL_MRKT.jpg', width=3000, height = 1500)
plot(x=mktdaily$date, y=mktdaily$averagedaily, type="b", pch=19, col="black", xlab="Date", ylab="Price", main="A comparion between stock price movement of AMAZON & APPLE vs MARKET", 
     cex.lab=2, cex.axis=1.5, cex.main=2.5, cex.sub=1.5)
par(new=TRUE)
plot(x=mktdaily$date, y2 , pch=18, col="blue", type="b", cex.axis=0.1)
par(new=TRUE)
plot(x=mktdaily$date, y3, pch=19, col="red", type="b", cex.axis=0.1)
dev.off()

#=====================
#=====================
# Question 4)
#Show the relationship of Risk and Return of all the stocks within the market index on a scatter plot. 

# Return and Risk (STD)
TReturn <- aggregate(Return ~ Name, data=d, FUN=mean)
TRisk <- aggregate(Return ~ Name, data=d, FUN=sd)
dim(TReturn)
dim(TRisk)
RR= merge(x=TReturn, y=TRisk, by='Name')
names(RR) <- c("Name","TReturn", "TRisk")

jpeg('Risk_Return_ScatterPlot.jpg', width=2200, height = 1500)
plot(x=RR$TRisk, y=RR$TReturn, pch = "+", cex=4, frame = TRUE, xlab= "Risk (STDEV)", ylab="Retrun", col="#2E9FDF",
     xlim=c(0,.04), ylim=c(-0.002,.002), cex.lab=2, cex.axis=1.5, cex.main=2.5, main="Risk & Retunr Scatter Plot")
abline(v=0, h=0, col="skyblue")
dev.off()


'''
#Smooth Line
install.packages("lattice")
library("lattice")
xyplot(RR$TReturn~RR$TRisk, data = RR,
       type = c("p", "g", "smooth"), xlim=c(0.0075,.018), ylim=c(-0.001,.002),
       xlab = "Risk (STDEV)", ylab = "Retrun")
'''

jpeg('Risk_Return_ScatterPlot_limited.jpg', width=3000, height = 1500)
plot(x=RR$TRisk, y=RR$TReturn, pch = "+", cex=4, frame = TRUE, xlab= "Risk (STDEV)", ylab="Retrun", col="#2E9FDF",
     xlim=c(0.008,.02), ylim=c(-0.0005,.002), cex.lab=2, cex.axis=1.5, cex.main=2.5, main="Risk & Retunr Scatter Plot")
dev.off()

jpeg('Risk_Return_ScatterPlot_limited_Market_Line.jpg', width=3000, height = 1500)
plot(x=RR$TRisk, y=RR$TReturn, pch = "+", cex=4, frame = TRUE, xlab= "Risk (STDEV)", ylab="Retrun", col="#2E9FDF",
     xlim=c(0.008,.02), ylim=c(-0.0005,.002), cex.lab=2, cex.axis=1.5, cex.main=2.5, main="Risk & Retunr Scatter Plot with Market Line",
     text(RR$TRisk, RR$TReturn, labels=RR$Name, cex= 1.7, pos=3))
rMarket= mean(d$Return, na.rm=TRUE)
stdMarket = sd(d$Return, na.rm=TRUE)
abline(rMarket,stdMarket)
dev.off()


#Risk and Return compared to Market Index
jpeg('Risk_Return_ScatterPlot_Market.jpg', width=2200, height = 1500)
plot(x=RR$TRisk, y=RR$TReturn, pch = "+", cex=4, frame = TRUE, xlab= "Risk (STDEV)", ylab="Retrun", col="#2E9FDF",
     xlim=c(0,.04), ylim=c(-0.002,.002), cex.lab=2, cex.axis=1.5, cex.main=2.5, main="Risk & Retunr Scatter Plot and Market Line")
abline(v=0, h=0, col="skyblue")
rMarket= mean(d$Return, na.rm=TRUE)
stdMarket = sd(d$Return, na.rm=TRUE)
abline(rMarket,stdMarket, col="darkred")
dev.off()



cor(RR$TReturn, RR$TRisk, method = "pearson")
cor.test(RR$TReturn, RR$TRisk, method = "pearson")

cor(RR$TReturn, RR$TRisk, method = "spearman")
cor.test(RR$TReturn, RR$TRisk, method = "spearman")

cor(RR$TReturn, RR$TRisk, method = "kendall")
cor.test(RR$TReturn, RR$TRisk, method = "kendall")


#=====================
#=====================
# Question 5)
#Show the distribution of Risk and Return of the Market as well as those of Amazon & Apple.
#Market Price
jpeg('Market_Price_Histogram.jpg', width=2500, height = 1500)
hist(mktdaily$averagedaily,br=100,col="gold",xlab="Price",ylab="Frequency", cex.lab=2, cex.axis=2.5, cex.main=2.5,
     freq=TRUE, main="Histogram of Stock Market Prices")
dev.off()

jpeg('Market_Price_Kernel_Density_Plot.jpg', width=1500, height = 850)
trt <- density(mktdaily$averagedaily)
plot(trt) 
polygon(trt, col="gold", border="black", cex.lab=2, cex.axis=3, cex.main=6)
dev.off()

#Market Return
jpeg('Market_Return_Histogram2.jpg', width=2500, height = 1500)
hist(RR$TReturn,br=100,col="dark green",xlab="Return",ylab="Frequency", xlim=c(-0.001,0.0025), cex.lab=2, cex.axis=2.5, cex.main=2.5,
     freq=TRUE, main="Histogram of Stock Market Returns")
abline(h = 0, v = 0:2/2, lty = 4, col = "red")
dev.off()

#Market Risk
jpeg('Market_Risk_Histogram.jpg', width=3000, height = 1500)
hist(RR$TRisk,br=100,col="red",xlab="Return",ylab="Frequency", cex.lab=2, cex.axis=1.5, cex.main=2.5,
     freq=TRUE, main="Histogram of Stock Market Risk")
dev.off()

#Market Risk Density
jpeg('Market_Risk_Density_Histogram.jpg', width=3000, height = 1500)
hist(RR$TRisk,br=100,col="red",xlab="Return",ylab="Density", cex.lab=2, cex.axis=1.5, cex.main=2.5,
     freq=FALSE, main="Density of Stock Market Risk")
lines(density(RR$TRisk),col="black")
dev.off()

#==Amazon & Apple Return
ReturnAMZN = dsplit$AMZN$Return
ReturnAAPL = dsplit$AAPL$Return
library(ggplot2)

jpeg('Amazon_Apple_Retrun_Histogram.jpg', width=3000, height = 1500)
plot(hist(ReturnAMZN,br=100, xlab="Return", ylab="Frequency", main="Histogram of Amazon vs Apple Returns"), 
     col=rgb(0,0,1,1/8), xlim=c(-0.05,0.05))
plot(hist(ReturnAAPL,br=100, add=T, freq=TRUE), col=rgb(1,0,0,1/8), add=T)
legend("topright", legend=c("AMZN", "AAPL"), fill=c(rgb(0,0,1,1/8),rgb(1,0,0,1/8)), lty=1:3, cex=5.4, text.font=2, pt.lwd =50)
dev.off()


'''par(mfrow=c(1,2)) #side by side'''
#box()
#==

#=====================
#=====================
# Question 6)
#How's the frequency of the Market return's on a scale of least favorable and the most favorable? Show it on a pie chart.
median(RR$TReturn)
mean(RR$TReturn)
quantile(RR$TReturn, 0.8)
RR$ReturnFreq <- cut(RR$TReturn, 
                   breaks=c(-Inf, 0.000, 0.0005286672, 0.0008247795, Inf), 
                   labels=c("Below Zero","Below Average", "Above Average", "TOP 20%"))


library(plotrix)
jpeg('Pie chart Frequency of Returns.jpg', width=1500, height = 1000)
slices <- table(RR$ReturnFreq)
lbls <- c("Below Zero ","Greater than zero, Below Average", "Above Average", "(r >= 0.082%), TOP ")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="") 
pie3D(slices,labels=lbls,explode=0.1, labelcex = 2, cex.main=2.5, col=c("grey","red","dark green","gold"), main="Frequency of Returns")
dev.off()


#=====================
#=====================
# Question 7)
#What are the Top 10 stocks based on Return? Also, illustrate the Top 10 return stcoks along with their corresponding risk level.
#Top 10 return stocks
HighestReturn=tapply(d$Return,d$Name,mean, na.rm = TRUE)
HighestR=sort(HighestReturn, decreasing = T)[1:10]

jpeg('Top_10_Return.jpg', width=1500, height = 1000)
barplot(HighestR, main="Top 10 stocks based on Return", horiz=F, col=heat.colors(10))
dev.off()

#Retrun with Risk
HighestR1=tapply(d$Return,d$Name,mean, na.rm = TRUE)
HighestR2=tapply(d$Return,d$Name,sd, na.rm = TRUE)
NameRnR = unique(d$Name)
length(HighestR1)
length(HighestR2)
length(NameRnR)
HighestReturnnR = data.frame(HighestR1, HighestR2, NameRnR)
names(HighestReturnnR) = c("Return","Risk", "Name")
head(HighestReturnnR)
HighestRnR= HighestReturnnR[order(-HighestReturnnR$Return), ]
head(HighestRnR)  
Highest10RnR = HighestRnR[c(1:10),c(1:3)]
Highest10RnR

jpeg('Top_10_Return_With_Risk.jpg', width=800, height = 600)
ggplot(data=Highest10RnR, aes(x=Name, y=Return, fill=Risk)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()
dev.off()

#=====================
#=====================
# Question 8)
#List Top 30 stock with the highest Risk-Adjusted Return.
#Sharpe Ratio
#The Sharpe ratio is simply the return per unit of risk (represented by variability). 
fRAR = function(a,b){
  (a-0.0005286672)/b
  }
HighestReturnnR$RiskAdjustedReturn =  with (HighestReturnnR, {RiskAdjustedReturn = (Return-(mean(RR$TReturn)))/Risk})
mean(RR$TReturn)
head(HighestReturnnR)
HighestRAR= HighestReturnnR[order(-HighestReturnnR$RiskAdjustedReturn), ]
head(HighestRAR)  
Highest30RAR = HighestRAR[c(1:30),]
Highest30RAR

jpeg('Risk Adjusted Return.jpg', width=1000, height = 800)
p= ggplot(data=Highest30RAR, aes(x=reorder(Name, RiskAdjustedReturn), y=RiskAdjustedReturn, fill="")) +
  geom_bar(stat="identity", color="black", fill="darkred", position=position_dodge())+ xlab("Name") + ylab("Risk Adjusted Return") + 
  theme_minimal() 
p + coord_flip()
dev.off()

#=====================
#=====================
# Question 9)
#Is there any correlation between volume of stock traded with corresponding Risk & Return? 
install.packages("scatterplot3d") 
library("scatterplot3d")
TVolume <- aggregate(volume ~ Name, data=d, FUN=mean)
head(TVolume)
dim(TVolume)
RRV3D= merge(x=RR, y=TVolume, by='Name')
dim(RRV3D)
typeof(RRV3D)
normalizedRRV = scale(RRV3D[,c(2,3,5)])
head(normalizedRRV)

jpeg('Risk-Return-Volume Scatter Plot_standardaized.jpg', width=1500, height = 1000)
scatterplot3d(normalizedRRV[,c(1:3)],
              main="Risk-Return-Volume Scatter Plot",
              pch = 8, color="steelblue", angle=35,
              xlim=c(-2,2), ylim=c(0,2), zlim=c(-2,4),
              xlab = "Risk (STDEV)",
              ylab = "Return ",
              zlab = "Volume (log)")
dev.off()

#=====
#Is there any relationship between Volume and Return or Risk?
#Normalization
#MaxMinRRV = normalize(x, range = c(0, 1), domain = range(x, na.rm = TRUE),
MaxMinRRV = merge(x=RR, y=TVolume, by='Name')
MaxMinRRV = MaxMinRRV[,c(2,3,5)]
normalize = function(x) {
  return ((x - min(x)) / (max(x) - min(x)))}
MaxMinNorlmalizedRRV = as.data.frame(lapply(MaxMinRRV, normalize))  
head(MaxMinNorlmalizedRRV)

jpeg('Risk_Volume_ScatterPlot_limited.jpg', width=2000, height = 1200)
plot(x=MaxMinNorlmalizedRRV[,2], y=MaxMinNorlmalizedRRV[,3], pch = "+", cex=4, frame = TRUE, xlab="Risk (STDEV)" , ylab="Volume", col="#2E9FDF",
     xlim=c(0,.2), ylim=c(0,.4), cex.lab=2, cex.axis=1.5, cex.main=2.5, main="Risk & Volume Scatter Plot - Normalized Data")
dev.off()


jpeg('Return_Volume_ScatterPlot_limited.jpg', width=2000, height = 1200)
plot(x=MaxMinNorlmalizedRRV[,3], y=MaxMinNorlmalizedRRV[,1], pch = "+", cex=4, frame = TRUE, xlab= "Return", ylab="Volume", col="#2E9FDF",
     xlim=c(0,.2), ylim=c(0.4,1), cex.lab=2, cex.axis=1.5, cex.main=2.5, main="Retuen & Volume Scatter Plot - Normalized Data")
dev.off()

'''
qplot(MaxMinNorlmalizedRRV[,2], MaxMinNorlmalizedRRV[,3], geom=c("point", "smooth"), xlab="Risk", ylab= "Volume", xlim=c(0,.2), ylim=c(0,.4))
qplot(MaxMinNorlmalizedRRV[,3], MaxMinNorlmalizedRRV[,1], geom=c("point", "smooth"), xlab="Volume", ylab= "Return", xlim=c(0,.2), ylim=c(.4,1))
qplot(MaxMinNorlmalizedRRV[,2], MaxMinNorlmalizedRRV[,1], geom=c("point", "smooth"), xlab="Risk", ylab= "Return", xlim=c(0,.5), ylim=c(.5,1))
'''

length(RRV3D$volume)
length(RRV3D$TRisk)
length(RRV3D$TReturn)
##=======
##=======
##===
#correlation Test (absolute values)
#Volume and Risk (absolute values)
cor(RRV3D$volume, RRV3D$TRisk, method = "pearson")
X23PP = cor.test(RRV3D$volume, RRV3D$TRisk, method = "pearson")

cor(RRV3D$volume, RRV3D$TRisk, method = "spearman")
X23SS =cor.test(RRV3D$volume, RRV3D$TRisk, method = "spearman")

cor(RRV3D$volume, RRV3D$TRisk, method = "kendall")
X23KK= cor.test(RRV3D$volume, RRV3D$TRisk, method = "kendall")
#
#Volume and Return (absolute values)
cor(RRV3D$volume, RRV3D$TReturn, method = "pearson")
X13PP =cor.test(RRV3D$volume, RRV3D$TReturn, method = "pearson")

cor(RRV3D$volume, RRV3D$TReturn, method = "spearman")
X13SS =cor.test(RRV3D$volume, RRV3D$TReturn, method = "spearman")

cor(RRV3D$volume, RRV3D$TReturn, method = "kendall")
X13KK =cor.test(RRV3D$volume, RRV3D$TReturn, method = "kendall")

##===
#correlation Test (Normalized values)
#Volume and Risk (Normalized values)
cor(MaxMinNorlmalizedRRV[,2], MaxMinNorlmalizedRRV[,3], method = "pearson")
X23P = cor.test(MaxMinNorlmalizedRRV[,2], MaxMinNorlmalizedRRV[,3], method = "pearson")

cor(MaxMinNorlmalizedRRV[,2], MaxMinNorlmalizedRRV[,3], method = "spearman")
X23S = cor.test(MaxMinNorlmalizedRRV[,2], MaxMinNorlmalizedRRV[,3], method = "spearman")

cor(MaxMinNorlmalizedRRV[,2], MaxMinNorlmalizedRRV[,3], method = "kendall")
X23K = cor.test(MaxMinNorlmalizedRRV[,2], MaxMinNorlmalizedRRV[,3], method = "kendall")
#
#Volume and Return (Normalized values)
cor(MaxMinNorlmalizedRRV[,1], MaxMinNorlmalizedRRV[,3], method = "pearson")
X13P =cor.test(MaxMinNorlmalizedRRV[,1], MaxMinNorlmalizedRRV[,3], method = "pearson")

cor(MaxMinNorlmalizedRRV[,1], MaxMinNorlmalizedRRV[,3], method = "spearman")
X13S = cor.test(MaxMinNorlmalizedRRV[,1], MaxMinNorlmalizedRRV[,3], method = "spearman")

cor(MaxMinNorlmalizedRRV[,1], MaxMinNorlmalizedRRV[,3], method = "kendall")
X13K = cor.test(MaxMinNorlmalizedRRV[,1], MaxMinNorlmalizedRRV[,3], method = "kendall")



##Print
X23PP
X23SS
X23KK
X13PP
X13SS
X13KK
#
X23P
X23S
X23K
X13P
X13S
X13K

#=====================
#=====================
# Question 10)
#What percent of the Market Turn-over is coming from the Top 10 with the highest volume traded?
HighestVolume=tapply(d$volume,d$Name, sum, na.rm = TRUE)
head(HighestVolume)
HighestV=sort(HighestVolume, decreasing = T)[1:10]
HighestVpct= apply(HighestV,1,function(x) (100*x)/sum(d$volume))
head(HighestVpct)
sum(HighestVpct)     
RestoftheMarket = 100- sum(HighestVpct)

library(plotrix)
jpeg('Market Turn-over Top 10 compared to whole market.jpg', width=1500, height = 1000)
slices <- c(sum(HighestVpct), RestoftheMarket)
lbls <- c("TOP TEN","Rest of the Market")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="") 
pie3D(slices,labels=lbls,explode=0.25, labelcex = 3, cex.main=2.5, col=c("Gold","skyblue"), main="Volume/Turn-over of Top 10 vs The rest of Market")
dev.off()

