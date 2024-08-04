rm(list=ls(all=TRUE))
setwd("~/Google Drive/My Drive/Utbildning/ME2323 Asset pricing & Climate mitigation/Project/Data")

####################### Constants #######################
start_date <-"2011-01-01"
end_date <- "2022-12-31"
trading_days_in_year <- 250

################## Loading R packages  ##################
library(lubridate)
library(timeSeries)

################# Importing Data Files ##################
Identifiers <-read.csv(file="Identifiers.csv",header=T,sep=",")
Prices <-read.csv2(file="Prices.csv",header=T,sep=",")
ESG <-read.csv2(file="ESG.csv",header=T,sep=",")
SBTi <-read.csv2(file="SBTi.csv",header=T,sep=",")

##################### Process data  ##################### 
Pt <- Prices[, 2:(length(colnames(Prices))-2)]
Pt <- apply(Pt, 2, as.numeric)                                # Convert to numeric values for calculation
Rt <- Pt[2:nrow(Pt),]/Pt[1:(nrow(Pt)-1),]-1                   # Calculate daily returns
rownames(Rt) <- Prices[,'Date'][-1]
write.csv(Rt, file = "~/Google Drive/My Drive/Utbildning/ME2323 Asset pricing & Climate mitigation/Project/Data/Returns.csv", row.names = FALSE)

ESGt <- ESG[1:(nrow(ESG)-1), 2:length(colnames(ESG))]
ESGt <- apply(ESGt, 2, as.numeric)                            # Convert to numeric values for calculation
rownames(ESGt) <- ESG[1:(nrow(ESG)-1),'Date']

##################### Clean files  ##################### ¨
Rt_2011_2022 <- Rt[rownames(Rt) >= start_date & rownames(Rt) <= end_date, ]
Rt_NonMissing <- Rt_2011_2022[, colSums(is.na(Rt_2011_2022)) == 0]    # Non-missing returns
ESG_NonMissing <- ESGt[, colSums(is.na(ESGt)) == 0]                   # Non-missing ESG

################# Get lists of stocks  ################# 
countries <- unique(Identifiers$Country.of.Headquarters)
Assets_USA <- Identifiers$Identifier[Identifiers$Country.of.Headquarters%in%countries[25]]
Assets_NonMissing <- intersect(colnames(ESG_NonMissing), colnames(Rt_NonMissing))

##################### Filter stocks ##################### 
AssetN <- Assets_NonMissing[Assets_NonMissing%in%Assets_USA]
ESG_N <- ESG_NonMissing[,AssetN]
Rt_N <- Rt_NonMissing[,AssetN]

################### Calculate returns ################### 
rf <- Prices[Prices$Date%in%rownames(Rt_N), ncol(Prices)]
rf <- sapply(rf, as.numeric)

pm <- Prices[, ncol(Prices)-1]
pm <- sapply(pm, as.numeric)

rm <- pm[2:length(pm)]/pm[1:(length(pm)-1)]-1
rm <- sapply(rm, as.numeric)

names(rm) <- Prices$Date[-1]
rm<-rm[names(rm)%in%rownames(Rt_N)]*100
excess_rm<-rm-rf

#########################################################
########### Descriptive Statistics for Stocks ###########
#########################################################
Des.Stocks<-matrix(NA,nr=length(AssetN),nc=5)

for(. in 1:length(AssetN)){
  rt<-(Rt_N[,.])*100
  Des.Stocks[.,]<-round(c(mean(rt),sd(rt),mean(rt)/sd(rt),quantile(rt,0.01),mean(ESG_N[,.])),2)
}
colnames(Des.Stocks)<-c("Ave. Ret.","SD","SR","VaR","ESG")
rownames(Des.Stocks)<-AssetN

############### Summarize based on sector ############### 
IndustryMatch <- Identifiers[,c("Identifier", "TRBC.Economic.Sector")]
rownames(IndustryMatch) <- IndustryMatch[,1]
Des.Stocks <- merge(Des.Stocks, IndustryMatch, by = 0, all.x = TRUE)

sectors <- unique(Des.Stocks["TRBC.Economic.Sector"])
Des.Sectors<-matrix(NA,nr=nrow(sectors)+1,nc=6)

for(i in 1:nrow(sectors)){
  sector_stocks<-Des.Stocks[Des.Stocks$TRBC.Economic.Sector == sectors[i,],]
  Des.Sectors[i,]<-c(nrow(sector_stocks), mean(sector_stocks$`Ave. Ret.`),mean(sector_stocks$`SD`),mean(sector_stocks$`SR`),mean(sector_stocks$`VaR`),mean(sector_stocks$`ESG`))
}
Des.Sectors[i+1,]<-c(nrow(Des.Stocks), mean(Des.Stocks$`Ave. Ret.`),mean(Des.Stocks$`SD`),mean(Des.Stocks$`SR`),mean(Des.Stocks$`VaR`),mean(Des.Stocks$`ESG`))
colnames(Des.Sectors) <- c("No. stocks", "Ave. Ret.","SD","SR","VaR","ESG")
rownames(Des.Sectors) <- c(sectors$TRBC.Economic.Sector, "Total")

#########################################################
########## Constructing EQW and ESG-Portfolios ##########
#########################################################
Q1<-Q2<-Q3<-Q4<-data.frame(matrix(NA,nr=12,nc=93))
rownames(Q1)<-rownames(Q2)<-rownames(Q3)<-rownames(Q4)<-ESG$Date[1:12]
for (i in 1:12){
  esg<-ESG_N[i,]
  Q1[i,]<-names(sort(esg)[281:373])
  Q2[i,]<-names(sort(esg)[188:280])
  Q3[i,]<-names(sort(esg)[94:186])
  Q4[i,]<-names(sort(esg)[1:93])
}

ESG_Portfolios<-ESG_Scores<-matrix(NA,nr=nrow(Rt_N),nc=5)
rownames(ESG_Portfolios)<-rownames(ESG_Scores)<-rownames(Rt_N)
for(i in 1:nrow(ESG_Portfolios)){
  q1<-unlist(Q1[rownames(Q1)==(year(as.Date(rownames(ESG_Portfolios)[i]))-1),])
  q2<-unlist(Q2[rownames(Q2)==(year(as.Date(rownames(ESG_Portfolios)[i]))-1),])
  q3<-unlist(Q3[rownames(Q3)==(year(as.Date(rownames(ESG_Portfolios)[i]))-1),])
  q4<-unlist(Q4[rownames(Q4)==(year(as.Date(rownames(ESG_Portfolios)[i]))-1),])
  esg1<-mean(ESG_N[rownames(Q1)==(year(as.Date(rownames(ESG_Portfolios)[i]))-1),q1])
  esg2<-mean(ESG_N[rownames(Q2)==(year(as.Date(rownames(ESG_Portfolios)[i]))-1),q2])
  esg3<-mean(ESG_N[rownames(Q3)==(year(as.Date(rownames(ESG_Portfolios)[i]))-1),q3])
  esg4<-mean(ESG_N[rownames(Q4)==(year(as.Date(rownames(ESG_Portfolios)[i]))-1),q4])
  esg5<-mean(ESG_N[rownames(Q4)==(year(as.Date(rownames(ESG_Portfolios)[i]))-1),])
  ESG_Portfolios[i,]<-c(mean(Rt_N[i,q1]),mean(Rt_N[i,q2]),mean(Rt_N[i,q3]),mean(Rt_N[i,q4]),
                        mean(Rt_N[i,]))
  ESG_Scores[i,]<-c(esg1,esg2,esg3,esg4,esg5)
}

#########################################################
######### Task 5: Portfolio Performance Measures ########
#########################################################
Stat.Tab<-matrix(NA,nr=7,nc=5)
for(i in 1:5){
  rp<-ESG_Portfolios[,i]*100
  excess_rp<-rp-rf
  regression_model<-lm(excess_rp~excess_rm)
  Stat.Tab[,i]<-round(c(mean(rp),sd(rp),mean(rp)/sd(rp),quantile(rp,0.01),
                  mean(ESG_Scores[,i]),coef(regression_model)),3)
}

summary(regression_model)

# Statistics
rownames(Stat.Tab)<-c("Average Return", "Standard Deviation", "Sharpe Ratio",
                      "1%-VaR", "Average ESG", "Jensen's Alpha","Beta")
colnames(Stat.Tab)<-c("Best ESG Portfolio","Medium-Best ESG Portfolio",
                      "Worst-Medium ESG Portfolio","Worst ESG Portfolio", "EQW")
Stat.Tab


# Plot portfolio performance
Portfolio_Wealth<-data.frame(matrix(NA,nr=nrow(ESG_Portfolios)+1,nc=ncol(ESG_Portfolios)))
Portfolio_Wealth[1,]<-100
for (i in 2:nrow(Portfolio_Wealth)){
  Portfolio_Wealth[i,]<-Portfolio_Wealth[i-1,]*(1+ESG_Portfolios[i-1,])
}
rownames(Portfolio_Wealth)<-c(as.Date(rownames(ESG_Portfolios)[1])-1,
                              rownames(ESG_Portfolios))
Portfolio_Wealth<-timeSeries(Portfolio_Wealth)

plot(Portfolio_Wealth[,1],type="l",col="blue",lwd=2,main="Portfolio Performance",
     ylab="Wealth",xlab="Time",ylim=range(Portfolio_Wealth))
lines(Portfolio_Wealth[,2],col="red",lwd=2)
lines(Portfolio_Wealth[,3],col="gold",lwd=2)
lines(Portfolio_Wealth[,4],col="green",lwd=2)
lines(Portfolio_Wealth[,5],col="black",lwd=2)
legend("topleft",legend = colnames(Stat.Tab),
       col = c("blue","red","gold","green","black"),
       lwd=2, bty="n")

write.csv(Stat.Tab,file="Stat.Tab.csv")