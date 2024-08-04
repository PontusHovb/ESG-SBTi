rm(list=ls(all=TRUE))
library(timeSeries)

trading_days_in_year <- 250

####################################################################################
#################################### Load files ####################################
SBTi <- read.csv2(file="SBTi.csv",header=T,sep=",")
Prices <- read.csv2(file="Prices.csv",header=T,sep=",")
Returns <- read.csv2(file="Returns.csv",header=T,sep=",")
rownames(Returns) <- Prices[,'Date'][-1]
Identifiers <-read.csv(file="Identifiers.csv",header=T,sep=",")

####################################################################################
############################ Filter out relevant stocks ############################
countries <- unique(Identifiers$Country.of.Headquarters)
Assets_USA <- Identifiers$Identifier[Identifiers$Country.of.Headquarters%in%countries[25]]
SBTi_US <- SBTi[SBTi$Country.of.Headquarters == "United States of America", c("Instrument", "BA1.5Date")]
SBTi_US <- merge(data.frame(Instrument = Assets_USA), SBTi_US, by = "Instrument", all.x = TRUE)

####################################################################################
################################# Create portfolios ################################
SBTi_Regression <- matrix(NA, nrow = 0, ncol = 4)
SBTi_Returns <- matrix(NA, nrow = 0, ncol = 3)
SBTi_Size <- matrix(NA, nrow = 0, ncol = 3)
colnames(SBTi_Regression) <- c("Date", "Portfolio excess return", "SBTi", "Benchmark excess return")
colnames(SBTi_Returns) <- c("Date", "SBTi", "non-SBTi")
colnames(SBTi_Size) <- c("Date", "SBTi", "non-SBTi")

i = 1
for(date in rownames(Returns)) {
  # Update progress
  if (i %% 100 == 0) { print(sprintf("%.2f%%", 100* i / length(rownames(Returns)))) }
  
  # Split stocks into two portfolios
  SBTi_Stocks <- SBTi_US$Instrument[date >= SBTi_US$BA1.5Date & !is.na(SBTi_US$BA1.5Date)]
  nonSBTi_Stocks <- SBTi_US$Instrument[is.na(SBTi_US$BA1.5Date) | (date < SBTi_US$BA1.5Date & !is.na(SBTi_US$BA1.5Date))]
  
  # Calculate only if there is two portfolios
  if(length(SBTi_Stocks) > 0) {
    
    # Calculate risk-free and market returns
    risk_free_yield <- as.numeric(Prices[i+1, ncol(Prices)])
    risk_free_daily <- (1 + risk_free_yield)^(1/trading_days_in_year) - 1
    benchmark_ret <- as.numeric(Prices[i+1, ncol(Prices)-1]) / as.numeric(Prices[i, ncol(Prices)-1]) - 1
    
    # Save returns
    SBTi_Regression <- rbind(SBTi_Regression, c(date, (mean(na.omit(as.numeric(Returns[i, SBTi_Stocks]))) - risk_free_daily)*100, 1, (benchmark_ret - risk_free_daily)*100))         # Add SBTi return
    SBTi_Regression <- rbind(SBTi_Regression, c(date, (mean(na.omit(as.numeric(Returns[i, nonSBTi_Stocks]))) - risk_free_daily)*100, 0, (benchmark_ret - risk_free_daily)*100))      # Add non-SBTi return
    
    # Save portfolio size & returns
    SBTi_Size <- rbind(SBTi_Size, c(date, length(SBTi_Stocks), length(nonSBTi_Stocks)))
    SBTi_Returns <- rbind(SBTi_Returns, c(date, mean(na.omit(as.numeric(Returns[i, SBTi_Stocks]))), mean(na.omit(as.numeric(Returns[i, nonSBTi_Stocks])))))
  }
  i = i + 1
}

SBTi_Returns_Selected <- SBTi_Returns[SBTi_Returns[,"Date"] > as.Date("2019-09-01"), ]

####################################################################################
############################### Portfolio statistics  ##############################
Des.Portfolios <- matrix(NA,nr=2,nc=4)

# Full period
SBTi_ret <- as.numeric(SBTi_Returns[, 2])*100
nonSBTi_ret <- as.numeric(SBTi_Returns[, 3])*100

# Selected period
SBTi_ret <- as.numeric(SBTi_Returns_Selected[, 2])*100
nonSBTi_ret <- as.numeric(SBTi_Returns_Selected[, 3])*100

# Calculate statistics
Des.Portfolios[1,]<-round(c(mean(SBTi_ret),sd(SBTi_ret),mean(SBTi_ret)/sd(SBTi_ret),quantile(SBTi_ret,0.01)),2)
Des.Portfolios[2,]<-round(c(mean(nonSBTi_ret),sd(nonSBTi_ret),mean(nonSBTi_ret)/sd(nonSBTi_ret),quantile(nonSBTi_ret,0.01)),2)

colnames(Des.Portfolios)<-c("Ave. Ret.","SD","SR","VaR")
rownames(Des.Portfolios)<-c("SBTi", "non-SBTi")
Des.Portfolios

####################################################################################
############################ Plot portfolio performance  ###########################
# Full period
Portfolio_Wealth<-data.frame(matrix(NA,nr=nrow(SBTi_Returns)+1,nc=2))
Portfolio_Wealth[1,]<-100
for (i in 2:nrow(Portfolio_Wealth)){
  Portfolio_Wealth[i,]<-Portfolio_Wealth[i-1,]*(1+as.numeric(SBTi_Returns[i-1, c(2,3)]))
}
rownames(Portfolio_Wealth) <- c(as.Date(SBTi_Returns[1, 1])-1,as.Date(SBTi_Returns[, 1]))
Portfolio_Wealth<-timeSeries(Portfolio_Wealth)

# Selected period
Portfolio_Wealth<-data.frame(matrix(NA,nr=nrow(SBTi_Returns_Selected)+1,nc=2))
Portfolio_Wealth[1,]<-100
for (i in 2:nrow(Portfolio_Wealth)){
  Portfolio_Wealth[i,]<-Portfolio_Wealth[i-1,]*(1+as.numeric(SBTi_Returns_Selected[i-1, c(2,3)]))
}
rownames(Portfolio_Wealth) <- c(as.Date(SBTi_Returns_Selected[1, 1])-1,as.Date(SBTi_Returns_Selected[, 1]))
Portfolio_Wealth<-timeSeries(Portfolio_Wealth)

# Plot
plot(Portfolio_Wealth[,1],type="l",col="blue",lwd=2,main="Portfolio Performance",
     ylab="Wealth",xlab="Time",ylim=range(Portfolio_Wealth))
lines(Portfolio_Wealth[,2],col="red",lwd=2)
legend("topleft",legend = c("SBTi", "non-SBTi"),
       col = c("blue","red"),
       lty = 1, lwd = 2, bty = "n")

####################################################################################
################################ Regression analysis ###############################
# Full period
regression_input <- SBTi_Regression[,c("Portfolio excess return", "SBTi", "Benchmark excess return")]
regression_input <- apply(regression_input, 2, as.numeric)
head(regression_input)
regression_model <- lm(regression_input[,"Portfolio excess return"] ~ regression_input[,"Benchmark excess return"] + regression_input[,"SBTi"])

# Mininimum 5 stocks
regression_input <- SBTi_Regression[SBTi_Regression[,"Date"] > as.Date("2019-09-01"),c("Portfolio excess return", "SBTi", "Benchmark excess return")]
regression_input <- apply(regression_input, 2, as.numeric)
head(regression_input)
regression_model <- lm(regression_input[,"Portfolio excess return"] ~ regression_input[,"Benchmark excess return"] + regression_input[,"SBTi"])

# Summary regression
summary(regression_model)

# (Externally) Studentized Residuals
# Create QQ-plot
qqnorm(rstudent(regression_model))  # rstudent = externally studentized
qqline(rstudent(regression_model), col='red')

# Create residuals vs. fitted
plot(regression_model$fitted.values, regression_model$residuals, 
     main = "Residuals vs. Fitted Values",
     xlab = "Fitted Values",
     ylab = "Studentized Residuals")
abline(0, 0)
abline(0.021, 0, col='red', lty='dashed')
abline(-0.021, 0, col='red', lty='dashed')

# Cook's Distance
library(olsrr)
n <- nrow(regression_input)
cooks.cutoff <- 4/n
full.cooks <- cooks.distance(regression_model)
full.cooks[full.cooks > cooks.cutoff]
ols_plot_cooksd_bar(regression_model)

####################################################################################
################################## Portfolios size #################################
plot(as.Date(SBTi_Size[, 1]), as.numeric(SBTi_Size[, 2]), type = "l", col = "blue", xlab = "Date", ylab = "No stocks", ylim=c(0, 500))
lines(as.Date(SBTi_Size[, 1]), as.numeric(SBTi_Size[, 3]), type = "l", col = "red")
legend("left", legend = c("non-SBTi", "SBTi"), col = c("red", "blue"), lty = 1, lwd = 2, bty = "n")


####################################################################################
############################### Portfolio composition ##############################
# Total
SBTi_sectors <- Identifiers[, "TRBC.Economic.Sector"]
sector_counts <- table(SBTi_sectors)
sector_percentages <- prop.table(sector_counts) * 100
print(sector_percentages)

# SBTi
SBTi_sectors <- Identifiers[Identifiers$Identifier%in%SBTi_Stocks, "TRBC.Economic.Sector"]
sector_counts <- table(SBTi_sectors)
sector_percentages <- prop.table(sector_counts) * 100
print(sector_percentages)

# non-SBTi
nonSBTi_sectors <- Identifiers[Identifiers$Identifier%in%nonSBTi_Stocks, "TRBC.Economic.Sector"]
sector_counts <- table(nonSBTi_sectors)
sector_percentages <- prop.table(sector_counts) * 100
print(sector_percentages)