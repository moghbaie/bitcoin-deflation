library(shiny)
require(quantmod)
library(zoo)
library(PerformanceAnalytics)
library(forecast)
library(TSA)
library(FinTS)
library(xts)
library(corrplot)
library(psych)
library(hydroTSM)
#~/Documents/FE541/blockchain-github
#~/Documents/FE541/shiny/
bc <- read.csv("bit_usd.csv", header=TRUE)
data <- read.csv("bitcoin_data.csv")
dxy <- read.csv("dxy.txt")
ir <- read.csv("Inflation_rate.csv")


# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  # Generate a plot of the requested variable against mpg and only 
  # include outliers if requested
  output$Plot1 <- renderPlot({
    plot(bc$Date,bc$Close.Price,pch=1 , col='black', main="BIT/USD daily close price")
  })
  output$Plot2 <- renderPlot({
    plot(dxy$Close, pch=1 , col='black',main="DXY within the same time period")
  })
  output$Plot3 <- renderPlot({
    par(mfrow=c(2,2))
    hist(bc$Close.Price)
    hist(dxy$Close)
    hist(log(dxy$Close))
    hist(log(bc$Close.Price))
  })
  
  #adding the return to the bc data
  bc_Return <- diff(bc$Close.Price)/bc$Close.Price[-length(bc$Close.Price)]
  bc$bc_Return <- 0
  bc$bc_Return[2:length(bc$bc_Return)]<-bc_Return
  output$Plot7 <- renderPlot({
    hist(bc_Return)
  })
  #adding the return to the dxy data
  dxy_Return <- diff(dxy$Close)/dxy$Close[-length(dxy$Close)]
  dxy$dxy_Return <- 0
  dxy$dxy_Return[2:length(dxy$dxy_Return)]<-dxy_Return
  #output$Plot8 <- renderPlot({
   # hist(dxy_Return)
  #})
  
  #change the date format
  bc$Date <- as.Date(bc$Date, origin = "2013-05-31")
  dxy$Date <- as.Date(dxy$Date,format="%m/%d/%Y")
  ir$Date <- as.Date(ir$Date,format="%Y-%m-%d")
  data$Date <- as.Date(data$Date,format="%Y-%m-%d")
  
  
  #matching the data together by the dates that they both have data
  merge<- merge(dxy,bc,by="Date")
  #x<-merge$Close.Price
  
  output$Plot9 <- renderPlot({
    #plot the scaled close prices of bit/usd and dxy in the same graph
    plot(merge$Date,scale(merge$Close.Price),col="red",ylab="Scaled data")
    lines(scale(merge$Close.Price),col="red")
    lines(merge$Date, scale(merge$Close),col="blue")
    legend("topright", legend=c("BIT/USD","DXY"),col=c("red","blue"),lw=3,cex=1,pt.cex=0.5)
  })
  
  output$Plot10 <- renderPlot({
    #plot the return of bit/usd and dxy in the same graph
    plot(merge$Date, merge$bc_Return,pch=1,col="red",ylab="return data",xlab="time")
    lines(merge$Date, merge$dxy_Return, col="blue")
    legend("topright", legend=c("BIT/USD_return","DXY_return"),col=c("red","blue"),lw=3,cex=1,pt.cex=0.5)
  })
  #simple linear regression: bitcoin ~ dxy
  fit <- lm(merge$Close.Price~merge$Close)
  summary(fit)
  output$Plot11 <- renderPlot({
    plot(merge$Close,merge$Close.Price)
    abline(fit,col="red")
  })
  
  #time series analysis
  myvars <- c("Date","Close.Price","Close")
  bit <- merge[myvars]
  #change variable names for easier recognition
  colnames(bit)[2] <- "bit.close"
  colnames(bit)[3] <- "dxy.close"
  #plot the time sesies data
  bit.zoo <- zoo(bit[,-1],order.by=bit$Date)
  output$Plot12 <- renderPlot({
    plot(bit.zoo,main="Daily Close Price - bit/usd and DXY")
  })
  output$Plot13 <- renderPlot({
    #moving average of both price: to create a more smooth line
    plot(rollapply(bit.zoo, width=20, mean, na.rm=T), main="Mean of Rolling 20 Obs.")
  })
  #now perform regression analysis where dependent variable used bit/usd and dxy
  #is the indepedent variable
  tmp <- merge(bit.zoo[,"bit.close"], bit.zoo[,"dxy.close"])
  tmp.ret <- CalculateReturns(tmp)
  colnames(tmp) <- colnames(tmp.ret) <- c("bit","dxy")
  output$Plot14 <- renderPlot({
    #plot the return series
    plot(tmp.ret, main="Return Series for bit/usd and US dollar Index", xlab="Date")
  })
  output$Plot15 <- renderPlot({
    #return series in x-y plot
    plot(coredata(na.omit(tmp.ret)), pch=".", main="Return Series for bit/usd vs US dollar Index")
  })
  #regression analysis
  rr <- rollapply(na.approx(na.trim(tmp, side="both")), width = 20,
                  FUN = function(z) coef(lm(bit ~ dxy, data = as.data.frame(z))),
                  by.column = FALSE, align = "right")
  
  rr.var <- rollapply(na.approx(na.trim(tmp, side="both")), width = 20,
                      FUN = function(z) sd(residuals(lm(bit ~ dxy, data = as.data.frame(z)))),
                      by.column = FALSE, align = "right")
  
  res <- merge(rr,rr.var)
  colnames(res)[3] <- "St. Dev. Resid." 
  output$Plot16 <- renderPlot({
    plot(res, main="Coefficients of Regs.", xlab="Date")
  })
  
  summary(res)
  output$Plot17 <- renderPlot({
    hist(res[,"dxy"], breaks=100, main="Histogram of Estimated Coefficients", xlab="Value")
  })
  
  # For return series
  rr <- rollapply(na.approx(na.trim(tmp.ret, side="both")), width = 20,
                  FUN = function(z) coef(lm(bit ~ dxy, data = as.data.frame(z))),
                  by.column = FALSE, align = "right")
  
  rr.var <- rollapply(na.approx(na.trim(tmp.ret, side="both")), width = 20,
                      FUN = function(z) sd(residuals(lm(bit ~ dxy, data = as.data.frame(z)))),
                      by.column = FALSE, align = "right")
  
  res.ret <- merge(rr,rr.var)
  colnames(res.ret)[3] <- "St. Dev. Resid." 
  output$Plot18 <- renderPlot({
    plot(res.ret, main="Coefficients of Regs for Return Seris.", xlab="Date")
  })
  output$Plot19 <- renderPlot({
    hist(res.ret[,"dxy"], breaks=100, main="Histogram of Estimated Coefficients for Return Series", xlab="Value")
  })
  #univariate time series analysis
  bit.ts <- ts(bit$bit.close)
  output$Plot20 <- renderPlot({
    tsdisplay(diff(log(bit.ts)))
  })
  auto.arima(log(bit.ts))
  output$Plot21<- renderPlot({
    tsdiag(arima(log(bit.ts), order = c(0,1,0)))
  })
  
  output$Plot22 <- renderPlot({
    #ploting monthly ir graph
    plot(ir$Date,ir$Inflation_rate,main="Monthly Inflation Rate",pch=1,col='black', cex=0.2,xlab='Date', ylab='Inflation_Rate')
    lines(ir$Date,ir$Inflation_rate,col='black')
  })
  inflation <- xts(ir[,-1], order.by=as.Date(ir[,1], "%m/%d/%Y"))
  
  # creating monthly bc and ir
  bc<- subset(bc,Date< as.Date("2016-11-01"))
  bchain <- xts(bc[,-1], order.by=as.Date(bc[,1], "%m/%d/%Y"))
  bchain<- bchain[complete.cases(bchain),]
  x <- bchain$bc_Return
  output$Plot23 <- renderPlot({
    plot(x, main="Bitcoin Return Daily")
  })
  
  bcm<- daily2monthly(x, FUN=mean, date.fmt ="%m/%Y", na.rm=TRUE)
  output$Plot24 <- renderPlot({
    plot(bcm, main="Bitcoin Return Monthly")
  })
  
  dxy_Return<-subset(dxy,Date<as.Date("2016-11-01"),select = c(Date,dxy_Return))
  dxt <- xts(dxy_Return[,-1], order.by=as.Date(dxy_Return[,1], "%m/%d/%Y"))
  output$Plot25 <- renderPlot({
    plot(dxt, main="dxy Return Daily")
  })
  dxm<- daily2monthly(dxt, FUN=mean, date.fmt ="%m/%Y", na.rm=TRUE)
  output$Plot26 <- renderPlot({
    plot(dxm, main="dxy Return Monthly")
  })
  
  data_bc<-xts(data[,-1],order.by=as.Date(data[,1], "%Y-%m-%d"))
  datam<-daily2monthly(data_bc, FUN=mean, date.fmt ="%m/%Y", na.rm=TRUE)

  colnames(datam)<- c('Tot_BC','BC_Est_Trans_USD','BCMC','BC_Trade_vs_Trans')
  output$Plot27 <- renderPlot({
    plot(datam, main="Bitcoin monthly data", cex.lab = 1.5)
  })
  
  write.csv(datam,'datam.csv')
  #filtering ir on Date
  irt <- subset(ir,Date>=as.Date("2013-05-01"))
  merge1=merge(dxm,bcm,irt$Inflation_rate)
  colnames(merge1)<- c('dxm','bcm','ir')
  output$Plot28 <- renderPlot({
    plot(merge1,main="")
  })
  #cor(merge1)
  output$Plot29 <- renderPlot({
    corrplot(cor(merge1), order = "hclust", tl.col='black', tl.cex=.75)
  })
  
  #plot.new()
  par(mfrow=c(1,1))
  output$Plot30 <- renderPlot({
    par(mfrow=c(2,2))
    hist(dxm)
    hist(bcm)
    hist(irt$Inflation_rate)
  })
 
  
  ir.reg<-lm(merge1$ir~merge1$bcm+merge1$dxm)
  #anova(ir.reg)
  par(mfrow=c(2,2))
  output$Plot33 <- renderPlot({
    par(mfrow=c(2,2))
    plot(lm(residuals(ir.reg)~fitted(ir.reg)),col='red')
  })
  
  par(mfrow=c(1,1))
  #summary(ir.reg)
  output$Plot34 <- renderPlot({
    plot(ir~bcm, data=merge1, main="Inflation Rate to Bitcoin Return")
    abline(ir.reg,col="red")
    lines(lowess(merge1$ir~merge1$bcm),col="blue")
  })
  
  #economic data
  econ_data<- read.csv("Total_data1.csv")
  econ_data$Date <- as.Date(econ_data$Date,format="%m/%d/%Y")
  #str(econ_data)
  output$Plot34 <- renderPlot({
    corrplot(cor(econ_data[,-1]), order = "hclust", tl.col='black', tl.cex=.75)
  })
  
  output$Plot35 <- renderPlot({
    plot(econ_data$Date,econ_data$Inflation_Rate, pch=1 , col='black',main="Inflation Rate")
  })
  
  output$Plot36 <- renderPlot({
    #cor(econ_data[,-1])
    plot(scale(econ_data$Bitcoin_Est_Transaction_Vol_USD)~econ_data$Date,cex=0.2, xlab="Date",ylab="")
    lines(econ_data$Date, scale(econ_data$Bitcoin_Est_Transaction_Vol_USD),col="black",lwd=2)
    lines(econ_data$Date, scale(econ_data$Inflation_Rate),col="blue", lwd=3.5)
    lines(econ_data$Date, scale(econ_data$Global_Price_Brent_Crude),col="red")
    lines(econ_data$Date, scale(econ_data$Customer_Price_Index),col="green")
    lines(econ_data$Date, scale(econ_data$Trade_Weighted_Dollar),col="purple")
    legend("topleft", legend=c("Bitcoin_Est_Transaction_Vol_USD","Inflation_Rate","Global_Price_Brent_Crude","Customer_Price_Index","Trade_Weighted_Dollar"),col=c("black","blue","red","green","purple"),lw=3,cex=1,pt.cex=0.5)
  })
 
  
})