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
library(nFactors)
options(shiny.maxRequestSize=10000*1024^2)
#~/Documents/FE541/bitcoin-deflation
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
    hist(bc$Close.Price, main="Bitcoin Close Price")
    hist(dxy$Close, main="Dollar Index")
    hist(log(bc$Close.Price), main="Log Bitcoin Close Price")
    hist(log(dxy$Close), main="Log Dollar Index")
  })
  
  #adding the return to the bc data
  bc_Return <- diff(bc$Close.Price)/bc$Close.Price[-length(bc$Close.Price)]
  bc$bc_Return <- 0
  bc$bc_Return[2:length(bc$bc_Return)]<-bc_Return
  #adding the return to the dxy data
  dxy_Return <- diff(dxy$Close)/dxy$Close[-length(dxy$Close)]
  dxy$dxy_Return <- 0
  dxy$dxy_Return[2:length(dxy$dxy_Return)]<-dxy_Return
  dxy_Return<-as.numeric(dxy_Return)
  
  output$Plot7 <- renderPlot({
    par(mfrow=c(1,2))
    hist(bc_Return, main="Histogram of Bitcoin close price return")
    hist(dxy$dxy_Return, main="Histogram of Dollar index return")
  })
  
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
  output$Print1 <-renderPrint({
    summary(fit)
  })
  output$Plot11 <- renderPlot({
    plot(merge$Close,merge$Close.Price)
    abline(fit,col="red")
  })
  output$Plot12 <- renderPlot({
    plot(fit$residual~merge$Date)
  })
  #time series analysis
  myvars <- c("Date","Close.Price","Close")
  bit <- merge[myvars]
  #change variable names for easier recognition
  colnames(bit)[2] <- "bit.close"
  colnames(bit)[3] <- "dxy.close"
  #plot the time sesies data
  bit.zoo <- zoo(bit[,-1],order.by=bit$Date)

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
  output$Print2 <-renderPrint({
    summary(res)
  })
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
  inflation <- xts(ir[,-1], order.by=as.Date(ir[,1], "%m/%d/%Y"))
  
  # creating monthly bc and ir
  bc<- subset(bc,Date< as.Date("2016-11-01"))
  bchain <- xts(bc[,-1], order.by=as.Date(bc[,1], "%m/%d/%Y"))
  bchain<- bchain[complete.cases(bchain),]
  bcm<- daily2monthly(bchain$bc_Return, FUN=mean, date.fmt ="%m/%Y", na.rm=TRUE)
  
  dxy_Return<-subset(dxy,Date<as.Date("2016-11-01"),select = c(Date,dxy_Return))
  dxt <- xts(dxy_Return[,-1], order.by=as.Date(dxy_Return[,1], "%m/%d/%Y"))
  dxm<- daily2monthly(dxt, FUN=mean, date.fmt ="%m/%Y", na.rm=TRUE)

  output$Plot26 <- renderPlot({
    par(mfrow=c(2,2))
    hist(ir$Inflation_rate, main="Histogram Monthly Inflation Rate")
    hist(bcm, main="Histogram Monthly Bitcoin Return")
    hist(dxm, main="Histogram Monthly dxy Return")
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
    plot(ir~bcm, data=merge1, main="Inflation Rate to Bitcoin Return")
    abline(ir.reg,col="red")
    lines(lowess(merge1$ir~merge1$bcm),col="blue")
  })
  
  ir.reg<-lm(merge1$ir~merge1$bcm+merge1$dxm)
  output$Print3 <-renderPrint({
    summary(ir.reg)
  })
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
  
  output$Plot37 <- renderPlot({
    hist(econ_data$Bitcoin_Est_Transaction_Vol_USD)
  })
  output$Plot38 <- renderPlot({
  plot(econ_data$Inflation_Rate~econ_data$Bitcoin_Est_Transaction_Vol_USD,cex=0.5, main='Inflation Rate to Bitcoin transaction Vol USD')
  abline(lm(econ_data$Inflation_Rate~econ_data$Bitcoin_Est_Transaction_Vol_USD),col='red')
    })
  output$Print4 <-renderPrint({
    summary(lm(econ_data$Inflation_Rate~econ_data$Bitcoin_Est_Transaction_Vol_USD))
  })
  reg.model2<-lm(econ_data$Inflation_Rate~econ_data$Bitcoin_Est_Transaction_Vol_USD)
  output$Plot39 <- renderPlot({
    par(mfrow=c(2,2))
    plot(lm(residuals(reg.model2)~fitted(reg.model2)),col='red')
  })
  mod_gam <- gam(econ_data$Inflation_Rate ~ s(econ_data$Bitcoin_Est_Transaction_Vol_USD,bs="cr"))
  output$Plot40 <- renderPlot({
    plot(mod_gam, pages=1, residuals=T, pch=19, cex=0.25,
         scheme=1, col='#FF8000', shade=T,shade.col='gray90') 
  })
  output$Print5 <-renderPrint({
    summary(mod_gam)
  })
  
  ev <- eigen(cor(econ_data[,c(-1,-2)])) # get eigenvalues
  ap <- parallel(subject=nrow(econ_data[,c(-1,-2)]),var=ncol(econ_data[,c(-1,-2)]),
                 rep=100,cent=.05)
  nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
  output$Plot41 <- renderPlot({
    plotnScree(nS) 
  })
  f3w <- fa(econ_data[,c(-1,-2)],2,n.obs = 100,fm="wls")
  output$Plot42 <- renderPlot({
    #plot(f3w)
    fa.diagram(f3w)
  })
  attach(econ_data)
  SumScale1<-(Producer_Price_Index+Export+Import+Customer_Price_Index+ 
                Global_Price_Brent_Crude + (10-Trade_Weighted_Dollar)+
                (10-+Bitcoin_Est_Transaction_Vol_USD))/7
  SumScale2<-(Total_Bitcoin+Bitcoin_market_Cap+ 
                Bitcoin_trade_Vol_VS_Transaction_Vol+Personal_Consumption_Expenditure)/4
  output$Plot43 <- renderPlot({
    par(mfrow=c(1,2))
    hist(SumScale1)
    hist(SumScale2)
  })
  model.reg<-lm(Inflation_Rate~SumScale1+SumScale2)
  output$Print6 <-renderPrint({
    summary(lm(Inflation_Rate~SumScale1+SumScale2))
  })
   
})