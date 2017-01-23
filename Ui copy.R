library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Deflationary effect of Bitcoin on US economy "),
 
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    span("This project was done by"), 
    a("Mehrnoosh Oghbaie", href="https://github.com/moghbaie/bitcoin-deflation", target="_blank") , 
    span("& Hongjing Zhang as part of the"), 
    a("applied statistics with application in finance", href="https://github.com/moghbaie/bitcoin-deflation", target="_blank"),span("course at Stevens Institute of Technology.")
    ),
 
  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    br(),
    br(),
    p("The goal of this project was to investigate the deflationary effect of Bitcoin on US economy. Some economists, such as Paul Krugman believed in the possibility of deflationary pressure in crypto-currency networks. Bitcoin’s capped total money supply could be viewed as a variation on Milton Friedmans “k-percent rule”. This theory states that an optimal way to control inflation over the long term is for the central bank to grow the money supply by a fixed amount of k% each year, irrespective of the cyclical state of the economy. In particular, one should set the growth variable of k% at a rate equal to the growth of real GDP each year. This connection between Milton Freidman’s Nobel prize winning theory and Bitcoin practice was highlighted recently in a paper by B¨ohme who argues that one can consider Bitcoin as a type of “... proposal to fix the annual growth rate of the money supply to a fixed rate of growth.” B¨ohme reiterates the views of Paul Krugman that “... the fixed slow growth rate of Bitcoin creates the possibility of deflation if Bitcoin was to be used widely...”. He also notes that there have been other crypto-currency extensions of Bitcoin proposed to overcome such potential problems, see discussion by for instance King which introduces Primecoin with infinite money supply or the introduction of Peercoin which keeps k% around 1-2."),
    h2("Bitcoin price vs. US dollar index"),
    p("US dollar index is a good indicator of how strong US economy is. We studied the relationship between Bitcoin price and US dollar index.
While the US dollar is a stable currency, the price of the bitcoin compare to the US dollar reveals how investors value the virtual currency and how the speculation formed during time.
      The DXY is a convenient index to measure the strength and weakness of the US dollar, JP Morgan created the DXY in 1973, and it has only been updated once, for the introduction of the Euro currency. We explored some of the features of the DXY and the BIT/USD such as the histogram on raw data and histogram on log (data). We put both measures in the same graph, and build a linear regression model. Then we did a simple time series analysis on the data."),
    h4("Data:"),
    div("Daily close price of BIT/USD from 2013-05-31 to 2016-11-17"),
    div("Daily return of close price of BIT/USD from 2013-05-31 to 2016-11-17"),
    div("Daily close price of the US dollar index from the same time span (DXY)"),
    div("Daily return of close price of US dollar index from the same time span"),
    plotOutput("Plot9"),
    p("The negative relationship between Bitcoin price and Dollar index is more obvious after we draw them together."),
    br(),
    plotOutput("Plot3"),
    p("The histogram of the daily close price of BIT/USD appears to be slightly right-skewed. The log transformation distribution is even more complicated (with no single peak and left skewed)."),
    p("The histogram of the daily DXY appears to be bivariate normal, and stays the same after log transformation."),
    br(),
    plotOutput("Plot7"),
    p("Histogram of Bitcoin return appears to be more positive than negative (the bar that is positive is slightly higher than the one that’s negative), this matches the fact that despite a short period of price falling, Bitcoin value is increasing with regards to USD, which is a rather stable currency."),
    p("Histogram of US dollar index appears to have more negative returns than positive returns, indicating that the US dollar is possibly going weaker and weaker over time."),
    plotOutput("Plot10"),
    p("The return of the bitcoin is much more volatile than the US dollar index which coincides with the reality that bitcoin still remains as a very speculative asset in the market. 
Also we can see that the returns appear to have no pattern, which proves the complete market hypothesis: it is impossible to predict the market and make profit of it. Of course there will be no financial market if there appears zero arbitrage opportunities, but speaking from the broad aspect, the market is kind of complete.
      "),
    verbatimTextOutput("Print1"),
    h4("Result:"),
    HTML("<ol class='circle-list'><li><p>The p-values of the intercept and the slope are both significant at a 95% confidence level, which indicates that there appears to be a relationship between these two currencies.</p>
      </li>
      <li>
      <p>The coefficient of the slope is negative, so we make the statement that when the DXY goes down, the BIT/USD will go up.</p>
      </li>
      <li>
      <p>The R-square is only 0.01564, which means that only 1.5% percent of the change in BIT/USD can be explained by the change in DXY. This is strong evidence against the statement we made in 1 and 2. </p>
      </li>
      </ol> "),
    plotOutput("Plot11"),
    p("The residual against the X-axis plot suggests that the residuals are not randomly distributed, which violates the assumption of a randomly distributed residuals."),
    plotOutput("Plot12"),
    p("residuals plot shows that there is a volatility cluttering at certain time periods, responding to certain period of economy where there are some major events happening."),
    plotOutput("Plot16"),
    p("We did time series analysis, which differs from the linear regression in that the coefficient and the intercept do not stay constant: they evolve over time."),
    verbatimTextOutput("Print2"),
    plotOutput("Plot17"),
    p("From the histogram of the estimated coefficients we can see that the negative ones outnumber the positive ones, hence we are certai that the DXY and BIT/USD do have a negative relationship with each other."),
    br(),
    h2("US inflation rate vs. Bitcoin return and DXY return"),
    br(),
    h4("Data:"),
    div("Monthly return on close price of BIT/USD from 2013-05-01 to 2016-11-01"),
    div("Monthly return on DXY from 2013-05-01 to 2016-11-01"),
    div("Monthly return on US inflation rate from 2013-05-01 to 2016-11-01"),
    br(),
    p("To investigate the effect of Bitcoin on inflation rate, we transform the Bitcoin return and DXY return to monthly data, and tried to build a multiple linear regression model. Our model was supposed to show the relationship between inflation rate and Bitcoin return and DXY return."),
    plotOutput("Plot28"),
    plotOutput("Plot26"),
    p("Monthly inflation rate and monthly dxy return follow normal distribution. Bitcoin return is normally distributed but has outlier."),
    verbatimTextOutput("Print3"),
    p("The multiple regression model shows that there is no significant relationship between inflation rate and Bitcoin or DXY return, the R-squared is extremely low and linear model is far from ideal. Residuals don’t seem to be normally distributed"),
    plotOutput("Plot29"),
    plotOutput("Plot33"),
    h2("Relationship between Bitcoin measures and economy indicators:"),
    p("We decided to use more data related to bitcoin Volume and its transaction to US dollar and economic indicators including inflation rate to study their correlation and possible relationship."),
    br(),
    h4("Data:"),
    p("We downloaded Bitcoin data from Quandle and Economy related data from Fred:"),
    br(),
    div("Monthly total bitcoin BCHAIN/TOTBC from 2013-05-31 to 2016-11-17"),
    div("Monthly Bitcoin USD Exchange Trade Volume BCHAIN/ETRAV from 2013-05-31 to 2016-11-17"),
    div("Monthly Bitcoin Market Capitalization BCHAIN/MKTCP from 2013-05-31 to 2016-11-17"),
    div("Monthly Bitcoin Trade Volume vs. Transaction Volume Ratio BCHAIN/TVTVR from 2013-05-31 to 2016-11-17"),
    div("Monthly US inflation Rate from 2013-05-31 to 2016-11-17"),
    div("Monthly Producer Price Index from 2013-05-31 to 2016-11-17"),
    div("Monthly Customer Price Index from 2013-05-31 to 2016-11-17"),
    div("Monthly Global price Brent Crude from 2013-05-31 to 2016-11-17"),
    div("Monthly Export from 2013-05-31 to 2016-11-17"),
    div("Monthly Import from 2013-05-31 to 2016-11-17"),
    div("Monthly Personal Consumption Expenditure from 2013-05-31 to 2016-11-17"),
    br(),
    h4("Analysis:"),
    p("As we made a correlation plot between all the factors, we found strong negative correlation between Bitcoin estimated transaction volume in USD and inflation rate. "),
    br(),
    plotOutput("Plot34"),
    br(),
    p("Building a comparison scaled plot helped us to follow the changes and correlation as shown below. There is positive correlation between global price Brent crude and inflation rate and also negative correlation between Bitcoin estimated transaction volume in USD and inflation rate. Other Bitcoin measures seem to be negatively correlated too."),
    plotOutput("Plot36"),
    plotOutput("Plot37"),
    p("Bitcoin estimated transaction volume in USD doesn’t have an ideal normal distribution. Linear regression model shows obvious significant relationship between inflation rate and Bitcoin estimated transaction volume in USD. P-values are small and R-squared is around 0.45."),
    plotOutput("Plot38"),
    verbatimTextOutput("Print4"),
    p("Residuals seem to be normally distributed. Residuals/ fitted values graph show concentration in around low values that could be caused by instability of Bitcoin in early days."),
    plotOutput("Plot39"),
    p("Building the gam model we got slightly better R-square and significant p-value as shown below. Residuals seem to be normally distributed."),
    plotOutput("Plot40"),
    verbatimTextOutput("Print5"),
    p("We also did a factor analysis to find out how our indicators are related to inflation rate. Factor analysis suggested us using two scales."),
    plotOutput("Plot41"),
    plotOutput("Plot42"),
    plotOutput("Plot43"),
    p("Both scales don’t seem to have normal distribution. After building our scales, we build a linear regression model to see how different scales contributing to inflation rate. Sumscale2 doesn’t to be very significant."),
    verbatimTextOutput("Print6"),
    h2("Conclusion:"),
    p("Bitcoin still remains very volatile in the financial market because of its virtual nature. Compared with it the US dollar index has a much lower return rate in general.Despite its economical dependence, the BIT/USD and DXY does not appear to have a statistically significant relationship among them, if there exists a relationship, it is a rather weak one, and they are negatively dependent on each other, meaning if one goes up, the other one goes down. Also in the end the time series analysis method is brought up, suggesting a possible solution for depicting the relationship between the virtual currency bitcoin and so far the most stable currency in the world – the US dollar. There is significant negative relationship between Bitcoin estimated transaction volume in USD and US inflation rate, which proves our hypothesis, was right. We also found that some of our indicators are more related to inflation rate than others, which could be good base for future research.")
    )
))