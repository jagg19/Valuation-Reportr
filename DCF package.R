#Load packages

if("quantmod" %in% rownames(installed.packages()) == FALSE){
  install.packages("quantmod")
}  else{ require(quantmod)}


if("PerformanceAnalytics" %in% rownames(installed.packages()) == FALSE){
  install.packages("PerformanceAnalytics")
}  else{ require(PerformanceAnalytics)}


if("zoo" %in% rownames(installed.packages()) == FALSE){
  install.packages("zoo")
}  else{ require(zoo)}


if("finreportr" %in% rownames(installed.packages()) == FALSE){
  install.packages("finreportr")
}  else{ require(finreportr)}


if("dplyr" %in% rownames(installed.packages()) == FALSE){
  install.packages("dplyr")
}  else{ require(dplyr)}




#=====================================================================================================================================================================



#Custom function library

present.value <- function(fv, r, n){
                  pv <- fv / (1+r)^n
                  return(pv)}

#=====================================================================================================================================================================


future.value <- function(pv, r, n){
                fv <- pv * (1+r)^n
                return(fv)}

#=====================================================================================================================================================================

fcfe.value <- function(ni, dep, capex, wkcp){
              fcfe <- ni + dep - capex - wkcp
              return(fcfe)}

#=====================================================================================================================================================================

terminal.value <- function(fcfe, disc, g){
                  tv <- fcfe * (1+g) / (disc - g)
                  return(tv)}

#=====================================================================================================================================================================


equity.value <- function(fcfe, disc, g, n = 5, tvg = .02){ 
              #default forecast period of 5 years
              #create dataframe & calculate fv of FCF and pv of the 
              #forecasted 5 years fv fcf and sum it all up
              cf <- rep(fcfe, n)
              cf <- data.frame(cf)
              cf$period <- seq(1,n,1)
              cf$fv_factor <- (1 + g)^cf$period
              cf$fv <- cf$cf * cf$fv_factor
              cf$pv_factor <- 1 / (1 + disc)^cf$period  #growth or discount rate here?
              cf$pv <- cf$fv * cf$pv_factor
              pv_cf <- sum(cf$pv)
              
              #terminal period - default value at 2%
              #calculate Terminal value and discount it to PV
              tv <- cf$fv[n] * (1+tvg) / (disc - tvg)
              pv_tv <- tv / (1+disc)^n
              
              #add PV of forecasted values and PV of terminal value to
              #get PV equity value
              equity_value <- pv_cf + pv_tv
              return(equity_value)
}



#=====================================================================================================================================================================





#Function to calculate book value
bk.val <- function(symbol, year) {
  
  
  ticker <- symbol
  
  symbol_is <- GetIncome(ticker, year)
  symbol_bs <- GetBalanceSheet(ticker, year)
  
  bvps <- as.numeric(filter(symbol_bs, symbol_bs$Metric == "Stockholders' Equity Attributable to Parent")[-1,]$Amount) / 
    
    as.numeric(filter(symbol_is, symbol_is$Metric == "Weighted Average Number of Shares Outstanding, Basic")$Amount)
  
  bvps <- as.data.frame(bvps)
  
  rownames(bvps) <- filter(symbol_is, symbol_is$Metric == "Weighted Average Number of Shares Outstanding, Basic")$endDate
  
  return(bvps)
}




#=====================================================================================================================================================================



#Function to calculate price to book ratio
price.book <- function(symbol, year) {
  
  ticker <- symbol
  
  symbol_is <- GetIncome(ticker, year)
  symbol_bs <- GetBalanceSheet(ticker, year)
  
  bvps <- as.numeric(filter(symbol_bs, symbol_bs$Metric == "Stockholders' Equity Attributable to Parent")[-1,]$Amount) / 
    
    as.numeric(filter(symbol_is, symbol_is$Metric == "Weighted Average Number of Shares Outstanding, Basic")$Amount)
  
  bvps <- as.data.frame(bvps)
  
  rownames(bvps) <- filter(symbol_is, symbol_is$Metric == "Weighted Average Number of Shares Outstanding, Basic")$endDate
  
  #Price data to finish calculation
  symbol_prices <- getSymbols(ticker, from = as.Date(rownames(bvps)[1])-1, to = as.Date(rownames(bvps)[3])+1, auto.assign = FALSE)
  a <- c("2015-09-25", "2016-09-23", "2017-09-29")
  price_book <- as.data.frame(Cl(symbol_prices[a])) / bvps
  rownames(price_book) <- rownames(bvps)
  return(price_book)
}



#=====================================================================================================================================================================



beta <- function(symbol){
  
  
  d <- as.POSIXlt(Sys.Date())
  d$year <- d$year-5
  from <- as.Date(d)
  to <- Sys.Date()
  
  #Get price data for SPY then your symbol
  spy.price <- getSymbols("SPY", from = from, to = to, auto.assign = FALSE)
  spy.price <- to.monthly(spy.price[,4], OHLC=FALSE)
  rets <- Return.calculate(spy.price)
  
  #Your stock and combine with spy
  symbol.price <- getSymbols(symbol, from = from, to = to, auto.assign = FALSE)
  symbol.price <- to.monthly(symbol.price[,4], OHLC=FALSE)
  rets$symbol <- Return.calculate(symbol.price)
  
  #Rename first variable
  names(rets)[1] <- "spy_ret"
  
  #Remove NA from first obv
  rets <- rets[-1,]
  rets
  #run regression to find the beta
  reg <- lm(symbol ~ spy_ret, data = rets)
  
  beta <- summary(reg)$coeff[2]
  print(beta)
}





#=====================================================================================================================================================================




capm <- function(symbol, rm = .10, rf = .03){
  
  #Calculate equity risk premium
  erp <- rm - rf
  
  #Set dates
  d <- as.POSIXlt(Sys.Date())
  d$year <- d$year-5
  from <- as.Date(d)
  to <- Sys.Date()
  
  #Get price data for SPY then your symbol
  spy.price <- getSymbols("SPY", from = from, to = to, auto.assign = FALSE)
  spy.price <- to.monthly(spy.price[,4], OHLC=FALSE)
  rets <- Return.calculate(spy.price)
  
  #Your stock and combine with spy
  symbol.price <- getSymbols(symbol, from = from, to = to, auto.assign = FALSE)
  symbol.price <- to.monthly(symbol.price[,4], OHLC=FALSE)
  rets$symbol <- Return.calculate(symbol.price)
  
  #Rename first variable
  names(rets)[1] <- "spy_ret"
  
  #Remove NA from first obv
  rets <- rets[-1,]
  
  #Run regression to find the beta
  reg <- lm(symbol ~ spy_ret, data = rets)
  
  beta <- summary(reg)$coeff[2]
  
  ca.pm <- rf + (beta * erp)
  return(ca.pm)
}





#=====================================================================================================================================================================
#=====================================================================================================================================================================
#=====================================================================================================================================================================
#=====================================================================================================================================================================
#=====================================================================================================================================================================



#Ticker information
ticker <- "AAPL"
ticker_name <- CompanyInfo(ticker)
#To record how long it takes to webscrape and retrieve financial statement data from SEC
t1 <- Sys.time()

#USE IF STATEMENT WHEN UPDATING TICKER OR WHEN RERUNNING AFTER FIRST INITIAL RUN;
#FOR FIRST RUN, USE FUNCTIONS INSIDE IF STATEMENT (WILL FIX THIS LATER)

#symbol_info <- CompanyInfo(ticker)
#symbol_is <- GetIncome(ticker, 2017)
#symbol_bs <- GetBalanceSheet(ticker, 2017)
#symbol_cf <- GetCashFlow(ticker, 2017)

if (ticker_name[1] %in% symbol_info[1] == FALSE){
  symbol_info <- ticker_name
  symbol_is <- GetIncome(ticker, 2017)
  symbol_bs <- GetBalanceSheet(ticker, 2017)
  symbol_cf <- GetCashFlow(ticker, 2017)
}
t2 <- Sys.time()




#Filter out the quarterly data to get a cleaner look
symbol_is_an <- filter(symbol_is, as.numeric(as.Date(symbol_is$endDate) - as.Date(symbol_is$startDate)) > 330)


#Check unique labels for the lines of each statement
unique(symbol_is_an$Metric)



#Calculate Book Value per share
symbol_bvps <- as.numeric(filter(symbol_bs, symbol_bs$Metric == "Stockholders' Equity Attributable to Parent")[-1,]$Amount) / as.numeric(filter(symbol_is, symbol_is$Metric == "Weighted Average Number of Shares Outstanding, Basic")$Amount)

symbol_bvps <- as.data.frame(symbol_bvps)

rownames(symbol_bvps) <- filter(symbol_is, symbol_is$Metric == "Weighted Average Number of Shares Outstanding, Basic")$endDate

print(symbol_bvps)


 
 
#Get price data
from <- as.Date(rownames(symbol_bvps)[1])-1
to <- as.Date(rownames(symbol_bvps)[3])+1
symbol_prices <- getSymbols(ticker, from = from, to = to, auto.assign = FALSE)


#Calculate Price to Book ratio
a <- c("2015-09-25", "2016-09-23", "2017-09-29")
price_book <- as.data.frame(Cl(symbol_prices[a])) / symbol_bvps$symbol_bvps
rownames(price_book) <- rownames(symbol_bvps)
price_book


#Calculate price to earnings
unique(symbol_is$Metric)
symbol_eps <- filter(symbol_is, symbol_is$Metric == "Earnings Per Share, Diluted" , as.numeric(as.Date(symbol_is$endDate) - as.Date(symbol_is$startDate)) > 330 )


b <- as.Date(symbol_eps$endDate) -1

pe_ltm <- as.data.frame(Cl(symbol_prices[b])) / as.numeric(symbol_eps$Amount)



#Next twelve months, forward PE
#If we expect 2017 earnings to grow 20%, then we calculate forward PE by taking todays price / future eps
g = .20
eps_t1 <- last(as.numeric(symbol_eps$Amount)) * (1+g)
pe_ntm <- last(Cl(symbol_prices[b])) / eps_t1
pe_ntm




#Calculate return on equity
symbol_roe <- pe_ltm / symbol_bvps

symbol_roe



#Setting & plot historical + projected revenues###

unique(symbol_is$Metric)
rev_hist <- filter(symbol_is, symbol_is$Metric == "Revenue, Net", as.numeric(as.Date(symbol_is$endDate) - as.Date(symbol_is$startDate)) > 330)

rev_hist


#Projected growth, 15% YoY
rev_g <- 0.15
n <- 1:5


# Copy Historical revenue data frame and add extra rows to forecast next 5 years for projected revenue. Combine dataframes together 
rev_proj <- rev_hist[,c(-3:-5)]
extra_rows <- rev_proj[-1,]
rownames(extra_rows) <- c("4","5")
rev_proj <- rbind(rev_proj,extra_rows)
rev_proj$Amount <- as.numeric(last(rev_hist$Amount)) * (1+rev_g)^n
rev_proj$startDate <- c("2017-10-01","2018-09-28","2019-09-30", "2020-09-29", "2021-10-01")
rev_proj$endDate <- c("2018-09-27","2019-09-29","2020-09-28","2021-09-30", "2022-09-27")


rev_shell_hist <- rbind(rev_hist, rev_proj)
rev_shell_hist[4:8,3] <- rep(0,5)

rev_shell_proj <- rbind(rev_hist, rev_proj)
rev_shell_proj[1:3,3] <- rep(0,3)



#Combine two revenue data frames together and create a matrix with result
symbol_rev <- rbind(rev_shell_hist,rev_shell_proj)

symbol_rev <- matrix(symbol_rev$Amount, nrow = 2, ncol = 8, byrow = TRUE)


rownames(symbol_rev) <- c("Hist Revenue", "Proj Revenue")
colnames(symbol_rev) <- c(2015:2022)
symbol_rev



#Plot your revenues! See if you find any unusaul trends in your projected values
barplot(symbol_rev,
        col = c("red", "blue"),
        main = "Historical vs. Projected Revenues")

legend("topleft",
       legend = c("Historical", "Projected"),
       fill = c("red", "blue"))






#Calculate free cash flow
unique(symbol_cf$Metric)

symbol_cf_an <- filter(symbol_cf, as.numeric(as.Date(symbol_cf$endDate) - as.Date(symbol_cf$startDate)) > 330)
symbol_cf_an_recent <- filter(symbol_cf_an, symbol_cf_an$endDate == symbol_cf_an$endDate[3])

symbol_cf_an_recent

op_cshflw <- as.numeric(symbol_cf_an_recent[13,3])

capex <- as.numeric(symbol_cf_an_recent[18,3])
  
fcf_current <- op_cshflw - capex




#Forecast and calculate projected FCF
n<-1:5
g <- .05
fcf_future <- fcf_current * (1+g)^n
fcf_future <- as.data.frame(fcf_future)
rownames(fcf_future) <- 2018:2022
colnames(fcf_future) <- "fcf"
fcf_future
fcf_current
fcf <- fcf_future


# Calculate CAPM Cost of Equity using custom function
ke <- capm(ticker, rm = .10, rf = .03)


# Calculate Discount Periods, or the amount of years being forecasted that needs to be discounted
fcf$disc_periods <- seq(1,nrow(fcf),1)


# Calculate discount factor
fcf$disc_factor <- 1/ (1+ke)^fcf$disc_periods


# Calculate PV of each period's total free cash flow
fcf$pv <- fcf$fcf * fcf$disc_factor
fcf


# Calculate sum of Projected fcf
pv_proj_period <- sum(fcf$pv)
pv_proj_period




#Calculate terminal value using custom function.
tv_2022 <- terminal.value(last(fcf$fcf), ke, .03)



# Calculate PV of Terminal Value
pv_terminal <- tv_2022 / (1 + ke)^5
pv_terminal



#Sum with PV of projected fcf and divide by shares outstanding to get price per share
equity_value_fcf <- pv_proj_period + pv_terminal

#Get shares outstanding
sh_out <- as.numeric(last(filter(symbol_is, symbol_is$Metric == "Weighted Average Number of Shares Outstanding, Basic")$Amount))

#Price per share
equity_pps <- equity_value_fcf / sh_out


equity_pps

#to record how long it takes to webscrape and retrieve financial statement data from SEC
print(t2-t1)