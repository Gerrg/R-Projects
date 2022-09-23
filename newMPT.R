set.seed(123)
suppressPackageStartupMessages({
  # General purpose
  library(tidyverse) # for mutate(), select(), filter()
  library(tidyquant) # for ROC(), endpoints()
  library(data.table) # for fread()
  library(lubridate) # for days()
  library(magrittr) # for is_less_than()
  library(janitor) # for clean_names()
  library(kableExtra) # for kable()
  
  # Models
  library(tidymodels)
  library(feasts) # for STL()
  library(lmtest) # for bptest() for heteroskedasticity
  library(forecast) # for forecast()
  
  # Time series
  library(tsibble) # for as_tsibble()
  library(fable)  # for ARIMA(), ETS(), MEAN(), NAIVE(), SNAIVE() TSLM()
  library(timetk) # for tk_ts(), tk_tbl()
  
  # Plotting
  library(ggthemes) # for theme_hc()
  library(formattable) # for formattable()
  
  # Data processing
  library(sweep) # for sw_tidy(), sw_glance(), sw_augment()

  library(plotly)
  library(fPortfolio)
  library(PerformanceAnalytics)
  library(Metrics)
  library(quantmod)
  library(fBasics)
  library(rcompanion)
  library(tseries)
  library(forecast)
  library(rugarch)
  library(rmgarch)
  library(rsq)
  library(skimr)  
})

# Get data
tickers = c("FB","CNK","TTWO","PENN", "WYNN")

prices_data_daily <- tq_get(tickers,
                  # src = 'yahoo',
                  from = "2016-01-01",
                  to = "2019-12-31",
                  get = "stock.prices")

prices_data_monthly <- tq_get(tickers,
                            # src = 'yahoo',
                            from = "2016-01-01",
                            to = "2019-12-31",
                            get = "stock.prices",
                            periodicity = "monthly")

#Plot the Prices of the stocks
prices_data_monthly %>%
  ggplot(aes(x = date, y = adjusted, colour = symbol)) +
  geom_line() +
  facet_wrap(~symbol,scales = 'free_y') +
  scale_x_date(breaks = "1 year", date_labels = "%b\n%y") +
  theme_classic() +
  scale_colour_brewer(palette = "Dark2") +
  labs(
    title ="Plot of the Stocks Daily Prices",
    x = "Dates",
    y = "Adjusted Prices",
    colour = "Ticker Symbol")

#A Box Plot of the Prices across the multiple stocks
prices_data_monthly %>%
  ggplot(aes(y = adjusted, x = reorder(symbol, adjusted, FUN = median), fill = symbol)) +
  geom_boxplot() +
  stat_summary(fun.y = "mean", colour = "red") +
  scale_y_log10() +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 400, by = 50)) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title ="Plot of the Prices across multiple stocks",
    x = "Ticker Symbol",
    y = "Prices",
    colour = "Ticker Symbol")

# Calculating the logarithmic monthly returns for these stocks
log_ret_tidy_monthly <- prices_data_monthly %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
             mutate_fun = periodReturn,
             col_rename = 'ret',
             type = 'log') %>% 
  subset(ret !=0)
log_ret_tidy_monthly

#Scatter Plot of Log tranformed Returns across the stocks  
log_ret_tidy_monthly %>%
  ggplot(aes(x = date, y = ret, colour = symbol)) +
  geom_jitter() +
  facet_wrap(~symbol,scales = 'free_y') +
  scale_x_date(breaks = "1 year", date_labels = "%b\n%y") +
  theme_classic() +
  geom_hline(aes(yintercept = 0), 
             colour = "red",
             size = 1,
             linetype = "dashed") +
  scale_colour_brewer(palette = "Dark2") +
  labs(
    title ="Plot of the Stocks Monthly Returns",
    x = "Dates",
    y = "Returns",
    colour = "Ticker Symbol")

# Converting into wide format and time series object
log_ret_xts_monthly <- log_ret_tidy_monthly %>%
  spread(symbol, value = ret) %>%
  tk_xts()

# Calculating the mean monthly returns 
mean_ret <- colMeans(log_ret_xts_monthly)
print(round(mean_ret, 5))

#Plotting the Expected return to risk of the portfolio
s0<-log_ret_tidy_monthly %>% 
  group_by(symbol) %>% 
  summarise(Return0 = mean(ret),
            Risk0 = sd(ret)) %>% 
  ggplot(aes(y = Return0, x = Risk0)) +
  geom_point(aes(colour = symbol),size = 6) +
  theme_classic() +
  scale_colour_brewer(palette = "Dark2") +
  scale_y_continuous(labels = scales::percent) + #, limits = c(-0.01, 0.03)
  scale_x_continuous(labels = scales::percent) + #, limits = c(0, 0.25)
  labs(x = 'Volatility',
       y = 'Expected Returns',
       title = "Risk-Return Tradeoff Monthly",
       colour = "Ticker")
s0
plotly::ggplotly(s0)

# calculate the covariance matrix for all these stocks
# We will annualize it by multiplying by 12.
cov_mat <- cov(log_ret_xts_monthly) * 12
print(round(cov_mat,4))

# Calculate the random weights
wts <- runif(n = length(tickers))
wts <- wts/sum(wts)
wts
# Calculate the portfolio returns
port_returns <- (sum(wts * mean_ret) + 1)^12 - 1

# Calculate the portfolio risk
port_risk <- sqrt(t(wts) %*% (cov_mat %*% wts))

# Calculate the Sharpe Ratio
sharpe_ratio <- port_returns/port_risk

print(wts)
print(port_returns)
print(port_risk)
print(sharpe_ratio)

num_port <- 1000

# Creating a matrix to store the weights
all_wts <- matrix(nrow = num_port,
                  ncol = length(tickers))

# Creating an empty vector to store
# Portfolio returns
port_returns <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Standard deviation
port_risk <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Sharpe Ratio
sharpe_ratio <- vector('numeric', length = num_port)

for (i in seq_along(port_returns)) {
  
  wts <- runif(length(tickers))
  wts <- wts/sum(wts)
  
  # Storing weight in the matrix
  all_wts[i,] <- wts
  
  # Portfolio returns
    port_ret <- sum(wts * mean_ret)
  port_ret <- ((port_ret + 1)^12) - 1
  
  # Storing Portfolio Returns values
  port_returns[i] <- port_ret
  
  # Creating and storing portfolio risk
  port_sd <- sqrt(t(wts) %*% (cov_mat  %*% wts))
  port_risk[i] <- port_sd
  
  # Creating and storing Portfolio Sharpe Ratios
  # Assuming 0% Risk free rate
    sr <- port_ret/port_sd
  sharpe_ratio[i] <- sr
  
}

# Storing the values in the table
portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
                           SharpeRatio = sharpe_ratio)
# Converting matrix to a tibble and changing column names
all_wts <- tk_tbl(all_wts)

colnames(all_wts) <- colnames(log_ret_xts_monthly)

# Combing all the values together
portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))
portfolio_values

min_var <- portfolio_values[which.min(portfolio_values$Risk),]
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]

# plot the weights of each portfolio.
p <- min_var %>%
  gather( CNK:WYNN, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Minimum Variance Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label=sprintf("%.02f %%",min_var[,1:5]*100)),
            position=position_dodge(width=0.9), 
            vjust=-0.25, check_overlap = TRUE)

plotly::ggplotly(p)

# Plot of Tangency PortFolio
p1 <- max_sr %>%
  gather(CNK:WYNN, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Tangency Portfolio Weights") +
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label=sprintf("%.02f %%",max_sr[,1:5]*100)),
            position=position_dodge(width=0.9), 
            vjust=-0.25, check_overlap = TRUE)
p1
plotly::ggplotly(p1)

# plot all the random portfolios and visualize the efficient frontier.
p2 <- portfolio_values %>%
  ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
  geom_jitter(alpha = .5) +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Annualized Risk',
       y = 'Annualized Returns',
       title = "Portfolio Optimization & Efficient Frontier") +
  geom_jitter(aes(x = Risk,
                 y = Return), data = min_var, color = 'red') +
  geom_jitter(aes(x = Risk,
                 y = Return), data = max_sr, color = 'forest green') +
  annotate('text', x = 0.30, y = 0.42, label = "Tangency Portfolio") +
  annotate('text', x = 0.38, y = 0.018, label = "Minimum variance portfolio") +
  annotate(geom = 'segment', x = 0.17, xend = 0.135,  y = 0.17, 
           yend = 0.06, color = 'red', arrow = arrow(type = "open")) +
  annotate(geom = 'segment', x = 0.23, xend = 0.3,  y = 0.405, 
           yend = 0.365, color = 'forest green', arrow = arrow(type = "open"))

p2
plotly::ggplotly(p2)

##Determine the efficient frontier and plot the same
effFrontier <- portfolioFrontier(as.timeSeries(log_ret_xts_monthly),
                                 constraints = "LongOnly")

frontierPlot(effFrontier)
grid()

plot(effFrontier, c(1,2,3,4))
grid()

###############################################################################
##Plot the weights for all the portfolio in the efficient frontier
weightsPlot(effFrontier)
frontierWeights <- getWeights(effFrontier)

##Obtain the weights for each stock for the portfolio with the least variance
mvp <- minvariancePortfolio(as.timeSeries(log_ret_xts_monthly),
                            spec=portfolioSpec(),
                            constraints="LongOnly")
mvp
tanPort <- tangencyPortfolio(as.timeSeries(log_ret_xts_monthly),
                             spec=portfolioSpec(),
                             constraints="LongOnly")
tanPort
minvarweights <- getWeights(mvp) 
tanportweights <- getWeights(tanPort)
mvpret <- ((getTargetReturn(mvp)[1])+1)^12 - 1
tanret <- ((getTargetReturn(tanPort)[1])+1)^12 - 1
mvprisk <- getTargetRisk(mvp)[4] * sqrt(12)
tanrisk <- getTargetRisk(tanPort)[4] * sqrt(12)
return <- cbind(tanret, mvpret)
risk <- cbind(tanrisk, mvprisk)
parameters <- rbind(return,risk)
colnames(parameters) <- c("Tangency Portfolio","Min Var Portfolio")
parameters
wt <- (cbind(minvarweights, tanportweights))
colnames(wt) <- c("Minimum Variance Portfolio", "Tangency Portfolio")

##Plot the weights of the minimum variance portfolio
weights <- data.frame(minvarweights)
assets <- colnames(frontierWeights)
ggplot(data=weights, aes(x=fct_reorder(assets,minvarweights) ,
                         y=minvarweights,
                         fill=assets)) +
  geom_bar(stat="identity",
           position=position_dodge(),
           colour="black") +
  geom_text(aes(label=sprintf("%.02f %%",minvarweights*100)),
            position=position_dodge(width=0.9), 
            vjust=-0.25, check_overlap = TRUE) +
  ggtitle("Minimum Variance Portfolio Optimal Weights")+ 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Assets", y = "Weight (%)") +
  theme_classic() +
  scale_y_continuous(labels = scales::percent)

##Plot the weights of the tangency portfolio
tanwt <- as_tibble(tanportweights)
assets <- colnames(frontierWeights)
ggplot(data=tanwt, aes(x=fct_reorder(assets,tanportweights), y=tanportweights, fill=assets)) +
  geom_bar(stat="identity", position=position_dodge(),colour="black") +
  geom_text(aes(label=sprintf("%.02f %%",tanportweights*100)),
            position=position_dodge(width=0.9), vjust=-0.25, check_overlap = TRUE) +
  ggtitle("Tangency Portfolio Optimal Weights") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Assets", y = "Weight (%)") +
  scale_fill_brewer(palette = "Dark2")

##Tabulate the risk and return for each point on the efficient frontier

riskReturnPoints <- frontierPoints(effFrontier) # get risk and return values for points on the efficient frontier
annualizedPoints <- data.frame(targetRisk=riskReturnPoints[, "targetRisk"] * sqrt(12),
                               targetReturn=riskReturnPoints[,"targetReturn"] * 12)

annualizedPoints <- cbind(frontierWeights, annualizedPoints)

plotReturns<-
log_ret_tidy_monthly %>% 
  group_by(symbol) %>% 
  summarise(Return0 = mean(ret),
            Risk0 = sd(ret)) %>% 
  ggplot(aes(y = Return0,
             x = Risk0)) +
  geom_point(aes(colour = symbol),
             size = 6) +
  theme_classic() +
  scale_colour_brewer(palette = "Dark2") +
  scale_y_continuous(labels = scales::percent,
                     limits = c(-0.01, 0.03)) +
  scale_x_continuous(labels = scales::percent) + #, limits = c(0, 0.25)
  labs(x = 'Volatility',
       y = 'Expected Returns',
       title = "Risk-Return Tradeoff Monthly",
       colour = "Ticker")

plotEfecient <-  
annualizedPoints %>% 
    pivot_longer(cols = c("CNK","FB","PENN","TTWO","WYNN"),
                 names_to = "symbol",
                 values_to = "weights") %>% 
    ggplot(aes(targetRisk, targetReturn))+
    geom_point() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) + 
  theme_bw()

  
returntoRisk <- log_ret_tidy_monthly %>%
  group_by(symbol) %>%
  summarise(Return0 = mean(ret),
            Risk0 = sd(ret))
  
plotEfecient 
##############################################################################

#Getting Portfolio Returns 
# port_returns
portfolio_values

 #portfolio returns as time series  
port_returns_ts <-ts(port_returns,frequency = 12, start = 1, end = 48)

# Naive
port_naive<- naive(port_returns_ts,3)
summary(port_naive)
autoplot(port_naive)

# Moving Average 5
port_ma5<-SMA(port_returns_ts,5)
summary(na.omit(port_ma5))
autoplot(na.omit(port_ma5))

# Moving Average 
port_ma15<-SMA(port_returns_ts,15)
summary(port_ma15)
autoplot(port_ma15)

# simple exponential smoothing 
port_ses<-ses(port_returns_ts,3)
summary(port_ses)
autoplot(port_ses) + 
  autolayer(fitted(port_ses),series ="Fitted")
acf(port_ses$residuals,na.action=na.pass) %>% autoplot()
Box.test(port_ses$residuals, type="Ljung-Box")

comp_port_returns_ts <- decompose(port_returns_ts)
autoplot(comp_port_returns_ts)

# holts winter method 
port_holt<-holt(port_returns_ts,3,level=c(80,95))
summary(port_holt)
autoplot(port_holt)+
  autolayer(fitted(port_holt),series ="Fitted")

acf(port_holt$residuals,na.action=na.pass) %>% autoplot()
Box.test(port_holt$residuals, type="Ljung-Box")

# holt's seasonal Treand Method
hw1 <-hw(port_returns_ts, seasonal = "additive")
hw2 <-hw(port_returns_ts, seasonal = "multiplicative")
autoplot(port_returns_ts) +
  autolayer(hw1, series = "HW additive forecasts", PI = F)+
  autolayer(hw2, series = "HW Multiplicative forecasts", PI = F)

# AR(1) model on portfolio returns
ARIMA1<-arima(port_returns_ts,order = c(1,0,0))
summary(ARIMA1)

# model specs for a GARCH constant model
model_specific = ugarchspec()

# model fitting 
mod_fitting<-ugarchfit(data = port_returns_ts,
                       spec = model_specific ,out.sample = 20 )

#Stationary testing 
library(urca)
stationary_Test<-ur.df(prices_data_monthly$adjusted, selectlags = "AIC")
#make first differences on the series 
D.prices_data <- diff(prices_data_monthly$adjusted, differences =1)
plot(D.prices_data, col="dark green", type = "l", ylab = "D.prices")
summary(ur.df(D.prices_data, selectlags = "AIC"))

# ACF AND PACF
ggAcf(D.prices_data)+ theme_bw()
ggPacf(D.prices_data)+ theme_bw()

# Find the best fitted ARIMA model 
Fitted.ARIMA = auto.arima(port_returns_ts)
# Plot forecasted ARIMA with the last
q= forecast(Fitted.ARIMA,h=3)
summary(q)
autoplot(q,include = 50)+ theme_bw()

coeftest(Fitted.ARIMA)
# Obtain the information criteria
AIC(Fitted.ARIMA)
BIC(Fitted.ARIMA)
# Estimate an AR(2) model, test coefficients significance and obtain the information criteria

