# FAMA FRENCH CAPM 
set.seed(1234)
suppressPackageStartupMessages({
  
  library(broom)
  library(glue)
  
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
  })

tick <- c("FB","CNK","TTWO","PENN", "WYNN")

prcs <- tq_get(tick,
               # src = 'yahoo',
               from = "2016-01-01",
               to = "2019-12-31",
               get = "stock.prices",
               periodicity = "monthly")

w <- runif(n = length(tick))
w <- w/sum(w)
sum(w)

# Calculating the logarithmic monthly returns for these stocks
log_return_long <- prcs %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               col_rename = 'ret',
               type = 'log') %>% 
  subset(ret !=0)
log_return_long

#  find portfolio monthly returns
portfolio_returns_tq_rebalanced_monthly <- 
  log_return_long %>%
  tq_portfolio(assets_col  = symbol, 
               returns_col = ret,
               weights     = w,
               col_rename  = "returns",
               rebalance_on = "months")

# Importing and Wrangling the Fama French Factors
temp <- tempfile()
base <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip"

download.file(base,temp,quiet = TRUE)

# read the csv
# roll monthly dates back to the last day of the previous month. 

Global_3_Factors <- 
  read_csv(unzip(temp), 
           skip = 3) %>% 
  rename(date = ...1) %>% 
  mutate_at(vars(-date), as.numeric) %>% 
  mutate(date = 
           rollback(ymd(parse_date_time(date, "%Y%m") + months(1)),
                    roll_to_first = TRUE)) %>% 
  subset(date >="2016-01-01" & date <= "2019-12-01")

Global_3_Factors
# map(Global_3_Factors, class)
# convert the FF data to decimal and create a new column called R_excess 
# to hold our returns above the risk-free rate.
ff_portfolio_returns <- 
  portfolio_returns_tq_rebalanced_monthly %>% 
  left_join(Global_3_Factors, by = "date") %>% 
  mutate(MKT_RF = `Mkt-RF`/100,
         SMB = SMB/100,
         HML = HML/100,
         RF = RF/100,
         R_excess = round(returns - RF, 4))

head(ff_portfolio_returns, 4)

library(moderndive)
#  Fitting the code into a linear regression 
score_model<- lm(R_excess ~ MKT_RF + SMB + HML,
         data = ff_portfolio_returns)

ff_dplyr_byhand <- get_regression_table(score_model)
ff_dplyr_byhand
# summary.lm(score_model)
get_regression_summaries(score_model)

# FAMA FRENCH 3 FACTOR COEFFICIENT PLOT 
ff_plot<-
ff_dplyr_byhand %>% 
  mutate_if(is.numeric, funs(round(., 3))) %>%
  subset(term != "intercept") %>% 
  ggplot(aes(x = term, y = estimate, shape = term, color = term)) + 
  geom_point(size=3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci)) +
  labs(title = "FF 3-Factor Coefficients for Our Portfolio",
       x = "Factors",
       y = "coefficient",
       caption = "data source: Fama French website and yahoo! Finance") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption  = element_text(hjust = 0))

plotly::ggplotly(ff_plot)

#  find portfolio monthly returns
portReturns_tbl <- 
  log_return_long %>%
  tq_portfolio(assets_col  = symbol, 
               returns_col = ret,
               weights     = w,
               col_rename  = "returnsP",
               rebalance_on = "months") 

portReturns_tbl

# Coerce the data to a time series
portReturns_ts<-
  portfolio_returns_tq_rebalanced_monthly %>% 
  tk_ts(select = returns, start = .$date[1] %>% 
          as.yearmon(), frequency = 12)
portReturns_ts

# Coerce the data to a tsibble
portReturns_tsi <- as_tsibble(portReturns_ts, index = date) %>%
  rename(returnsP = value, date = index)
# Scatterplot
portReturns_tbl %>%
  ggplot(aes(x = date, y = returnsP)) +
  geom_line(show.legend = FALSE, color = "steelblue") +
  geom_hline(aes(yintercept = mean(returnsP)), color = "firebrick2", 
             linetype = "dashed", size = 0.7) +
  labs(title = "Portfolio monthly Returns, 2016-2021",
       subtitle = "(Mean return in red)",
       y = "Portfolio Return", x = "date") +
  scale_y_continuous(labels = scales::percent_format()) +    
  scale_x_date(date_labels = "%Y %b") +
  theme_minimal()

# Violin plot of returns    
portReturns_tbl %>%
  mutate(yr = year(date)) %>%
  ggplot(aes(x = date, y = returnsP)) +
  geom_violin(aes(group = yr), draw_quantiles = c(0.25, 0.5, 0.75), 
              show.legend = FALSE, fill = "lightblue2", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "darkgrey", size = 0.7) +
  geom_hline(aes(yintercept = mean(returnsP)), color = "firebrick2", 
             size = 0.7, linetype = "longdash") +
  labs(title = "Violin plots of Portfolio Returns by Year",
       subtitle = "Mean in red",
       x = "", y = "Return") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y %b") +
  geom_point(aes(color = factor(yr)), size = 0.9,
             show.legend = FALSE) +
  theme_hc() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  facet_grid(~ yr, scale = "free")

# Calendar plot
calendar_plot<-
portReturns_tbl %>% 
  mutate(date = date - lubridate::days(1)) %>%
  mutate(month = month(date),
         year = year(date)) %>%
  mutate(bin = cut(returnsP, 
                   breaks = c(-Inf, -0.06, 0, 0.06, Inf), 
                   labels = c("Below -6%", "(-6%, 0%)", "(0%, 6%)", "Above 6%"))) %>%
  ggplot(aes(x = factor(months(date, abbr = TRUE),
                        levels = month.abb),
             y = substr(year, 1, 4), fill = bin)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("firebrick2", "pink", "lightblue", "mediumturquoise")) +
  labs(title = "Calendar plot", subtitle = "Portfolio Monthly Returns", 
       x = "", y = "", fill = "") +
  theme_hc() +
  theme(panel.grid.major = element_blank(),
        panel.border= element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top",
        axis.ticks = element_blank())

plotly::ggplotly(calendar_plot)
# THe months of MARCH,JUNE,OCTOBER  AND DECEMBER experienced positive
# returns over the years

# Breush Pagan Test for heteroskedasticity
# - First create a linear model and use residuals to test for heteroskedasticity
portlmMod <- tslm(portReturns_ts ~ trend) # initial model
bptest(portlmMod) %>%
  sw_glance() %>%
  pull(p.value) %>%
  is_less_than(0.05) %>%
  if_else("Heteroskedastic (alpha = 5%)", 
          "Not heteroskedastic (alpha = 5%)")

# Augmented Dickey-Fuller Test for autocorelation(stationarity)
ndiffs(portReturns_ts)

# View the ACF and PCF plots
portReturns_ts %>% 
  ggtsdisplay(theme = theme_minimal(), main = "Portfolio returns monthly")

# Box Test for Autocorrelation
# - Test of whether any of a group of autocorrelations of a time series are different from zero. Instead of testing randomness at each distinct lag, it tests the "overall" randomness based on a number of lags.
Box.test(portReturns_ts, type = "Ljung") %>%
  sw_glance() %>%
  pull(p.value) %>%
  is_less_than(0.05) %>%
  if_else("Autocorrelation exists (alpha = 5%).", 
          "No autocorrelation exists (alpha = 5%).")
# Seasonal decomposition
# If the RMSE of the model with the seasonal component is higher 
# than that of the model without the seasonal component, 
# then the seasonal component likely contributed to model performance.

portReturns_tsi %>%
  model(STL(returnsP ~ season())) %>%
  components() %>%
  rename(original = returnsP,
         seasonal = season_year) %>%
  clean_names() %>%
  select(-model) %>%
  pivot_longer(-c(date, season_adjust), names_to = "component") %>%
  arrange(factor(component, levels = unique(component)), date) %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~ component) +
  labs(title = "Plot of Decomposed Portfolio Returns",
       x = "", y = "") +
  theme_minimal()

# Split the time series into training and test sets
initial_time_split0 <- initial_time_split(portReturns_tsi, prop = 0.8) 
training_data0 <- initial_time_split0 %>% training() 
test_data0 <- initial_time_split0 %>% testing()

portReturns_recipe <- training_data0 %>%
  recipe(returnsP ~ .) %>%
  prep()

# Extract the data from the portReturns_recipe object
portReturns_training <- portReturns_recipe %>% juice()

# Apply the recipe to the testing data
portReturns_testing <- portReturns_recipe %>% bake(test_data0)

# Fit multiple time series models
fit0 <- portReturns_training %>%
  model(
    # Naive, Random Walk Forecasts
    # Forecasts equal to last observed value (appropriate for many financial series)
    rw0 = RW(returnsP),
    # Drift method
    # Forecasts equal to last value plus average change over series (appears as line)
    rw.drift0 = RW(returnsP, drift = TRUE),
    # Forecasts equal to mean of historical data
    mean0 = MEAN(returnsP),
    # Seasonal Naive
    # Forecasts equal to last value from same season
    snaive0 = SNAIVE(returnsP ~ lag("year")),
    # ARIMA
    # Forecasts based on lagged values of series as well as lagged errors
    arima0 = ARIMA(returnsP),
    # TSLM (Time Series Linear Model)
    # Applies a trend, seasonal, and error terms to the data
    tslm0 = TSLM(returnsP ~ trend() + season()),
    # ETS (Exponential Time Series)
    # Uses an exponential model with trend and seasonality to create forecasts
    ets0 = ETS(returnsP)) %>%
  mutate(mixed = (rw0 + rw.drift0 + mean0 + snaive0 + arima0 + tslm0 + ets0) / 7)

# Optimal ARIMA model parameters
fit0 %>% select(arima0) %>% report()

# Create the forecasts
fcast0 <- fit0 %>% 
  forecast(h = nrow(portReturns_testing))

# Plot the forecasts
fcast0 %>% 
  filter(.model %in% c("rw0", "snaive0", "rw.drift0", "mean0")) %>%
  autoplot(portReturns_training) +
  labs(title = "Forecasts for Portfolio monthly returns",
       x = "Year", y = "Monthly return") +
  guides(colour = guide_legend(title = "Forecast")) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  facet_wrap(~ .model)

fcast0 %>% 
  filter(.model %in% c("arima0", "tslm0", "ets0", "mixed")) %>%
  autoplot(portReturns_training) +
  labs(title = "Forecasts for Portfolio monthly returns",
       x = "Year", y = "Monthly return") +
  guides(colour = guide_legend(title = "Forecast")) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  facet_wrap(~ .model)

# Model performance
forecast::accuracy(fcast0, portReturns_testing) %>% 
  arrange(desc(RMSE)) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped"),
                full_width = FALSE)

# Check residuals
interp.residuals0 <- fit0 %>% 
  residuals(type="response")

# Plot residuals
interp.residuals0 %>%
  ggplot(aes(x = as_date(date), y = .resid)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE, level = 0.95) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_date(date_labels = "%Y") +
  facet_wrap(~ .model, ncol = 3) +
  labs(title = "Residuals plot", x = "", y = "Residual") +
  theme_hc()

# Plot density of residuals
interp.residuals0 %>%
  ggplot(aes(x = .resid)) + 
  geom_density(aes(color = .model), show.legend = FALSE) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
  scale_x_continuous(labels = scales::percent_format()) +
  facet_wrap(~ .model, ncol = 3) +
  labs(title = "Density plots of residuals", x = "Residual", y = "Frequency") +
  theme_hc()

# VaR at  1% 
VaR(SP500_ts,p = .01)
VaR(portReturns_ts,p=.01)

# VaR at and 5%
VaR(portReturns_ts)
VaR(SP500_ts,p = .05)

# Plot of portfolio returns 
chartSeries(portReturns_ts)

# model specs for a GARCH constant model
model_specific1A = ugarchspec()

# model fitting 
mod_fitting1A<-ugarchfit(data = portReturns_ts,
                         spec = model_specific1A)

mod_fitting1A
plot(mod_fitting1A, which= "all")

forc= ugarchforecast(fitORspec = mod_fitting1A,n.ahead = 3)
plot(fitted(forc), type="l")
plot(sigma(forc), type= "l")

# AR(1) model on portfolio returns
ARIMA1A<-arima(portReturns_ts,order = c(1,0,0))
summary(ARIMA1A)

# ACF AND PACF
ggAcf(portReturns_ts)+ theme_bw()
ggPacf(portReturns_ts)+ theme_bw()

# Find the best fitted ARIMA model 
Fitted.ARIMA1A = auto.arima(portReturns_ts)
# Plot forecasted ARIMA with the last
q1A= forecast(Fitted.ARIMA1A,h=3)
summary(q1A)
autoplot(q1A,include = 50)+ theme_bw()

library(lmtest)
coeftest(Fitted.ARIMA)
# Obtain the information criteria
AIC(Fitted.ARIMA1A)
BIC(Fitted.ARIMA1A)

# Find the best fitted ARIMA model 
Fitted.ARIMA = auto.arima(port_returns_ts)
# Plot forecasted ARIMA with the last
q= forecast(Fitted.ARIMA,h=3)
summary(q)
autoplot(q,include = 50)+ theme_bw()

###############################################################
