########################################################################
#                                                                      #
# Statistics 101                                                       #
# Exercise 5: Timeseries and forecasting                               #
#                                                                      #
########################################################################

# Load packages needed for analyses
library(ggplot2)
library(dplyr)
library(readxl)
library(zoo)
library(TTR)
library(forecast)

###################### Import and split the data #######################

# Here we import data about two databases and their average immediate locks 
# reported per 10 minutes.

immediate_lock <- read_excel("db_locks.xlsx")
immediate_lock$series <- factor(immediate_lock$series, levels=c("database1", "database2"))

db1_lock <- subset(immediate_lock, series=="database1")
db2_lock <- subset(immediate_lock, series=="database2")
db2_lock <- filter(db2_lock, time  >= db1_lock$time[281])
db1_lock <- filter(db1_lock, time  >= db1_lock$time[281])

###################### Create Timeseries data #######################

# Get a vector of all the unique dates
dates <- db1_lock$time[order(db1_lock$time)]

# Create a zoo object (in preparation of the time series analysis) ordered
# by the unique dates, for both databases.
db1_lock_ts <- zoo(db1_lock$mean_imm_grant_lock, order.by=dates)
db2_lock_ts <- zoo(db2_lock$mean_imm_grant_lock, order.by=dates)


###################### Run Timeseries for DB1 #######################

# Create a timeseries object for db1. We want to look at daily trends,
# and because there are 144 blocks of 10 minutes within one day, we set
# frequency to 144.

db1_lock_ts <- ts(db1_lock_ts, frequency=144)

# Decompose the timeseries to get the trend, seasonal, and cyclical parts
db1_lock_ts_comp <- decompose(db1_lock_ts)

# Use the following syntax to create a pretty plot of the decomposed timeseries.
# Just run all the lines.
db1_comp <- merge(as.zoo(db1_lock_ts_comp$x), as.zoo(db1_lock_ts_comp$trend), as.zoo(db1_lock_ts_comp$seasonal), as.zoo(db1_lock_ts_comp$random))
colnames(db1_comp) <- c("Observed", "Trend", "Seasonal", "Random")

db1_comp <- fortify(db1_comp, melt = TRUE)
db1_comp$Index <- dates

ggplot(aes(x = Index, y = Value, group = Series, colour = Series, linetype = Series), 
       data = db1_comp) + 
  geom_line() + xlab("Date") + ylab("Average Immediate Locks per 10 min") +
  #scale_x_date(breaks = "1 day", minor_breaks="1 hour", labels=date_format("%e-%b %R")) +
  geom_hline(yintercept=0, color="gray") +
  facet_grid(Series ~ .) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, vjust=0.5)) +
  theme(axis.line = element_line(size = .5, color = "black"), 
        #panel.grid.minor = element_line(size = 0, color = "white"),
        strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12),
        legend.position="none")

# What does the plot show? Does mean immediate locks show an increasing or decreasing trend
# over time or is it quite stable? Is there a strong seasonal pattern? Are there any
# weird values left in the Random noise part of the model?



###################### Forecasting for DB1 #######################

# Split the timeseries in a training and testing part. 
# The training part includes the first 5 days, the
# testing part includes the final 1.9 days of the data.

train_db1_lock <- window(db1_lock_ts,start=1, end=5)
test_db1_lock <- window(db1_lock_ts,start=5, end=c(6,9))

# Fit a simple and exponential moving average line to the timeseries.
# For both models, I want to include 6*10 minute observations in the
# calculation of the moving average. So I use the previous hour to model
# the next 10 minutes.

simple_MA <- SMA(train_db1_lock, n=6)
expo_MA <- EMA(train_db1_lock, n=6)

# Plot both the observed timeseries and the two moving average models.
# Are there any clear differences between the two models?
plot(train_db1_lock)
lines(x = index(train_db1_lock), y =simple_MA, col="red")
lines(x = index(train_db1_lock), y =expo_MA, col="blue")

# If you want the analysis to pick the best fitting model,
# you can use the ets or stlf function.

# Use ets function when you have a frequency lower than or equal to 24, or stlf
# when you have a frequency higher than 24. So, for our data, we use stlf.

fcast_db1 <- stlf(train_db1_lock, h = 144)

# Now we can forecast into the future based on the model.
fcast_db1 <- forecast(fcast_db1)

# Plot the forecast and add the observed test-data values in red.
plot(fcast_db1)
lines(test_db1_lock, col="red")


# Does the model seem to be a good forecaster of future behavior of database1?

# Test model accuracy formally.
# The error values of the test set are supposed to be larger, but
# how much larger is too large? 
# Key is to compare different prediction models. 
# The function above automatically picks the best model for the data.
accuracy(fcast_db1, test_db1_lock)

# Some definitions
# Forecast error is Y - F (Observed minus Forecast)
# There are various measures of Forecast accuracy.

# Some are dependent on scale of the errors.
# Mean Absolute Error (MAE) = mean(abs(error))
# Mean Square Error (MSE) = mean(error^2)
# Root Mean Square Error (RMSE) = sqrt(mean(error^2))

# Some are percentage errors.
# Mean Absolute Percentage Error (MAPE) = mean(100*e/Y)

# Some are Scale free errors.
# MASE 
# a scaled error is < 1 if it is a better forecast than a naive forecast
# a scaled error is > 1 if it is a better forecast than a naive forecast

###################### Anomaly detection for DB1 ####################

# To find anomalies in the timeseries, we can use all kinds of algorithms.
# Here, I use the following rule: observation x is an anomaly when its
# absolute value is larger than the median of random fluctuations +
# 3 * the standard deviation of random fluctuations.

db1_comp_random <- db1_comp$Value[which(db1_comp$Series == "Random")]
anomaly <-data.frame(anom = abs(db1_comp_random) > median(db1_comp_random, na.rm=TRUE) + 3*sd(db1_comp_random, na.rm=TRUE), 
                     value =db1_comp_random, Index = db1_comp$Index[which(db1_comp$Series == "Random")])


# Plot of the random fluctuations that cannot be explained by trend or seasonality.
# Anomalies are shown with open dots.

ggplot(aes(x = Index, y = value), data = anomaly) + 
  geom_line(linetype="dashed", color="#C77CFF") +
  xlab("Date") + ylab("Random") +
  geom_point(aes(x= Index, y=ifelse(anom==FALSE , NA, value)), size = 6, color="black", shape="o") +
  geom_hline(yintercept=0, color="gray") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, vjust=0.5)) +
  theme(axis.line = element_line(size = .5, color = "black"), 
        #panel.grid.minor = element_line(size = 0, color = "white"),
        strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12),
        legend.position="none")


# You can also run all this code for database 2 by changing all mentions of db1
# to db2 from the header Run Timeseries for DB1 onward.