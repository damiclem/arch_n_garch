# Load libraries
library(quantmod)
library(rugarch)

# Set random seed
set.seed(123)

# Obtain the S&P500 series and returns from quantmod library
getSymbols("^GSPC", from=as.Date('1950-01-01'), to=as.Date('2018-01-01'))
sp500 <- data.frame(Date=as.Date(index(GSPC)), Value=as.double(GSPC$GSPC.Close), row.names=NULL)
# Get log difference of log returns
sp500$Ret <- as.double(Delt(sp500$Value) * 100)
# Delete rows with NA values (e.g. the first after log diff)
sp500 <- na.omit(sp500)
# Show some dataset info
str(sp500)
# Remove GSPC (all info stored in sp500 dataframe)
rm(GSPC)

# Define layout for series and residuals plot
layout(mat = matrix(c(1, 2), nrow = 2, byrow = T),
       heights = c(1), # Heights of the two rows
       widths = c(1)) # Widths of the two columns
# Plot sp500 index
plot(sp500$Date, sp500$Value, col='steelblue', type='l', main='SP500 Index', ylab='Values', xlab='Date')
grid(col='lightgray', lty=1)
# Plot sp500 returns
plot(sp500$Date, sp500$Ret, col='steelblue', type='l', main='SP500 Returns', ylab='Returns', xlab='Date')
grid(col='lightgray', lty=1)

# Define layout for ACF and PACF functions
layout(mat = matrix(c(1, 2), nrow=1, byrow=T), heights=c(1), widths=c(1))
# Plot ACF and PACF of returns
acf(sp500$Ret, lag.max=30, type='correlation', plot=T, main='ACF')
acf(sp500$Ret, lag.max=30, type='partial', plot=T, main='PACF')

# Define and fit the GARCH model
garch.spec = ugarchspec(
  # Define a GARCH(1, 1) volatility model
  # Note that there is no need of grid search since this model is empirically believed to be sufficiently good
  variance.model=list(garchOrder=c(1, 1)),
  # Define a mean model using previously found  best ARIMA order
  mean.model=list(armaOrder=c(1, 0, 1), include.mean=T),
  # Define distribution
  distribution.model='sged'
)

garch.fit <- ugarchfit(garch.spec, sp500$Ret, out.sample=100, solver='hybrid')
garch.fit

garch.forecast <- ugarchforecast(garch.fit, data=NULL, n.roll=100, n.ahead=10)
garch.forecast

# Define layout for forecast
layout(mat=matrix(c(1, 2), nrow=2, byrow=T),
       heights = c(1), # Heights of the two rows
       widths = c(1)) # Widths of the two columns
# Plot forecast
plot(garch.forecast, which=2)
plot(garch.forecast, which=4)






