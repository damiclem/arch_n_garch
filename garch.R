# Libraries

library(fGarch)
library(rugarch)
library(quantmod)

# Set random seed
set.seed(13)

# Simulate ARCH(1) series
arch1.a0 <- 2.0
arch1.a1 <- 0.5
arch1.e <- rnorm(1000) # Initialize innovations (dependent * independent)
arch1.z <- arch1.e # Initialize random innovations (independent component)
# Compute dependent innovations
for(i in 2:1000) {
  # Overwrite previously initialized value
  arch1.e[i] <- arch1.z[i] * sqrt(arch1.a0 + arch1.a1 * arch1.e[i-1]**2)
}

# Plot simulated ARCH(1) series
plot(arch1.e, type='l', col='blue', xlab="t", ylab="Error", main="ARCH(1)")
abline(h=0, col="black")

# Plot ACF and PACF
par(mfrow=c(1, 2)) # Set plots layout
acf(arch1.e, lag.max=30, type='correlation', plot=T, main='ACF')
acf(arch1.e, lag.max=30, type='partial', plot=T, main='PACF')
# Plot qqplot and distribution
qqnorm(arch1.e, pch=1, frame=F, main='Quantiles') # QQplot
qqline(arch1.e, col="steelblue", lwd=2) # Add QQline
hist(arch1.e, freq=F, main='Distribution', xlab="Errors", ylim=c(0,0.25)) # Distplot (histogram)
lines(density(arch1.e)) # Distplot (kde)
par(mfrow=c(1,1)) # Reset plots layout

# Plot squared residuals
par(mfrow=c(1, 1)) # Set plots layout
plot(arch1.e**2, type='l', col='blue', xlab="t", ylab="Squared errors", main="Squared errors")
#qqnorm(arch1.e**2, pch=1, frame=F, main='Quantiles') # QQplot
#qqline(arch1.e, col="steelblue", lwd=2) # Add QQline
# Plot ACF and PACF
par(mfrow=c(1, 2)) # Set plots layout
acf(arch1.e**2, lag.max=30, type='correlation', plot=T, main='ACF')
acf(arch1.e**2, lag.max=30, type='partial', plot=T, main='PACF')
par(mfrow=c(1,1)) # Reset plots layout

# # Fit an ARCH(1) model on the simulated series
# model <- garchFit(~garch(1, 0), data=arch1.e, include.mean=F, trace=F)
# model

# First, define ARCH model specifications
model <- ugarchspec(
  mean.model=list(armaOrder=c(0,0), include.mean=F),
  variance.model=list(model = "sGARCH", garchOrder = c(1,0)),
  distribution.model='norm'
)
# Fit the model
model.fit <- ugarchfit(spec=model, data=arch1.e)
model.fit


###################################################################################################################
#GARCH


# Simulate GARCH(1,1) series
garch11.a0 <- 0.2
garch11.a1 <- 0.5
garch11.b1 <- 0.3
garch11.z <- rnorm(10000) # Initialize innovations (dependent * independent)
garch11.e <- rep.int(0, 10000) # Initialize random innovations (independent component)
garch11.ssq <- rep.int(0, 10000) # Initialize sigma squared
# Compute dependent innovations
for(i in 2:10000) {
  # Overwrite previously initialized values
  garch11.ssq[i] <- garch11.a0 + garch11.a1*(garch11.e[i-1]**2) + garch11.b1*garch11.ssq[i-1]
  garch11.e[i] <- garch11.z[i] * sqrt(garch11.ssq[i])
}

# Plot simulated ARCH(1) series
plot(garch11.e, type='l', col='blue')
abline(h=0, col="black")
# Plot ACF and PACF
par(mfrow=c(1, 2)) # Set plots layout
acf(garch11.e, lag.max=30, type='correlation', plot=T, main='ACF')
acf(garch11.e, lag.max=30, type='partial', plot=T, main='PACF')
# Plot qqplot and distribution
qqnorm(garch11.e, pch=1, frame=F, main='Quantiles') # QQplot
qqline(garch11.e, col="steelblue", lwd=2) # Add QQline
hist(garch11.e, freq=F, main='Distribution', ylim=c(0,0.4)) # Distplot (histogram)
lines(density(garch11.e)) # Distplot (kde)
par(mfrow=c(1,1)) # Reset plots layout

# Plot squared residuals
plot(garch11.e**2, type='l', col='blue')
#qqnorm(garch11.e**2, pch=1, frame=F, main='Quantiles') # QQplot
#qqline(garch11.e**2, col="steelblue", lwd=2) # Add QQline


# Plot ACF and PACF
par(mfrow=c(1, 2)) # Set plots layout
acf(garch11.e**2, lag.max=30, type='correlation', plot=T, main='ACF')
acf(garch11.e**2, lag.max=30, type='partial', plot=T, main='PACF')
par(mfrow=c(1,1)) # Reset plots layout

# Define GARCH(1, 1) model
model <- ugarchspec(
  mean.model=list(armaOrder=c(0,0), include.mean=F),
  variance.model=list(model='sGARCH', garchOrder=c(1,1)),
  distribution.model='norm'
)
# Fit the model
model.fit <- ugarchfit(spec=model, data=garch11.e)
model.fit

################################################################################################################
# Wilshere 5000 index

# Read wilshere 5000 index data
W5000 <- read.csv2('data/wilshire5000.csv', stringsAsFactors=F, header=T, sep=',', na.strings='.')
# Parse data
W5000$DATE <- as.Date(W5000$DATE)
W5000$WILL5000INDFC <- as.numeric(W5000$WILL5000INDFC)
# Clean the data: remove rows with NAs
W5000 <- na.omit(W5000)
# Compute new dataset containing daily percentage changes
W5000_PC <- data.frame(
  # Keep same date column
  'Date' = W5000$DATE,
  # Compute new value as percent difference wrt previous index value
  'Value' = as.numeric(Delt(W5000$WILL5000INDFC) * 100)
)
# Clean new dataset data
W5000_PC <- na.omit(W5000_PC)

# Plot percentage changes
plot(W5000_PC, ylab='Percent', main='Daily Percentage Changes', type='l', col='steelblue', lwd=0.5)
abline(0, 0) # Add horizontal line at y = 0

# Plot ACF and PACF
par(mfrow=c(1, 2)) # Set layout
acf(W5000_PC$Value, type='correlation', main='Wilshire 5000 Series ACF')
acf(W5000_PC$Value, type='partial', main='Wilshire 5000 Series PACF')
par(mfrow=c(1, 1)) # Reset layout

# Define GARCH(1,1) model of daily percentage changes
model <- ugarchspec(
  mean.model=list(armaOrder=c(0,0), include.mean=T),
  variance.model=list(model='sGARCH', garchOrder=c(1,1)),
  distribution.model='norm'
)
# Estimate coefficients for the model
model.fit <- ugarchfit(spec=model, data=W5000_PC$Value)
model.fit
# Save fitted model coefficients
model.coef <- coef(model.fit)
model.coef

# Add column to dataset: deviance from the mean (first coefficient)
W5000_PC$Dev <- W5000_PC$Value - model.coef[1]

# Plot deviation of percentage changes from mean
plot(W5000_PC$Date, W5000_PC$Dev, type = 'l', lwd = 0.2, col = 'steelblue', 
     ylab = 'Percent', xlab = 'Date',
     main = 'Estimated Bands of +- One Conditional Standard Deviation')
# Add horizontal line at y = 0
abline(0, 0)
# Add GARCH(1,1) confidence bands (one standard deviation) to the plot
lines(W5000_PC$Date, model.coef[1] + sigma(model.fit), col = 'darkred', lwd = 0.5)
lines(W5000_PC$Date, model.coef[1] - sigma(model.fit), col = 'darkred', lwd = 0.5)
