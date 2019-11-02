# Libraries
# library(fGarch)
library(rugarch)

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
plot(arch1.e, type='l', col='blue')
# Plot ACF and PACF
par(mfrow=c(1, 2)) # Set plots layout
acf(arch1.e, lag.max=30, type='correlation', plot=T, main='ACF')
acf(arch1.e, lag.max=30, type='partial', plot=T, main='PACF')
# Plot qqplot and distribution
qqnorm(arch1.e, pch=1, frame=F, main='Quantiles') # QQplot
qqline(arch1.e, col="steelblue", lwd=2) # Add QQline
hist(arch1.e, freq=F, main='Distribution') # Distplot (histogram)
lines(density(arch1.e)) # Distplot (kde)
par(mfrow=c(1,1)) # Reset plots layout

# Plot squared residuals
plot(arch1.e**2, type='l', col='blue')
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
# Plot ACF and PACF
par(mfrow=c(1, 2)) # Set plots layout
acf(garch11.e, lag.max=30, type='correlation', plot=T, main='ACF')
acf(garch11.e, lag.max=30, type='partial', plot=T, main='PACF')
# Plot qqplot and distribution
qqnorm(garch11.e, pch=1, frame=F, main='Quantiles') # QQplot
qqline(garch11.e, col="steelblue", lwd=2) # Add QQline
hist(garch11.e, freq=F, main='Distribution') # Distplot (histogram)
lines(density(garch11.e)) # Distplot (kde)
par(mfrow=c(1,1)) # Reset plots layout

# Plot squared residuals
plot(garch11.e**2, type='l', col='blue')
# Plot ACF and PACF
par(mfrow=c(1, 2)) # Set plots layout
acf(garch11.e**2, lag.max=30, type='correlation', plot=T, main='ACF')
acf(garch11.e**2, lag.max=30, type='partial', plot=T, main='PACF')
par(mfrow=c(1,1)) # Reset plots layout

# Define GARCH(1, 1) model
model <- ugarchspec(
  mean.model=list(armaOrder=c(0,0), include.mean=F),
  variance.model=list(model="sGARCH", garchOrder=c(1,1)),
  distribution.model='norm'
)
# Fit the model
model.fit <- ugarchfit(spec=model, data=garch11.e)
model.fit
