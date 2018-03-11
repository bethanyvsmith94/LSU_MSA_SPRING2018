# Computer lab 3 
library(mgcv)

# S1: Generate scattered data
m = 50
set.seed(555)
x = runif(m)
y = sin(5.5 * x) + 2.5 + rnorm(m) * 0.3
plot(x, y)
Dat1 = data.frame(x, y)

# S2 smooth with mgcv
g = gam(y ~ s(x), data = Dat1)
summary(g)

# S3 Predict curve
Dat2 = data.frame(x = seq(0, 1, length = 101))
z = predict(g, Dat2)
plot(x, y)
lines(Dat2$x, z, col = 'blue')

# S4 Predict curve with 2 SE bande
Dat2 = data.frame(x = seq(0, 1, length = 101))
z = predict(g, Dat2, se = T)
plot(x, y)
lines(Dat2$x, z$fit, col = 'blue')
lines(Dat2$x, z$fit - 2 * z$se.fit, col = 'red', lty = 2)
lines(Dat2$x, z$fit + 2 * z$se.fit, col = 'red', lty = 2)

# S5: Load the P-spline constructor
source('ps_construct.r')

# S6: Smooth with P-splines
g = gam(y ~ s(x, bs = 'ps', m = 2), data = Dat1)
summary(g)
# mgcv is picky about the allowed domain for prediction
Dat2 = data.frame(x = seq(min(Dat1$x), max(Dat1$x), length = 101))
z = predict(g, Dat2)
plot(x, y)
lines(Dat2$x, z, col = 'blue')

# S7: Read the ozone data and smooth
Dat1 = read.table('air.txt', header = T)
g = gam(ozone ~ s(wind, bs = 'ps', m = 2), data = Dat1)
Dat2 = data.frame(wind = seq(min(Dat1$wind), max(Dat1$wind), length = 101))
z = predict(g, Dat2, se = T)
plot(Dat1$wind, Dat1$ozone, ylab = 'Ozone', xlab = 'Wind speed')
lines(Dat2$wind, z$fit, col = 'blue')
lines(Dat2$wind, z$fit - 2 * z$se.fit, col = 'red', lty = 2)
lines(Dat2$wind, z$fit + 2 * z$se.fit, col = 'red', lty = 2)

# S8: There is an easier way, with certain drawbacks
plot(g, res = T)

# S9: A generalized additive model for the binomial Kyphosis data
load('kyphosis.Rdata')
head(kyphosis)
g = gam(Kyphosis ~ s(Age) + s(Start), data = kyphosis, family = 'binomial')
plot(g, res = T, cex = 4)

# S10: Two plots on one page
par(mfrow = c(2, 1))
plot(g, res = T, cex = 4, ask = F)



