# Computer lab 2

# Load supporting functions
source('bases.r')
source('ps_normal.r')
source('ps_poisson.r')
source('ps_binomial.r')

# S1: Generate scattered data
m = 50
set.seed(555)
x = runif(m)
y = sin(5.5 * x) + 2.5 + rnorm(m) * 0.3
plot(x, y)

# S2: smooth with ps.normal, using default parameters
pn1 = ps.normal(x, y)

# S3: smooth with ps,normal, setting all parameters
# (look at code in 'ps_normal.r' for meaning of parameters)
pn2 = ps.normal(x, y, bdeg = 3, nseg = 40, lambda = 10, pord = 2, se = 1)

# Decorate the graph
title('ps.normal() in action', xlab = 'x', ylab = 'y')

# S4: Read the ozone data and show first lines 
Data = read.table('air.txt', header = T)
head(Data)

# S5: Smooth ozone 
pn3 = ps.normal(Data$wind, Data$ozone)
title('Ozone data', ylab = 'Ozone concentration', xlab = 'Wind speed')

# S6: Using cross-validation to optimize smooth
lambda = 1
pn =  ps.normal(Data$wind, Data$ozone, lambda = lambda, plot = F)
cat(paste('lambda =', lambda, '  CV = ', pn$cv, '\n'))
lambda = 10
pn =  ps.normal(Data$wind, Data$ozone, lambda = lambda, plot = F)
cat(paste('lambda =', lambda, '  CV = ', pn$cv, '\n'))
lambda = 100
pn =  ps.normal(Data$wind, Data$ozone, lambda = lambda, plot = F)
cat(paste('lambda =', lambda, '  CV = ', pn$cv, '\n'))

# S7: A plot of CV is more useful
lla = seq(-1, 3, by = 0.2)
cvs = 0 * lla
for (k in 1:length(lla)) {
  lambda = 10 ^ lla[k]
  pn =  ps.normal(Data$wind, Data$ozone, lambda = lambda, plot = F)
  cvs[k] = pn$cv
}
plot(lla, cvs, xlab = 'log10(lambda)', ylab = 'CV')
lines(lla, cvs)

# S8: Apparently lambda = 10 ^ 1.4 is a optimal
lambda = 10 ^ 1.4
pn =  ps.normal(Data$wind, Data$ozone, lambda = lambda, plot = T)
cat(paste('lambda =', lambda, '  CV = ', pn$cv, '\n'))
title('Ozone data', xlab = 'Ozone concentration', ylab = 'Wind speed')

# S9: Now we switch to count data and Poisson smoothing
# The mining disaster data
Mine = read.table('mine.txt', header = F)
head(Mine)
x = Mine[, 1]
y = Mine[, 2]
plot(x, y)

# S10: Default smoothing
pp1 = ps.poisson(x, y)
title('Mining disasters', xlab = 'Year', ylab = 'Count')

# S11: A plot of AIC 
lla = seq(-2, 2, by = 0.2)
aics = 0 * lla
for (k in 1:length(lla)) {
  lambda = 10 ^ lla[k]
  pp =  ps.poisson(x, y, nseg = 20, lambda = lambda, plot = F)
  aics[k] = pp$aic
}
plot(lla, aics, xlab = 'log10(lambda)', ylab = 'CV')
lines(lla, aics)

# S12: Optimal smoothing
pp1 = ps.poisson(x, y, nseg = 20, lambda = 1)
title('Mining disasters', xlab = 'Year', ylab = 'Count')

# S13: Histogram smoothing of Old Faithful data
OF = read.table('faithful.dat')
le = OF[, 2]  # length of eruptions
he = hist(le, breaks = seq(1, 6, by = 0.1))
y = he$counts
x = he$mids
pp1 = ps.poisson(x, y, plot = F)
lines(pp1$xgrid, pp1$ygrid, col = 'blue')
pp2 = ps.poisson(x, y, lambda = 0.1, plot = F)
lines(pp2$xgrid, pp2$ygrid, col = 'red')

# S14: Search for optimal lambda, with a plot of AIC 
lla = seq(-2, 1, by = 0.2)
aics = 0 * lla
for (k in 1:length(lla)) {
  lambda = 10 ^ lla[k]
  pp =  ps.poisson(x, y, nseg = 20, lambda = lambda, plot = F)
  aics[k] = pp$aic
}
plot(lla, aics, xlab = 'log10(lambda)', ylab = 'CV')
lines(lla, aics)

# S15: Optimal smoothing of Old Faithful data
he = hist(le, breaks = seq(1, 6, by = 0.1))
y = he$counts
x = he$mids
pp3 = ps.poisson(x, y, nseg = 20, lambda = 0.1, plot = F)
lines(pp3$xgrid, pp3$ygrid, col = 'blue')
  
# S16: And finally, binomial data
load('Kyphosis.Rdata')
x = kyphosis$Age
y = 1 * (kyphosis$Kyphosis == 'present')  # make y 0/1
pb1 = ps.binomial(x, y)
title('Kyphosis data', xlab = 'Age', ylab = 'Occurence')

# S17: Use your newly gained experience to optimize smooth
