# Scripts for computer lab 1
# 
# Paul Eilers
# You can freely steal from this code for your own projects ;-)

# Load the functions to compute bases
# (take a look at these functions later)
source('bases.r')

# S1: Generate x on a linear grid ans simulate y
m = 101
x = seq(0, 1, length = m)
set.seed(555)
y = sin(5.5 * x) + 2.5 + rnorm(m) * 0.3

# Scatterplot to check
plot(x, y)


# S2: Compute a B-spline basis with default parameters and show it
# (cubic splines, 10 segments, min and max computed from x)
B = bbase(x)
matplot(x, B)

# Use smaller symbols and add connecting lines
matplot(x, B, cex = 0.7)
matlines(x, B)

# Plot without symbols, solid and thicker lines
matplot(x, B, type = 'l', lty = 1, lwd = 2)

# S3: Less B-splines
B = bbase(x, nseg = 5)
matplot(x, B, type = 'l', lty = 1, lwd = 2)

# S4: Linear B-splines
B = bbase(x, nseg = 5, deg = 1)
matplot(x, B, type = 'l', lty = 1, lwd = 2)

# S5: Regression on B-splines (no intercept!)
B = bbase(x)
fit = lsfit(B, y, intercept = F)

# S6: Extract coefficients and compute fit
a = fit$coefficients
z = B %*% a
plot(x, y)
lines(x, z, col = 'red')

# S7: A less wiggly fit, using a smaller basis
B = bbase(x, nseg = 3)
fit = lsfit(B, y, intercept = F)
a = fit$coeff
z = B %*% a
plot(x, y)
lines(x, z, col = 'red')

# S8: A piecewise linear fit, using splines of degree 1
B = bbase(x, nseg = 3, deg = 1)
fit = lsfit(B, y, intercept = F)
a = fit$coeff
z = B %*% a
plot(x, y)
lines(x, z, col = 'red')

# S9: Fitting B-splines by doing the explicit computations yourself
B = bbase(x)
a = solve(t(B) %*% B, t(B) %*% y)
z = B %*% a
plot(x, y)
lines(x, z, col = 'red')

# S10: Now we add a penalty
lambda = 1
n = ncol(B)
D = diff(diag(n))
P = t(D) %*% D
a = solve(t(B) %*% B + lambda * P, t(B) %*% y) # Notice penalty
z = B %*% a
plot(x, y)
lines(x, z, col = 'red')

# S11: A strong penalty gives an almost constant curve
lambda = 1000
P = t(D) %*% D
a = solve(t(B) %*% B + lambda * P, t(B) %*% y)
z = B %*% a
plot(x, y)
lines(x, z, col = 'blue')

# S12: compare to average of y
abline(h = mean(y), col = 'red')

# S13: A strong second order penalty
lambda = 1000
D = diff(diag(n), diff = 2)
P = t(D) %*% D
a = solve(t(B) %*% B + lambda * P, t(B) %*% y)
z = B %*% a
plot(x, y)
lines(x, z, col = 'blue')

# S14: Compare to linear regression line
lf = lsfit(x, y)
abline(lf$coef[1], lf$coef[2], col = 'red')

# S15: Generate scattered data
m = 50
set.seed(555)
x = runif(m)
y = sin(5.5 * x) + 2.5 + rnorm(m) * 0.3
plot(x, y)

# S16: P-spline fit with second order penalty
lambda = 1
B = bbase(x, xl = 0, xr = 1, nseg = 10, deg = 3)
n = ncol(B)
D = diff(diag(n), diff = 2)
P = t(D) %*% D
a = solve(t(B) %*% B + lambda * P, t(B) %*% y)
z = B %*% a
plot(x, y)
lines(x, z, col = 'blue')

# S17: Oops, the plot looks bad, because x is scattered
# We need to compute the curve on a grid
xg = seq(0, 1, length = 101)
Bg = bbase(xg, xl = 0, xr = 1, nseg = 10, deg = 3)
zg = Bg %*% a
plot(x, y)
lines(xg, zg, col = 'blue')

# S18: Interpolation and extrapolation
sel = x < 0.3 | x > 0.7
x = x[sel]
y = y[sel]
lambda = 1
B = bbase(x, xl = -0.3, xr = 1.3, nseg = 10, deg = 3)
n = ncol(B)
D = diff(diag(n), diff = 2)
P = t(D) %*% D
a = solve(t(B) %*% B + lambda * P, t(B) %*% y)
xg = seq(-0.3, 1.3, length = 101)
Bg = bbase(xg, xl = -0.3, xr = 1.3, nseg = 10, deg = 3)
zg = Bg %*% a
plot(x, y, xlim = c(-0.3, 1.3), ylim = c(0, 4))
lines(xg, zg, col = 'blue')
