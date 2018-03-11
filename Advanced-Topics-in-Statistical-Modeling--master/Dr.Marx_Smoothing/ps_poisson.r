ps.poisson = function(x, y, nseg = 10 ,bdeg = 3, pord = 3, lambda = 1, 
                      plot = T, show = F)
{
# Function ps.poisson: smooths scattered Poisson response data with P-splines.
# Input: x = abcissae of data.
# Input: y = response (counts).
# Input: nseg = number of intervals for B-splines.
# Input: bdeg = degree of B-splines.
# Input: pord = order of difference penalty.
# Input: lambda = smoothness parameter.
#
# Output: a list with many components
#
# Side effect: a plot of (x,y) and the estimated probability.

#
# Paul Eilers and Brian Marx, 2007
#

  # Compute B-spline basis
  m = length(x)
  xl = min(x)
  xr = max(x)
  B = bbase(x, xl = xl, xr = xr, nseg = nseg, deg = bdeg)

  # Construct penalty stuff
  n = dim(B)[2]
  P = sqrt(lambda) * diff(diag(n), diff = pord)
  nix = rep(0, n - pord)

  # Initialize
  znew = log(y + 0.01)
  z = rep(1, m)

  # Fit
  for (it in 1:20) {
    mu = exp(z)
    w = mu
    u = (y - mu) / w + z
    f = lsfit(rbind(B, P), c(u, nix), wt = c(w, (nix + 1)), intercept = F)
    beta = f$coef
    znew = B %*% beta
    dz = max(abs(z - znew))
    z = znew
    if(dz < 1e-005)  break
    if (show) print(c(it, dz))
  }

  # Compute AIC
  dev = 2 * sum(y  * log((y + 1e-9) / mu))
  h = hat(f$qr)[1:m]
  ed = sum(h)
  aic = dev + 2 * ed

  # Compute curve on grid
  xgrid = seq(xl, xr, length = 50)
  Bu = bbase(xgrid, xl = xl, xr = xr, nseg = nseg, deg = bdeg)
  zu = Bu %*% beta
  ygrid = exp(zu)

  # Plot data and fit
  if (plot) {
     plot(x, y, xlab = '', ylab = '')
     lines(xgrid, exp(zu), col = 'blue')
  }

  # Return list
  pp = list(aic = aic, x = x, y = y, muhat = mu, nseg = nseg,
             bdeg = bdeg, pord = pord,
             lambda = lambda, xgrid = xgrid, ygrid = ygrid, effdim = ed,
             dispersion = 1, family = "poisson", link = "log")
  class(pp) = "pspfit"
  return(pp)

}
