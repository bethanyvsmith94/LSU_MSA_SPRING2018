source('bases.r')

ps.normal= function(x, y, nseg = 10, bdeg = 3, pord = 2, lambda = 1, 
                   plot = T, se = 2)
{
# Function ps.normal: smooths scatterplot data with P-splines.
# Input: x = abcissae of data.
# Input: y = response (counts).
# Input: nseg = number of intervals for B-splines.
# Input: bdeg = degree of B-splines.
# Input: pord = order of difference penalty.
# Input: lambda = smoothness parameter.
# Input: plot = plot data and fit (or not) 
# Input: se = width of standard error bands
#
# Output: object with many components.
#
# Side effect: a plot of (x,y), the estimated curve and error bands.

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

    # Fit
    f = lsfit(rbind(B, P), c(y, nix), intercept = F)
    h = hat(f$qr)[1:m]
    beta = f$coef
    mu = B %*% beta

    # Cross-validation and dispersion
    r = (y - mu ) / (1 - h)
    cv = sqrt(sum(r ^2))
    sigma = sqrt(sum((y - mu) ^2) / (m - sum(h)))

    # Compute curve on grid
    u = seq(xl, xr, length = 100)
    Bu = bbase(u, xl = xl, xr = xr, nseg = nseg, deg = bdeg)
    zu = Bu %*% beta

    # Plot data and fit
    if (plot) {
       plot(x, y, main = '', xlab = '', ylab = '')
       lines(u, zu, col = 'blue')
    }

    # Error bands ("Bayesian estimate")
    if (plot & se > 0 ) {
         Covb = solve(t(B) %*% B + t(P) %*% P)
         Covz = sigma ^ 2 * Bu %*% Covb %*% t(Bu)
         seb = se * sqrt(diag(Covz))
         lines(u, zu + seb, lty = 2, col = 'red')
         lines(u, zu - seb, lty = 2, col = 'red')
    }
    
    # Return list
    pp = list(x = x, y = y, muhat = mu, nseg = nseg,
               xmin = xl, xmax = xr, bdeg = bdeg, pord = pord,
               lambda = lambda, xgrid = u, ygrid = zu,
               cv = cv, effdim = sum(h), ed.resid = m - sum(h),
               family = "gaussian", link = "identity", sigma = sigma)
    class(pp) = "pspfit"
    return(pp)
}
