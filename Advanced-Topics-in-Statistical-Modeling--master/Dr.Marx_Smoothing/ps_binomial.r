source('bases.r')

ps.binomial = function(x, y, nseg = 10, bdeg = 3, pord = 2, lambda = 1, 
              plot = T, se = 2, show = F)
{
  # Function ps.binomial: smooths scattered binary response data with P-splines.
  # Input: x = abcissae of data.
  # Input: y = binary response.
  # Input: nseg = number of intervals for B-splines.
  # Input: bdeg = degree of B-splines.
  # Input: pord = order of difference penalty.
  # Input: lambda = smoothness parameter.
  #
  # Output: object list, including AIC = deviance + 2*trace(Hat).
  #
  # Side effect: a plot of (x,y) and the estimated probability with twice se bands.
  #
  # Paul Eilers and Brian Marx, 2007 (c)
  #

  # Compute B-spline basis
	m = length(x)
	xl = min(x)
	xr = max(x)
	B = bbase(x, xl = xl, xr = xr, nseg = nseg, deg = bdeg)	

  # Construct penalty stuff
	n = dim(B)[2]
	P = sqrt(lambda) * diff(diag(n), diff = pord)
	nix = rep(0, n - pord)	# Initialize
	znew = rep(0, m)
	z = rep(1, m)
	it = 0	

  # Fit
	for (it in 1:20) {
		prob = 1/(1 + exp( - z))
		mu = prob
		w = prob * (1 - prob)
		u = (y - mu)/w + z
		f = lsfit(rbind(B, P), c(u, nix), wt = c(w, (nix + 1)), intercept = F)
		beta = f$coef
		znew = B %*% beta
		dz = max(abs(z - znew))
		if (show) print(c(it, dz))
		z = znew
		if(dz < 1e-005) break
	}
  
  # Compute AIC
	e = 1e-009
	dev = 2 * sum((y + e) * log((y + e)/mu) + (1 - y + e) * log((1 - y + e)/(1 - mu
		)))
	h = hat(f$qr)[1:m]
	aic = dev + 2 * sum(h)	
  
  # Compute curve on grid
	xgrid = seq(xl, xr, length = 100)
	Bu = bbase(xgrid, xl = xl, xr = xr, nseg = nseg, deg = bdeg)
	zu = Bu %*% beta
	ygrid = 1 / (1 + exp(-zu))	# Plot data and fit
	
	# Plot data and fit
	if(plot) {
		plot(x, y, xlab = '', ylab = '')
		lines(xgrid, ygrid, col = 'blue')
	}
	
	# Standard error bands (sandwich estimator)
	if (se > 0 & plot) {
		phi = t(B) %*% (as.vector(w) * B)
		lDD = t(P) %*% P
		bread = solve(phi + lDD)
		C = diag(Bu %*% bread %*% phi %*% bread %*% t(Bu))
		pivot = 2 * sqrt(C)
		U = exp(zu + pivot)
		U = U/(1 + U)
		L = exp(zu - pivot)
		L = L/(1 + L)
		lines(xgrid, U, lty = 2, col = 'red')
		lines(xgrid, L, lty = 2, col = 'red')
	}

  # Return list
	pp = list(aic = aic, x = x, y = y, muhat = mu, nseg = nseg, 
            bdeg = bdeg, pord = pord, lambda = lambda, 
            xgrid = xgrid, ygrid = ygrid, 
           	effdim = sum(h), ed.resid = m - sum(h), dispersion = 1, 
            family = "binomial", link = "logit", coef = beta)
	class(pp) = "pspfit"
	return(pp)
}


