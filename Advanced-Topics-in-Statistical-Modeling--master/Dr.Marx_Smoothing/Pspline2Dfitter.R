"psp2dG"<-
function(Data, Pars, ridge.adj = 0, XYpred = NULL, z.predicted = NULL, x.lab = 
	"X", y.lab = "Y", z.lab = "Z", persp.plot = T, image.plot = F, se = T, 
	family = "gaussian", link = "default", m.binomial = NULL, wts = NULL, 
	r.gamma = NULL)
{
# P-spline regression with 2-D regressors
# Input:
#   Data: 3 columns, giving x, y, z
#   Pars: 2 rows with P-spline parameters: [min max nseg deg lambda pdeg]
#   XYpred: 2 columns, giving x and y at points to predict
#   z.predicted: response vector associated with XYpred to compute CV
#   ridge.adj: small ridge penalty to stabilize estimation, can be zero
#   x.lab, y.lab, z.lab: "character" labels for estimated surface
#   persp.plot or image.plot: T or F for perspective or image plot of fitted surface
#   se: T or F for twice standard error band surfaces
#   family: "gaussian", "binomial", "poisson", "Gamma"
#   link: "logit", "probit", "log", "sqrt", "loglog", "cloglog", "identity", "inverse"
#   wts: non-negative weights (can be zero)
#   m.binomial: number of trials associated with binomial r.v. (can vary)
#   r.gamma: gamma scale parameter
#
#  Output: list with elements
#   coef: tensor product P-spline coefficients
#   summary.predicted: predicted value at new 2-D regressor locations (with 2 se bands)
#   cv: cross-validation statistic
#   aic, dev, df.residual
#   eff.dim: effective df of estimated 2-D estimated coefficient surface
#   perspective plot of fit
#
#  Support functions needed: bsplbase(), pspline2d.checker(), pspline.fitter(), inverse.link()
#
# Paul Eilers (2000) and Brian Marx (2004) (c)
# Prepare bases for estimation
	z <- Data[, 3]
	x <- Data[, 1]
	y <- Data[, 2]
	m <- length(z)
	if(missing(wts)) {
		wts <- rep(1, m)
	}
	if(missing(m.binomial)) {
		m.binomial <- rep(1, m)
	}
	if(missing(r.gamma)) {
		r.gamma <- rep(1, m)
	}
	parms <- pspline2d.checker(family, link, Pars[1, 4], Pars[2, 4], Pars[1,
		6], Pars[2, 6], Pars[1, 3], Pars[2, 3], Pars[1, 5], Pars[2, 5], 
		ridge.adj, wts)
	family <- parms$family
	link <- parms$link
	ridge.adj <- parms$ridge.adj
	wts <- parms$wts
	Pars[1, 3:6] <- c(parms$ps.intervals1, parms$degree1, parms$lambda1, 
		parms$order1)
	Pars[2, 3:6] <- c(parms$ps.intervals2, parms$degree2, parms$lambda2, 
		parms$order2)
	B1 <- bsplbase(as.vector(x), Pars[1,  ])
	B2 <- bsplbase(as.vector(y), Pars[2,  ])
	n1 <- ncol(B1)
	n2 <- ncol(B2)	# Compute tensor products for estimated alpha surface
	B1. <- kronecker(B1, t(rep(1, n2)))
	B2. <- kronecker(t(rep(1, n1)), B2)
	Q <- B1. * B2.	# 
#Construct penalty matrices
	d1 <- Pars[1, 6]
	D1 <- diag(n1)
	if(d1 != 0) {
		for(j in 1:d1) {
			D1 <- diff(D1)
		}
	}
	lambda1 <- Pars[1, 5]
	P1 <- sqrt(lambda1) * kronecker(D1, diag(n2))
	d2 <- Pars[2, 6]
	D2 <- diag(n2)
	if(d2 != 0) {
		for(j in 1:d2) {
			D2 <- diff(D2)
		}
	}
	lambda2 <- Pars[2, 5]
	P2 <- sqrt(lambda2) * kronecker(diag(n1), D2)
	Pen <- rbind(P1, P2)
	p.ridge <- NULL
	if(ridge.adj > 0) {
		nix.ridge <- rep(0, n1 * n2)
		p.ridge <- sqrt(ridge.adj) * diag(n1 * n2)
	}
# Data augmentation and regression
	z1 <- rep(0, n2 * (n1 - d1))
	z2 <- rep(0, n1 * (n2 - d2))
	n.col <- ncol(Q)
	ps.fit <- pspline.fitter(family, link, n.col, m.binomial, r.gamma, z, b
		 = Q, Pen, p.ridge, nix = c(z1, z2), nix.ridge = rep(0, n1 * n2
		), ridge.adj, wts)
	mu <- ps.fit$mu
	pcoef <- ps.fit$coef
	bin.percent.correct <- bin.0percent <- bin.1percent <- NULL
	if(family == "binomial") {
		count1 <- count2 <- pcount1 <- pcount2 <- 0
		p.hat <- mu/m.binomial
		for(ii in 1:m) {
			if(p.hat[ii] > 0.5) {
				count1 <- y[ii]
				count1 <- pcount1 + count1
				pcount1 <- count1
			}
			if(p.hat[ii] <= 0.5) {
				count2 <- m.binomial[ii] - y[ii]
				count2 <- pcount2 + count2
				pcount2 <- count2
			}
		}
		bin.percent.correct <- (count1 + count2)/sum(m.binomial)
		bin.1percent <- count1/sum(m.binomial[p.hat > 0.5])
		bin.0percent <- count2/sum(m.binomial[p.hat <= 0.5])
	}
	w <- ps.fit$w
	e <- 1e-009
	h <- hat(ps.fit$f$qr, intercept = F)[1:m]
	trace <- eff.dim <- sum(h)
	if(family == "binomial") {
		dev <- 2 * sum((z + e) * log((z + e)/mu) + (m.binomial - z + e) *
			log((m.binomial - z + e)/(m.binomial - mu)))
		dispersion.parm <- 1
		cv <- NULL
	}
	if(family == "poisson") {
		dev <- 2 * sum(z * log(z + e) - z - z * log(mu) + mu)
		dispersion.parm <- 1
		cv <- NULL
	}
	if(family == "Gamma") {
		dev <- -2 * sum(r.gamma * (log((z + e)/mu) - ((z - mu)/mu)))
		ave.dev <- dev/m
		dispersion.parm <- (ave.dev * (6 + ave.dev))/(6 + 2 * ave.dev)
		cv <- NULL
	}
	cv <- press.mu <- press.e <- var.c <- NULL
	if(family == "gaussian") {
		dev <- sum(ps.fit$f$residuals[1:m]^2)
		dispersion.parm <- dev/(m - trace)
		press.e <- ps.fit$f$residuals[1:m]/(1 - h)
		cv <- sqrt(sum((press.e)^2)/(m))
		press.mu <- z - press.e
	}
	aic <- dev + 2 * trace
	w.aug <- c(w, (c(z1, z2) + 1))	#if(int) {
#Compute grid to plot predicted values
	nu <- 20
	nv <- 25
	u <- seq(min(x), max(x), length = nu)
	v <- seq(min(y), max(y), length = nv)
	U. <- outer(rep(1, nv), u)
	V. <- outer(v, rep(1, nu))
	U <- as.vector(U.)
	V <- as.vector(V.)
	Bxgrid <- bsplbase(U, Pars[1,  ])
	Bygrid <- bsplbase(V, Pars[2,  ])
	B1grid <- kronecker(Bxgrid, t(rep(1, n2)))
	B2grid <- kronecker(t(rep(1, n1)), Bygrid)
	Bgrid <- B1grid * B2grid
	zgrid <- Bgrid %*% pcoef
	izgrid <- inverse.link(x = zgrid, link = link)
	Fitgrid <- matrix(izgrid, nu, nv, byrow = T)
	if(persp.plot) {
		pp <- persp(U.[1,  ], V.[, 1], Fitgrid, xlab = x.lab, ylab = 
			y.lab, zlab = z.lab , phi=30,theta=30, col='lightblue', expand=.5)
		if(missing(m.binomial)) {
			points(perspp(x, y, z, pp))
		}
		if(image.plot) {
			image(U.[1,  ], V.[, 1], Fitgrid, xlab = x.lab, ylab = 
				y.lab, sub = "fitted")
		}
		if(se) {
			half.meat <- sqrt(c(w)) * Q
			meat <- t(half.meat) %*% half.meat
			if(ridge.adj > 0) {
				bread <- solve(meat + t(Pen) %*% Pen + t(
				  p.ridge) %*% p.ridge)
			}
			if(ridge.adj == 0) {
				bread <- solve(meat + t(Pen) %*% Pen)
			}
			half.sw <- half.meat %*% bread[, 1:n.col]
			var.c <- t(half.sw) %*% half.sw
			half.lunch <- half.sw %*% t(Bgrid)
			ones <- 0 * y + 1
			var.hat <- ones %*% (half.lunch * half.lunch)
			stdev.hat <- sqrt(dispersion.parm) * t(sqrt(var.hat))
			pivot <- 2 * stdev.hat
			upper <- zgrid + pivot
			lower <- zgrid - pivot
			ilower <- inverse.link(x = lower, link = link)
			iupper <- inverse.link(x = upper, link = link)
			if(link == "recipical") {
				ilowup <- cbind(ilower, iupper)
				iupper <- ilowup[, 1]
				ilower <- ilowup[, 2]
			}
			L.hatm <- matrix(ilower, nu, nv, byrow = T)
			U.hatm <- matrix(iupper, nu, nv, byrow = T)
			persp(U.[1,  ], V.[, 1], U.hatm, xlab = x.lab, ylab = 
				y.lab, zlab = "2 se Upper Surface")
			if(image.plot) {
				image(U.[1,  ], V.[, 1], U.hatm, xlab = x.lab, 
				  ylab = y.lab, sub = "2 se Upper Surface")
			}
			persp(U.[1,  ], V.[, 1], L.hatm, xlab = x.lab, ylab = 
				y.lab, zlab = "2 se Lower Surface")
			if(image.plot) {
				image(U.[1,  ], V.[, 1], L.hatm, xlab = x.lab, 
				  ylab = y.lab, sub = "2 se Lower Surface")
			}
		}
	}
	summary.predicted <- NULL
	cv.predicted <- eta.predicted <- avediff.pred <- NULL
	if(!missing(XYpred)) {
#Compute XY new predictions
		Bxp <- bsplbase(XYpred[, 1], Pars[1,  ])
		Byp <- bsplbase(XYpred[, 2], Pars[2,  ])
		B1p <- kronecker(Bxp, t(rep(1, n2)))
		B2p <- kronecker(t(rep(1, n1)), Byp)
		Bp <- B1p * B2p
		zXYpred <- Bp %*% pcoef
		half.meat <- sqrt(c(w)) * Q
		meat <- t(half.meat) %*% half.meat
		if(ridge.adj > 0) {
			bread <- solve(meat + t(Pen) %*% Pen + t(p.ridge) %*% 
				p.ridge)
		}
		if(ridge.adj == 0) {
			bread <- solve(meat + t(Pen) %*% Pen)
		}
		half.sw <- half.meat %*% bread[, 1:n.col]
		var.c <- t(half.sw) %*% half.sw
		eta.predicted <- zXYpred
		var.pred <- Bp %*% var.c %*% t(Bp)
		stdev.pred <- as.vector(sqrt(diag(var.pred)))
		stdev.pred <- sqrt(dispersion.parm) * stdev.pred
		pivot <- as.vector(2 * stdev.pred)
		upper <- eta.predicted + pivot
		lower <- eta.predicted - pivot
		ieta.predicted <- inverse.link(eta.predicted, link)
		ilower <- inverse.link(lower, link)
		iupper <- inverse.link(upper, link)
		if(link == "recipical") {
			ilowup <- cbind(ilower, iupper)
			iupper <- ilowup[, 1]
			ilower <- ilowup[, 2]
		}
		summary.predicted <- as.matrix(cbind(ilower, ieta.predicted, 
			iupper))
		if(!missing(z.predicted)) {
			if(family == "gaussian") {
				cv.predicted <- sqrt(sum((z.predicted - 
				  eta.predicted)^2)/(length(z.predicted)))
				avediff.pred <- (sum(z.predicted - 
				  eta.predicted))/length(z.predicted)
			}
		}
		bin.percent.correct <- bin.0percent <- bin.1percent <- NULL
		if(link == "logit") {
			count1 <- count2 <- pcount1 <- pcount2 <- 0
			p.hat <- exp(eta.predicted)/(1 + exp(eta.predicted))
			if(!missing(z.predicted)) {
				z.predicted <- as.vector(z.predicted)
				for(ii in 1:length(eta.predicted)) {
				  if(p.hat[ii] > 0.5) {
				    count1 <- z.predicted[ii]
				    count1 <- pcount1 + count1
				    pcount1 <- count1
				  }
				  if(p.hat[ii] <= 0.5) {
				    count2 <- 1 - z.predicted[ii]
				    count2 <- pcount2 + count2
				    pcount2 <- count2
				  }
				}
				bin.percent.correct <- (count1 + count2)/length(
				  z.predicted)
				bin.1percent <- count1/sum(z.predicted)
				bin.0percent <- count2/(length(z.predicted) - 
				  sum(z.predicted))
			}
		}
		dimnames(summary.predicted) <- list(NULL, c("-2std_Lower", 
			"Predicted", "+2std_Upper"))
	}
	P <- list(coef = pcoef, Pars = Pars, family = family, link = link, dev
		 = dev, aic = aic, bin.percent.correct = bin.percent.correct, 
		bin.0 = bin.0percent, bin.1 = bin.1percent, df.resid = m - 
		trace, dispersion.parm = dispersion.parm, mu = mu, press.mu = 
		press.mu, summary.predicted = summary.predicted, eta.predicted
		 = eta.predicted, avediff.pred = avediff.pred, ridge.adj = 
		ridge.adj, cv = cv, cv.predicted = cv.predicted, eff.dim = 
		eff.dim)
	P
}
"bsplbase"<-
function(x, bpars)
{
# Compute a B-spline basis
# Input:
#   x: abcissae
#   bpars: B-spline parameters: xmin, xmax, nseg, degree (= one less than "order")
# Output:
#   base: matrix with nrow = length(x) and nseg + degree columns
#
# Paul Eilers, 2000
	dx <- (bpars[2] - bpars[1])/bpars[3]
	knots <- seq(bpars[1] - bpars[4] * dx, bpars[2] + bpars[4] * dx, by = 
		dx)
	base <- as.matrix(spline.des(knots, x, bpars[4] + 1, 0 * x)$design)
	base
}
"pspline2d.checker"<-
function(family, link, degree1, degree2, order1, order2, ps.intervals1, 
	ps.intervals2, lambda1, lambda2, ridge.adj, wts)
{
	if(link == "default" && family == "gaussian") {
		link <- "identity"
	}
	if(link == "default" && family == "poisson") {
		link <- "log"
	}
	if(link == "default" && family == "binomial") {
		link <- "logit"
	}
	if(link == "default" && family == "Gamma") {

		link <- "log"
	}
	if(family != "binomial" && family != "gaussian" && family != "poisson" && 
		family != "Gamma") {
		warning(paste("Improper FAMILY option. Choose: gaussian, poisson, binomial or Gamma"
			))
	}
	if((family == "binomial") && (link != "logit" && link != "probit" && 
		link != "cloglog" && link != "loglog")) {
		warning(paste("Improper LINK option with family=binomial. Choose: logit, probit, loglog, cloglog"
			))
	}
	if((family == "Gamma") && (link != "log" && link != "recipical" && link !=
		"identity")) {
		warning(paste("Improper LINK option with family=Gamma. Choose: recipical, log, identity"
			))
	}
	if((family == "poisson") && (link != "log" && link != "sqrt" && link != 
		"identity")) {
		warning(paste("Improper LINK option with family=poisson. Choose: log, sqrt, identity"
			))
	}
	if((family == "gaussian") && (link != "identity")) {
		warning(paste("Improper LINK option with family=gaussian. Choose: identity"
			))
	}
	if(degree1 < 0) {
		degree1 <- 1
		warning(paste("degree1 must be non-neg integer: have used 1"))
	}
	if(order1 < 0) {
		order1 <- 0
		warning(paste("order1 must be non-neg integer: have used 0"))
	}
	if(ps.intervals1 < 2) {
		ps.intervals1 <- 2
		warning(paste("ps.intervals1 must be positive integer, > 1: have used 2"
			))
	}
	if(lambda1 < 0) {
		lambda1 <- 0
		warning(paste("lambda1 cannot be negative: have used 0"))
	}
	if(degree2 < 0) {
		degree2 <- 1
		warning(paste("degree2 must be non-neg integer: have used 1"))
	}
	if(order2 < 0) {
		order2 <- 0
		warning(paste("order2 must be non-neg integer: have used 0"))
	}
	if(ps.intervals2 < 2) {
		ps.intervals2 <- 2
		warning(paste("ps.intervals2 must be positive integer, > 1: have used 2"
			))
	}
	if(lambda2 < 0) {
		lambda2 <- 0
		warning(paste("lambda2 cannot be negative: have used 0"))
	}
	if(ridge.adj < 0) {
		ridge.adj <- 0
		warning(paste("ridge.adj cannot be negative: have used 0"))
	}
	if(min(wts) < 0) {
		warning(paste("At least one weight entry is negative"))
	}
	llist <- list(family = family, link = link, degree1 = degree1, order1
		 = order1, ps.intervals1 = ps.intervals1, lambda1 = lambda1, 
		degree2 = degree2, order2 = order2, ps.intervals2 = 
		ps.intervals2, lambda2 = lambda2, ridge.adj = ridge.adj, wts = 
		wts)
	return(llist)
}
"pspline.fitter"<-
function(family, link, n.col, m.binomial, r.gamma, y, b, p, p.ridge, nix, 
	nix.ridge, ridge.adj, wts, ...)
{
	coef.est <- rep(1, ncol(b))
	if(family == "binomial") {
		mu <- (y + 0.5 * m.binomial)/2
	}
	if(family == "Gamma" || family == "poisson") {
		mu <- (y + 3)
	}
	if(family == "gaussian") {
		mu <- rep(mean(y), length(y))
	}
	it <- 0
	repeat {
		if(it == 0) {
			if(link == "identity") {
				eta <- mu
			}
			if(link == "log") {
				eta <- log(mu)
			}
			if(link == "sqrt") {
				eta <- sqrt(mu)
			}
			if(link == "logit") {
				eta <- log(mu/(m.binomial - mu))
			}
			if(link == "recipical") {
				eta <- 1/mu
			}
			if(link == "probit") {
				eta <- qnorm(mu/m.binomial)
			}
			if(link == "cloglog") {
				eta <- log( - log(1 - mu/m.binomial))
			}
			if(link == "loglog") {
				eta <-  - log( - log(mu/m.binomial))
			}
		}
		it <- it + 1
		if(it > 25)
			break
		if(link == "identity") {
			mu <- eta
			h.prime <- 1
		}
		if(link == "log") {
			mu <- exp(eta)
			h.prime <- mu
		}
		if(link == "sqrt") {
			mu <- eta^2
			h.prime <- 2 * eta
		}
		if(link == "logit") {
			mu <- m.binomial/(1 + exp( - eta))
			h.prime <- mu * (1 - mu/m.binomial)
		}
		if(link == "recipical") {
			mu <- 1/eta
			h.prime <-  - (mu^2)
		}
		if(link == "probit") {
			mu <- m.binomial * pnorm(eta)
			h.prime <- m.binomial * dnorm(eta)
		}
		if(link == "cloglog") {
			mu <- m.binomial * (1 - exp( - exp(eta)))
			h.prime <- (m.binomial) * exp(eta) * exp( - exp(eta))
		}
		if(link == "loglog") {
			mu <- m.binomial * exp( - exp( - eta))
			h.prime <- m.binomial * exp( - eta) * exp( - exp( - eta
				))
		}
		if(family == "gaussian") {
			w <- rep(1, length(y))
		}
		if(family == "poisson") {
			w <- h.prime^2/mu
		}
		if(family == "binomial") {
			w <- h.prime^2/(mu * (1 - mu/m.binomial))
		}
		if(family == "Gamma") {
			w <- (r.gamma * h.prime^2)/mu^2
		}
		u <- (y - mu)/h.prime + eta
		if(ridge.adj > 0) {
			f <- lsfit(rbind(b, p, p.ridge), c(u, nix, nix.ridge), 
				wt = c(wts, nix + 1, nix.ridge + 1) * c(w, (nix +
				1), (nix.ridge + 1)), intercept = F)
		}
		if(ridge.adj == 0) {
			f <- lsfit(rbind(b, p), c(u, nix), wt = c(wts, nix + 1) *
				c(w, (nix + 1)), intercept = F)
		}
		coef.old <- coef.est
		coef.est <- as.vector(f$coef)
		d.coef <- max(abs((coef.est - coef.old)/coef.old))
		if(d.coef < 1e-008)
			break
		print(c(it, d.coef))
		eta <- b %*% coef.est
	}
	if(it > 24) {
		warning(paste("parameter estimates did NOT converge in 25 iterations"
			))
	}
	llist <- list(coef = coef.est, mu = mu, f = f, w = w * wts)
	return(llist)
}
"inverse.link"<-
function(x, link)
{
	if(link == "identity") {
		invx <- x
	}
	if(link == "logit") {
		invx <- 1/(1 + exp( - x))
	}
	if(link == "probit") {
		invx <- apply(x, c(1, 2), pnorm)
	}
	if(link == "cloglog") {
		invx <- (1 - exp( - exp(x)))
	}
	if(link == "loglog") {
		invx <- exp( - exp( - x))
	}
	if(link == "sqrt") {
		invx <- x^2
	}
	if(link == "log") {
		invx <- exp(x)
	}
	if(link == "recipical") {
		invx <- 1/(x)
	}
	return(invx)
}
